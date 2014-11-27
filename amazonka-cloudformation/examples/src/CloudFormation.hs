{-# LANGUAGE OverloadedStrings #-}

-- Module      : CloudFormation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module CloudFormation where

import           Control.Lens
import           Control.Monad.Trans.AWS
import qualified Data.Text.IO               as Text
import           Network.AWS.CloudFormation
import           System.IO

stackFromFile :: IO (Either Error CreateStackResponse)
stackFromFile = do
    lgr  <- newLogger Debug stdout
    env  <- getEnv Ireland Discover <&> envLogger .~ lgr
    tmpl <- Text.readFile "load-balanced-apache.json"
    runAWST env . send $ createStack "amazonka-stack-from-file"
        & csTemplateBody ?~ tmpl
        & csParameters   .~ [ parameter & pParameterKey   ?~ "KeyName"
                                        & pParameterValue ?~ "default-keypair"
                            ]

stackFromUrl :: IO (Either Error CreateStackResponse)
stackFromUrl = do
    lgr <- newLogger Debug stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    let url = "https://s3-us-west-2.amazonaws.com/cloudformation-templates-us-west-2/EC2InstanceWithSecurityGroupSample.template"
    runAWST env . send $ createStack "amazonka-stack-from-url"
        & csTemplateURL ?~ url
        & csParameters  .~ [ parameter & pParameterKey   ?~ "KeyName"
                                       & pParameterValue ?~ "default-keypair"
                           ]
