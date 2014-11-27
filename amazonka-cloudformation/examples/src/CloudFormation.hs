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

example :: IO (Either Error CreateStackResponse)
example = do
    lgr  <- newLogger Debug stdout
    env  <- getEnv Oregon Discover <&> envLogger .~ lgr
    tmpl <- Text.readFile "load-balanced-apache.json"
    runAWST env . send $ createStack "amazonka-test-stack"
        & csTemplateBody ?~ tmpl
        & csParameters   .~ [ parameter & pParameterKey   ?~ "KeyName"
                                        & pParameterValue ?~ "default-keypair"
                            ]
