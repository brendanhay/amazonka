{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.CancelUpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration. Only stacks that are in the
-- UPDATE_IN_PROGRESS state can be canceled.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CancelUpdateStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
module Network.AWS.CloudFormation.V2010_05_15.CancelUpdateStack where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

data CancelUpdateStack = CancelUpdateStack
    { _cusiStackName :: Text
      -- ^ The name or the unique identifier associated with the stack.
    } deriving (Show, Generic)

makeLenses ''CancelUpdateStack

instance ToQuery CancelUpdateStack where
    toQuery = genericQuery def

data CancelUpdateStackResponse = CancelUpdateStackResponse
    deriving (Eq, Show, Generic)

makeLenses ''CancelUpdateStackResponse

instance AWSRequest CancelUpdateStack where
    type Sv CancelUpdateStack = CloudFormation
    type Rs CancelUpdateStack = CancelUpdateStackResponse

    request = post "CancelUpdateStack"
    response _ = nullaryResponse CancelUpdateStackResponse
