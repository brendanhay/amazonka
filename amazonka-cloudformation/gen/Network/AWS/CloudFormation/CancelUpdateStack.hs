{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Cancels an update on the specified stack. If the call completes successfully,
-- the stack will roll back the update and revert to the previous stack
-- configuration.
--
-- Only stacks that are in the UPDATE_IN_PROGRESS state can be canceled.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html>
module Network.AWS.CloudFormation.CancelUpdateStack
    (
    -- * Request
      CancelUpdateStack
    -- ** Request constructor
    , cancelUpdateStack
    -- ** Request lenses
    , cusStackName

    -- * Response
    , CancelUpdateStackResponse
    -- ** Response constructor
    , cancelUpdateStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

newtype CancelUpdateStack = CancelUpdateStack
    { _cusStackName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'CancelUpdateStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cusStackName' @::@ 'Text'
--
cancelUpdateStack :: Text -- ^ 'cusStackName'
                  -> CancelUpdateStack
cancelUpdateStack p1 = CancelUpdateStack
    { _cusStackName = p1
    }

-- | The name or the unique identifier associated with the stack.
cusStackName :: Lens' CancelUpdateStack Text
cusStackName = lens _cusStackName (\s a -> s { _cusStackName = a })

data CancelUpdateStackResponse = CancelUpdateStackResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CancelUpdateStackResponse' constructor.
cancelUpdateStackResponse :: CancelUpdateStackResponse
cancelUpdateStackResponse = CancelUpdateStackResponse

instance ToPath CancelUpdateStack where
    toPath = const "/"

instance ToQuery CancelUpdateStack where
    toQuery CancelUpdateStack{..} = mconcat
        [ "StackName" =? _cusStackName
        ]

instance ToHeaders CancelUpdateStack

instance AWSRequest CancelUpdateStack where
    type Sv CancelUpdateStack = CloudFormation
    type Rs CancelUpdateStack = CancelUpdateStackResponse

    request  = post "CancelUpdateStack"
    response = nullResponse CancelUpdateStackResponse
