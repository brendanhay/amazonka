{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a permission to a queue for a specific principal. This allows for
-- sharing access to the queue. When you create a queue, you have full control
-- access rights for the queue. Only you (as owner of the queue) can grant or
-- deny permissions to the queue. For more information about these
-- permissions, see Shared Queues in the Amazon SQS Developer Guide.
-- &amp;Attribute.1=this &amp;Attribute.2=that.
--
-- <AddPermission.html>
module Network.AWS.SQS.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apAWSAccountIds
    , apActions
    , apLabel
    , apQueueUrl

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data AddPermission = AddPermission
    { _apAWSAccountIds :: [Text]
    , _apActions       :: [Text]
    , _apLabel         :: Text
    , _apQueueUrl      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apAWSAccountIds' @::@ ['Text']
--
-- * 'apActions' @::@ ['Text']
--
-- * 'apLabel' @::@ 'Text'
--
-- * 'apQueueUrl' @::@ 'Text'
--
addPermission :: Text -- ^ 'apQueueUrl'
              -> Text -- ^ 'apLabel'
              -> AddPermission
addPermission p1 p2 = AddPermission
    { _apQueueUrl      = p1
    , _apLabel         = p2
    , _apAWSAccountIds = mempty
    , _apActions       = mempty
    }

-- | The AWS account number of the principal who will be given permission. The
-- principal must have an AWS account, but does not need to be signed up for
-- Amazon SQS. For information about locating the AWS account
-- identification, see Your AWS Identifiers in the Amazon SQS Developer
-- Guide.
apAWSAccountIds :: Lens' AddPermission [Text]
apAWSAccountIds = lens _apAWSAccountIds (\s a -> s { _apAWSAccountIds = a })

-- | The action the client wants to allow for the specified principal. The
-- following are valid values: * | SendMessage | ReceiveMessage |
-- DeleteMessage | ChangeMessageVisibility | GetQueueAttributes |
-- GetQueueUrl. For more information about these actions, see Understanding
-- Permissions in the Amazon SQS Developer Guide. Specifying SendMessage,
-- DeleteMessage, or ChangeMessageVisibility for the ActionName.n also
-- grants permissions for the corresponding batch versions of those actions:
-- SendMessageBatch, DeleteMessageBatch, and ChangeMessageVisibilityBatch.
apActions :: Lens' AddPermission [Text]
apActions = lens _apActions (\s a -> s { _apActions = a })

-- | The unique identification of the permission you're setting (e.g.,
-- AliceSendMessage). Constraints: Maximum 80 characters; alphanumeric
-- characters, hyphens (-), and underscores (_) are allowed.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\s a -> s { _apLabel = a })

-- | The URL of the Amazon SQS queue to take action on.
apQueueUrl :: Lens' AddPermission Text
apQueueUrl = lens _apQueueUrl (\s a -> s { _apQueueUrl = a })

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddPermissionResponse' constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse

instance AWSRequest AddPermission where
    type Sv AddPermission = SQS
    type Rs AddPermission = AddPermissionResponse

    request  = post "AddPermission"
    response = nullResponse AddPermissionResponse

instance ToPath AddPermission where
    toPath = const "/"

instance ToHeaders AddPermission

instance ToQuery AddPermission
