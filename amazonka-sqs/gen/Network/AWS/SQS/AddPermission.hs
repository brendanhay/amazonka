{-# LANGUAGE DeriveGeneric               #-}
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
-- AddPermission writes an Amazon SQS-generated policy. If you want to write
-- your own policy, use SetQueueAttributes to upload your policy. For more
-- information about writing your own policy, see Using The Access Policy
-- Language in the Amazon SQS Developer Guide. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that The
-- following example Query request grants a SendMessage permission to the
-- principal whose AWS account number is 125074342641.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=AddPermission &Label=testLabel &AWSAccountId.1=125074342641
-- &ActionName.1=SendMessage &AWSAccountId.2=125074342642
-- &ActionName.2=ReceiveMessage &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 9a285199-c8d6-47c2-bdb2-314cb47d599d.
module Network.AWS.SQS.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , mkAddPermission
    -- ** Request lenses
    , apQueueUrl
    , apLabel
    , apAWSAccountIds
    , apActions

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , mkAddPermissionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data AddPermission = AddPermission
    { _apQueueUrl :: Text
    , _apLabel :: Text
    , _apAWSAccountIds :: [Text]
    , _apActions :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddPermission' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @Label ::@ @Text@
--
-- * @AWSAccountIds ::@ @[Text]@
--
-- * @Actions ::@ @[Text]@
--
mkAddPermission :: Text -- ^ 'apQueueUrl'
                -> Text -- ^ 'apLabel'
                -> [Text] -- ^ 'apAWSAccountIds'
                -> [Text] -- ^ 'apActions'
                -> AddPermission
mkAddPermission p1 p2 p3 p4 = AddPermission
    { _apQueueUrl = p1
    , _apLabel = p2
    , _apAWSAccountIds = p3
    , _apActions = p4
    }

-- | The URL of the Amazon SQS queue to take action on.
apQueueUrl :: Lens' AddPermission Text
apQueueUrl = lens _apQueueUrl (\s a -> s { _apQueueUrl = a })

-- | The unique identification of the permission you're setting (e.g.,
-- AliceSendMessage). Constraints: Maximum 80 characters; alphanumeric
-- characters, hyphens (-), and underscores (_) are allowed.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\s a -> s { _apLabel = a })

-- | The AWS account number of the principal who will be given permission. The
-- principal must have an AWS account, but does not need to be signed up for
-- Amazon SQS. For information about locating the AWS account identification,
-- see Your AWS Identifiers in the Amazon SQS Developer Guide.
apAWSAccountIds :: Lens' AddPermission [Text]
apAWSAccountIds = lens _apAWSAccountIds (\s a -> s { _apAWSAccountIds = a })

-- | The action the client wants to allow for the specified principal. The
-- following are valid values: * | SendMessage | ReceiveMessage |
-- DeleteMessage | ChangeMessageVisibility | GetQueueAttributes | GetQueueUrl.
-- For more information about these actions, see Understanding Permissions in
-- the Amazon SQS Developer Guide. Specifying SendMessage, DeleteMessage, or
-- ChangeMessageVisibility for the ActionName.n also grants permissions for
-- the corresponding batch versions of those actions: SendMessageBatch,
-- DeleteMessageBatch, and ChangeMessageVisibilityBatch.
apActions :: Lens' AddPermission [Text]
apActions = lens _apActions (\s a -> s { _apActions = a })

instance ToQuery AddPermission where
    toQuery = genericQuery def

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddPermissionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAddPermissionResponse :: AddPermissionResponse
mkAddPermissionResponse = AddPermissionResponse

instance AWSRequest AddPermission where
    type Sv AddPermission = SQS
    type Rs AddPermission = AddPermissionResponse

    request = post "AddPermission"
    response _ = nullaryResponse AddPermissionResponse
