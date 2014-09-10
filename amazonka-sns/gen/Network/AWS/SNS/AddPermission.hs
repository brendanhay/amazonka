{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a statement to a topic's access control policy, granting access for
-- the specified AWS accounts to the specified actions.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &amp;ActionName.member.1=Publish
-- &amp;ActionName.member.2=GetTopicAttributes &amp;Label=NewPermission
-- &amp;AWSAccountId.member.1=987654321000
-- &amp;AWSAccountId.member.2=876543210000 &amp;Action=AddPermission
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=k%2FAU%2FKp13pjndwJ7rr1sZszy6MZMlOhRBCHx1ZaZFiw%3D
-- &lt;AddPermissionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6a213e4e-33a8-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/AddPermissionResponse&gt;.
module Network.AWS.SNS
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , mkAddPermission
    -- ** Request lenses
    , apTopicArn
    , apLabel
    , apAWSAccountId
    , apActionName

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , mkAddPermissionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

data AddPermission = AddPermission
    { _apTopicArn :: Text
    , _apLabel :: Text
    , _apAWSAccountId :: [Text]
    , _apActionName :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddPermission' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Text@
--
-- * @Label ::@ @Text@
--
-- * @AWSAccountId ::@ @[Text]@
--
-- * @ActionName ::@ @[Text]@
--
mkAddPermission :: Text -- ^ 'apTopicArn'
                -> Text -- ^ 'apLabel'
                -> [Text] -- ^ 'apAWSAccountId'
                -> [Text] -- ^ 'apActionName'
                -> AddPermission
mkAddPermission p1 p2 p3 p4 = AddPermission
    { _apTopicArn = p1
    , _apLabel = p2
    , _apAWSAccountId = p3
    , _apActionName = p4
    }

-- | The ARN of the topic whose access control policy you wish to modify.
apTopicArn :: Lens' AddPermission Text
apTopicArn = lens _apTopicArn (\s a -> s { _apTopicArn = a })

-- | A unique identifier for the new policy statement.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\s a -> s { _apLabel = a })

-- | The AWS account IDs of the users (principals) who will be given access to
-- the specified actions. The users must have AWS accounts, but do not need to
-- be signed up for this service.
apAWSAccountId :: Lens' AddPermission [Text]
apAWSAccountId = lens _apAWSAccountId (\s a -> s { _apAWSAccountId = a })

-- | The action you want to allow for the specified principal(s). Valid values:
-- any Amazon SNS action name.
apActionName :: Lens' AddPermission [Text]
apActionName = lens _apActionName (\s a -> s { _apActionName = a })

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
    type Sv AddPermission = SNS
    type Rs AddPermission = AddPermissionResponse

    request = post "AddPermission"
    response _ = nullaryResponse AddPermissionResponse
