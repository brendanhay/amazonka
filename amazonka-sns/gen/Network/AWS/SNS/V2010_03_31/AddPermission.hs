{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.AddPermission
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
module Network.AWS.SNS.V2010_03_31.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apiActionName
    , apiAWSAccountId
    , apiLabel
    , apiTopicArn

    -- * Response
    , AddPermissionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddPermission' request.
addPermission :: [Text] -- ^ 'apiActionName'
              -> [Text] -- ^ 'apiAWSAccountId'
              -> Text -- ^ 'apiLabel'
              -> Text -- ^ 'apiTopicArn'
              -> AddPermission
addPermission p1 p2 p3 p4 = AddPermission
    { _apiActionName = p1
    , _apiAWSAccountId = p2
    , _apiLabel = p3
    , _apiTopicArn = p4
    }
{-# INLINE addPermission #-}

data AddPermission = AddPermission
    { _apiActionName :: [Text]
      -- ^ The action you want to allow for the specified principal(s).
      -- Valid values: any Amazon SNS action name.
    , _apiAWSAccountId :: [Text]
      -- ^ The AWS account IDs of the users (principals) who will be given
      -- access to the specified actions. The users must have AWS
      -- accounts, but do not need to be signed up for this service.
    , _apiLabel :: Text
      -- ^ A unique identifier for the new policy statement.
    , _apiTopicArn :: Text
      -- ^ The ARN of the topic whose access control policy you wish to
      -- modify.
    } deriving (Show, Generic)

-- | The action you want to allow for the specified principal(s). Valid values:
-- any Amazon SNS action name.
apiActionName :: Lens' AddPermission ([Text])
apiActionName f x =
    f (_apiActionName x)
        <&> \y -> x { _apiActionName = y }
{-# INLINE apiActionName #-}

-- | The AWS account IDs of the users (principals) who will be given access to
-- the specified actions. The users must have AWS accounts, but do not need to
-- be signed up for this service.
apiAWSAccountId :: Lens' AddPermission ([Text])
apiAWSAccountId f x =
    f (_apiAWSAccountId x)
        <&> \y -> x { _apiAWSAccountId = y }
{-# INLINE apiAWSAccountId #-}

-- | A unique identifier for the new policy statement.
apiLabel :: Lens' AddPermission (Text)
apiLabel f x =
    f (_apiLabel x)
        <&> \y -> x { _apiLabel = y }
{-# INLINE apiLabel #-}

-- | The ARN of the topic whose access control policy you wish to modify.
apiTopicArn :: Lens' AddPermission (Text)
apiTopicArn f x =
    f (_apiTopicArn x)
        <&> \y -> x { _apiTopicArn = y }
{-# INLINE apiTopicArn #-}

instance ToQuery AddPermission where
    toQuery = genericQuery def

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddPermission where
    type Sv AddPermission = SNS
    type Rs AddPermission = AddPermissionResponse

    request = post "AddPermission"
    response _ = nullaryResponse AddPermissionResponse
