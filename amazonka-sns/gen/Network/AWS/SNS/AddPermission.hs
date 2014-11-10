{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SNS.AddPermission
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
module Network.AWS.SNS.AddPermission
    (
    -- * Request
      AddPermissionInput
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apiAWSAccountId
    , apiActionName
    , apiLabel
    , apiTopicArn

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data AddPermissionInput = AddPermissionInput
    { _apiAWSAccountId :: [Text]
    , _apiActionName   :: [Text]
    , _apiLabel        :: Text
    , _apiTopicArn     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddPermissionInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apiAWSAccountId' @::@ ['Text']
--
-- * 'apiActionName' @::@ ['Text']
--
-- * 'apiLabel' @::@ 'Text'
--
-- * 'apiTopicArn' @::@ 'Text'
--
addPermission :: Text -- ^ 'apiTopicArn'
              -> Text -- ^ 'apiLabel'
              -> AddPermissionInput
addPermission p1 p2 = AddPermissionInput
    { _apiTopicArn     = p1
    , _apiLabel        = p2
    , _apiAWSAccountId = mempty
    , _apiActionName   = mempty
    }

-- | The AWS account IDs of the users (principals) who will be given access to
-- the specified actions. The users must have AWS accounts, but do not need
-- to be signed up for this service.
apiAWSAccountId :: Lens' AddPermissionInput [Text]
apiAWSAccountId = lens _apiAWSAccountId (\s a -> s { _apiAWSAccountId = a })

-- | The action you want to allow for the specified principal(s). Valid
-- values: any Amazon SNS action name.
apiActionName :: Lens' AddPermissionInput [Text]
apiActionName = lens _apiActionName (\s a -> s { _apiActionName = a })

-- | A unique identifier for the new policy statement.
apiLabel :: Lens' AddPermissionInput Text
apiLabel = lens _apiLabel (\s a -> s { _apiLabel = a })

-- | The ARN of the topic whose access control policy you wish to modify.
apiTopicArn :: Lens' AddPermissionInput Text
apiTopicArn = lens _apiTopicArn (\s a -> s { _apiTopicArn = a })

instance ToPath AddPermissionInput where
    toPath = const "/"

instance ToQuery AddPermissionInput

data AddPermissionResponse = AddPermissionResponse

-- | 'AddPermissionResponse' constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse

instance AWSRequest AddPermissionInput where
    type Sv AddPermissionInput = SNS
    type Rs AddPermissionInput = AddPermissionResponse

    request  = post "AddPermission"
    response = const (nullaryResponse AddPermissionResponse)
