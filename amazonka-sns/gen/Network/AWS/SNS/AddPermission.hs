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
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apAWSAccountId
    , apActionName
    , apLabel
    , apTopicArn

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data AddPermission = AddPermission
    { _apAWSAccountId :: [Text]
    , _apActionName   :: [Text]
    , _apLabel        :: Text
    , _apTopicArn     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddPermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apAWSAccountId' @::@ ['Text']
--
-- * 'apActionName' @::@ ['Text']
--
-- * 'apLabel' @::@ 'Text'
--
-- * 'apTopicArn' @::@ 'Text'
--
addPermission :: Text -- ^ 'apTopicArn'
              -> Text -- ^ 'apLabel'
              -> AddPermission
addPermission p1 p2 = AddPermission
    { _apTopicArn     = p1
    , _apLabel        = p2
    , _apAWSAccountId = mempty
    , _apActionName   = mempty
    }

-- | The AWS account IDs of the users (principals) who will be given access to
-- the specified actions. The users must have AWS accounts, but do not need
-- to be signed up for this service.
apAWSAccountId :: Lens' AddPermission [Text]
apAWSAccountId = lens _apAWSAccountId (\s a -> s { _apAWSAccountId = a })

-- | The action you want to allow for the specified principal(s). Valid
-- values: any Amazon SNS action name.
apActionName :: Lens' AddPermission [Text]
apActionName = lens _apActionName (\s a -> s { _apActionName = a })

-- | A unique identifier for the new policy statement.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\s a -> s { _apLabel = a })

-- | The ARN of the topic whose access control policy you wish to modify.
apTopicArn :: Lens' AddPermission Text
apTopicArn = lens _apTopicArn (\s a -> s { _apTopicArn = a })

instance ToQuery AddPermission

instance ToPath AddPermission where
    toPath = const "/"

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AddPermissionResponse' constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse

instance AWSRequest AddPermission where
    type Sv AddPermission = SNS
    type Rs AddPermission = AddPermissionResponse

    request  = post "AddPermission"
    response = nullaryResponse AddPermissionResponse
