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

-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds a statement to a topic's access control policy, granting access for the
-- specified AWS accounts to the specified actions.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_AddPermission.html>
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
    { _apAWSAccountId :: List "member" Text
    , _apActionName   :: List "member" Text
    , _apLabel        :: Text
    , _apTopicArn     :: Text
    } deriving (Eq, Ord, Read, Show)

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

-- | The AWS account IDs of the users (principals) who will be given access to the
-- specified actions. The users must have AWS accounts, but do not need to be
-- signed up for this service.
apAWSAccountId :: Lens' AddPermission [Text]
apAWSAccountId = lens _apAWSAccountId (\s a -> s { _apAWSAccountId = a }) . _List

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: any Amazon SNS action name.
apActionName :: Lens' AddPermission [Text]
apActionName = lens _apActionName (\s a -> s { _apActionName = a }) . _List

-- | A unique identifier for the new policy statement.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\s a -> s { _apLabel = a })

-- | The ARN of the topic whose access control policy you wish to modify.
apTopicArn :: Lens' AddPermission Text
apTopicArn = lens _apTopicArn (\s a -> s { _apTopicArn = a })

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AddPermissionResponse' constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse

instance ToPath AddPermission where
    toPath = const "/"

instance ToQuery AddPermission where
    toQuery AddPermission{..} = mconcat
        [ "AWSAccountId" =? _apAWSAccountId
        , "ActionName"   =? _apActionName
        , "Label"        =? _apLabel
        , "TopicArn"     =? _apTopicArn
        ]

instance ToHeaders AddPermission

instance AWSRequest AddPermission where
    type Sv AddPermission = SNS
    type Rs AddPermission = AddPermissionResponse

    request  = post "AddPermission"
    response = nullResponse AddPermissionResponse
