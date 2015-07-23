{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic\'s access control policy, granting access
-- for the specified AWS accounts to the specified actions.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_AddPermission.html>
module Network.AWS.SNS.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , aprqTopicARN
    , aprqLabel
    , aprqAWSAccountId
    , aprqActionName

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | /See:/ 'addPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aprqTopicARN'
--
-- * 'aprqLabel'
--
-- * 'aprqAWSAccountId'
--
-- * 'aprqActionName'
data AddPermission = AddPermission'
    { _aprqTopicARN     :: !Text
    , _aprqLabel        :: !Text
    , _aprqAWSAccountId :: ![Text]
    , _aprqActionName   :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddPermission' smart constructor.
addPermission :: Text -> Text -> AddPermission
addPermission pTopicARN_ pLabel_ =
    AddPermission'
    { _aprqTopicARN = pTopicARN_
    , _aprqLabel = pLabel_
    , _aprqAWSAccountId = mempty
    , _aprqActionName = mempty
    }

-- | The ARN of the topic whose access control policy you wish to modify.
aprqTopicARN :: Lens' AddPermission Text
aprqTopicARN = lens _aprqTopicARN (\ s a -> s{_aprqTopicARN = a});

-- | A unique identifier for the new policy statement.
aprqLabel :: Lens' AddPermission Text
aprqLabel = lens _aprqLabel (\ s a -> s{_aprqLabel = a});

-- | The AWS account IDs of the users (principals) who will be given access
-- to the specified actions. The users must have AWS accounts, but do not
-- need to be signed up for this service.
aprqAWSAccountId :: Lens' AddPermission [Text]
aprqAWSAccountId = lens _aprqAWSAccountId (\ s a -> s{_aprqAWSAccountId = a});

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: any Amazon SNS action name.
aprqActionName :: Lens' AddPermission [Text]
aprqActionName = lens _aprqActionName (\ s a -> s{_aprqActionName = a});

instance AWSRequest AddPermission where
        type Sv AddPermission = SNS
        type Rs AddPermission = AddPermissionResponse
        request = post
        response = receiveNull AddPermissionResponse'

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToPath AddPermission where
        toPath = const "/"

instance ToQuery AddPermission where
        toQuery AddPermission'{..}
          = mconcat
              ["Action" =: ("AddPermission" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _aprqTopicARN, "Label" =: _aprqLabel,
               "AWSAccountId" =:
                 toQueryList "member" _aprqAWSAccountId,
               "ActionName" =: toQueryList "member" _aprqActionName]

-- | /See:/ 'addPermissionResponse' smart constructor.
data AddPermissionResponse =
    AddPermissionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddPermissionResponse' smart constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse'
