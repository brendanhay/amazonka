{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic's access control policy, granting access for the specified AWS accounts to the specified actions.
--
--
module Network.AWS.SNS.AddPermission
    (
    -- * Creating a Request
      addPermission
    , AddPermission
    -- * Request Lenses
    , apTopicARN
    , apLabel
    , apAWSAccountId
    , apActionName

    -- * Destructuring the Response
    , addPermissionResponse
    , AddPermissionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | /See:/ 'addPermission' smart constructor.
data AddPermission = AddPermission'
  { _apTopicARN     :: !Text
  , _apLabel        :: !Text
  , _apAWSAccountId :: ![Text]
  , _apActionName   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apTopicARN' - The ARN of the topic whose access control policy you wish to modify.
--
-- * 'apLabel' - A unique identifier for the new policy statement.
--
-- * 'apAWSAccountId' - The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
--
-- * 'apActionName' - The action you want to allow for the specified principal(s). Valid values: any Amazon SNS action name.
addPermission
    :: Text -- ^ 'apTopicARN'
    -> Text -- ^ 'apLabel'
    -> AddPermission
addPermission pTopicARN_ pLabel_ =
  AddPermission'
    { _apTopicARN = pTopicARN_
    , _apLabel = pLabel_
    , _apAWSAccountId = mempty
    , _apActionName = mempty
    }


-- | The ARN of the topic whose access control policy you wish to modify.
apTopicARN :: Lens' AddPermission Text
apTopicARN = lens _apTopicARN (\ s a -> s{_apTopicARN = a})

-- | A unique identifier for the new policy statement.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\ s a -> s{_apLabel = a})

-- | The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
apAWSAccountId :: Lens' AddPermission [Text]
apAWSAccountId = lens _apAWSAccountId (\ s a -> s{_apAWSAccountId = a}) . _Coerce

-- | The action you want to allow for the specified principal(s). Valid values: any Amazon SNS action name.
apActionName :: Lens' AddPermission [Text]
apActionName = lens _apActionName (\ s a -> s{_apActionName = a}) . _Coerce

instance AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        request = postQuery sns
        response = receiveNull AddPermissionResponse'

instance Hashable AddPermission where

instance NFData AddPermission where

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToPath AddPermission where
        toPath = const "/"

instance ToQuery AddPermission where
        toQuery AddPermission'{..}
          = mconcat
              ["Action" =: ("AddPermission" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _apTopicARN, "Label" =: _apLabel,
               "AWSAccountId" =:
                 toQueryList "member" _apAWSAccountId,
               "ActionName" =: toQueryList "member" _apActionName]

-- | /See:/ 'addPermissionResponse' smart constructor.
data AddPermissionResponse =
  AddPermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
--
addPermissionResponse
    :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse'


instance NFData AddPermissionResponse where
