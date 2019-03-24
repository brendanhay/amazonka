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
-- Module      : Network.AWS.Connect.UpdateUserHierarchy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified hierarchy group to the user.
--
--
module Network.AWS.Connect.UpdateUserHierarchy
    (
    -- * Creating a Request
      updateUserHierarchy
    , UpdateUserHierarchy
    -- * Request Lenses
    , uuhHierarchyGroupId
    , uuhUserId
    , uuhInstanceId

    -- * Destructuring the Response
    , updateUserHierarchyResponse
    , UpdateUserHierarchyResponse
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserHierarchy' smart constructor.
data UpdateUserHierarchy = UpdateUserHierarchy'
  { _uuhHierarchyGroupId :: !(Maybe Text)
  , _uuhUserId           :: !Text
  , _uuhInstanceId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserHierarchy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuhHierarchyGroupId' - The identifier for the hierarchy group to assign to the user.
--
-- * 'uuhUserId' - The identifier of the user account to assign the hierarchy group to.
--
-- * 'uuhInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
updateUserHierarchy
    :: Text -- ^ 'uuhUserId'
    -> Text -- ^ 'uuhInstanceId'
    -> UpdateUserHierarchy
updateUserHierarchy pUserId_ pInstanceId_ =
  UpdateUserHierarchy'
    { _uuhHierarchyGroupId = Nothing
    , _uuhUserId = pUserId_
    , _uuhInstanceId = pInstanceId_
    }


-- | The identifier for the hierarchy group to assign to the user.
uuhHierarchyGroupId :: Lens' UpdateUserHierarchy (Maybe Text)
uuhHierarchyGroupId = lens _uuhHierarchyGroupId (\ s a -> s{_uuhHierarchyGroupId = a})

-- | The identifier of the user account to assign the hierarchy group to.
uuhUserId :: Lens' UpdateUserHierarchy Text
uuhUserId = lens _uuhUserId (\ s a -> s{_uuhUserId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
uuhInstanceId :: Lens' UpdateUserHierarchy Text
uuhInstanceId = lens _uuhInstanceId (\ s a -> s{_uuhInstanceId = a})

instance AWSRequest UpdateUserHierarchy where
        type Rs UpdateUserHierarchy =
             UpdateUserHierarchyResponse
        request = postJSON connect
        response = receiveNull UpdateUserHierarchyResponse'

instance Hashable UpdateUserHierarchy where

instance NFData UpdateUserHierarchy where

instance ToHeaders UpdateUserHierarchy where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserHierarchy where
        toJSON UpdateUserHierarchy'{..}
          = object
              (catMaybes
                 [("HierarchyGroupId" .=) <$> _uuhHierarchyGroupId])

instance ToPath UpdateUserHierarchy where
        toPath UpdateUserHierarchy'{..}
          = mconcat
              ["/users/", toBS _uuhInstanceId, "/",
               toBS _uuhUserId, "/hierarchy"]

instance ToQuery UpdateUserHierarchy where
        toQuery = const mempty

-- | /See:/ 'updateUserHierarchyResponse' smart constructor.
data UpdateUserHierarchyResponse =
  UpdateUserHierarchyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserHierarchyResponse' with the minimum fields required to make a request.
--
updateUserHierarchyResponse
    :: UpdateUserHierarchyResponse
updateUserHierarchyResponse = UpdateUserHierarchyResponse'


instance NFData UpdateUserHierarchyResponse where
