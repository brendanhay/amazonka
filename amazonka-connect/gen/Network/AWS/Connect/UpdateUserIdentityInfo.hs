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
-- Module      : Network.AWS.Connect.UpdateUserIdentityInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity information for the specified user in a @UserIdentityInfo@ object, including email, first name, and last name.
--
--
module Network.AWS.Connect.UpdateUserIdentityInfo
    (
    -- * Creating a Request
      updateUserIdentityInfo
    , UpdateUserIdentityInfo
    -- * Request Lenses
    , uuiiIdentityInfo
    , uuiiUserId
    , uuiiInstanceId

    -- * Destructuring the Response
    , updateUserIdentityInfoResponse
    , UpdateUserIdentityInfoResponse
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserIdentityInfo' smart constructor.
data UpdateUserIdentityInfo = UpdateUserIdentityInfo'
  { _uuiiIdentityInfo :: !UserIdentityInfo
  , _uuiiUserId       :: !Text
  , _uuiiInstanceId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserIdentityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuiiIdentityInfo' - A @UserIdentityInfo@ object.
--
-- * 'uuiiUserId' - The identifier for the user account to update identity information for.
--
-- * 'uuiiInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
updateUserIdentityInfo
    :: UserIdentityInfo -- ^ 'uuiiIdentityInfo'
    -> Text -- ^ 'uuiiUserId'
    -> Text -- ^ 'uuiiInstanceId'
    -> UpdateUserIdentityInfo
updateUserIdentityInfo pIdentityInfo_ pUserId_ pInstanceId_ =
  UpdateUserIdentityInfo'
    { _uuiiIdentityInfo = pIdentityInfo_
    , _uuiiUserId = pUserId_
    , _uuiiInstanceId = pInstanceId_
    }


-- | A @UserIdentityInfo@ object.
uuiiIdentityInfo :: Lens' UpdateUserIdentityInfo UserIdentityInfo
uuiiIdentityInfo = lens _uuiiIdentityInfo (\ s a -> s{_uuiiIdentityInfo = a})

-- | The identifier for the user account to update identity information for.
uuiiUserId :: Lens' UpdateUserIdentityInfo Text
uuiiUserId = lens _uuiiUserId (\ s a -> s{_uuiiUserId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
uuiiInstanceId :: Lens' UpdateUserIdentityInfo Text
uuiiInstanceId = lens _uuiiInstanceId (\ s a -> s{_uuiiInstanceId = a})

instance AWSRequest UpdateUserIdentityInfo where
        type Rs UpdateUserIdentityInfo =
             UpdateUserIdentityInfoResponse
        request = postJSON connect
        response
          = receiveNull UpdateUserIdentityInfoResponse'

instance Hashable UpdateUserIdentityInfo where

instance NFData UpdateUserIdentityInfo where

instance ToHeaders UpdateUserIdentityInfo where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserIdentityInfo where
        toJSON UpdateUserIdentityInfo'{..}
          = object
              (catMaybes
                 [Just ("IdentityInfo" .= _uuiiIdentityInfo)])

instance ToPath UpdateUserIdentityInfo where
        toPath UpdateUserIdentityInfo'{..}
          = mconcat
              ["/users/", toBS _uuiiInstanceId, "/",
               toBS _uuiiUserId, "/identity-info"]

instance ToQuery UpdateUserIdentityInfo where
        toQuery = const mempty

-- | /See:/ 'updateUserIdentityInfoResponse' smart constructor.
data UpdateUserIdentityInfoResponse =
  UpdateUserIdentityInfoResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserIdentityInfoResponse' with the minimum fields required to make a request.
--
updateUserIdentityInfoResponse
    :: UpdateUserIdentityInfoResponse
updateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'


instance NFData UpdateUserIdentityInfoResponse where
