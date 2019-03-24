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
-- Module      : Network.AWS.Connect.UpdateUserPhoneConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the phone configuration settings in the @UserPhoneConfig@ object for the specified user.
--
--
module Network.AWS.Connect.UpdateUserPhoneConfig
    (
    -- * Creating a Request
      updateUserPhoneConfig
    , UpdateUserPhoneConfig
    -- * Request Lenses
    , uupcPhoneConfig
    , uupcUserId
    , uupcInstanceId

    -- * Destructuring the Response
    , updateUserPhoneConfigResponse
    , UpdateUserPhoneConfigResponse
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { _uupcPhoneConfig :: !UserPhoneConfig
  , _uupcUserId      :: !Text
  , _uupcInstanceId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPhoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcPhoneConfig' - A @UserPhoneConfig@ object that contains settings for @AfterContactWorkTimeLimit@ , @AutoAccept@ , @DeskPhoneNumber@ , and @PhoneType@ to assign to the user.
--
-- * 'uupcUserId' - The identifier for the user account to change phone settings for.
--
-- * 'uupcInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
updateUserPhoneConfig
    :: UserPhoneConfig -- ^ 'uupcPhoneConfig'
    -> Text -- ^ 'uupcUserId'
    -> Text -- ^ 'uupcInstanceId'
    -> UpdateUserPhoneConfig
updateUserPhoneConfig pPhoneConfig_ pUserId_ pInstanceId_ =
  UpdateUserPhoneConfig'
    { _uupcPhoneConfig = pPhoneConfig_
    , _uupcUserId = pUserId_
    , _uupcInstanceId = pInstanceId_
    }


-- | A @UserPhoneConfig@ object that contains settings for @AfterContactWorkTimeLimit@ , @AutoAccept@ , @DeskPhoneNumber@ , and @PhoneType@ to assign to the user.
uupcPhoneConfig :: Lens' UpdateUserPhoneConfig UserPhoneConfig
uupcPhoneConfig = lens _uupcPhoneConfig (\ s a -> s{_uupcPhoneConfig = a})

-- | The identifier for the user account to change phone settings for.
uupcUserId :: Lens' UpdateUserPhoneConfig Text
uupcUserId = lens _uupcUserId (\ s a -> s{_uupcUserId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
uupcInstanceId :: Lens' UpdateUserPhoneConfig Text
uupcInstanceId = lens _uupcInstanceId (\ s a -> s{_uupcInstanceId = a})

instance AWSRequest UpdateUserPhoneConfig where
        type Rs UpdateUserPhoneConfig =
             UpdateUserPhoneConfigResponse
        request = postJSON connect
        response = receiveNull UpdateUserPhoneConfigResponse'

instance Hashable UpdateUserPhoneConfig where

instance NFData UpdateUserPhoneConfig where

instance ToHeaders UpdateUserPhoneConfig where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserPhoneConfig where
        toJSON UpdateUserPhoneConfig'{..}
          = object
              (catMaybes
                 [Just ("PhoneConfig" .= _uupcPhoneConfig)])

instance ToPath UpdateUserPhoneConfig where
        toPath UpdateUserPhoneConfig'{..}
          = mconcat
              ["/users/", toBS _uupcInstanceId, "/",
               toBS _uupcUserId, "/phone-config"]

instance ToQuery UpdateUserPhoneConfig where
        toQuery = const mempty

-- | /See:/ 'updateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse =
  UpdateUserPhoneConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPhoneConfigResponse' with the minimum fields required to make a request.
--
updateUserPhoneConfigResponse
    :: UpdateUserPhoneConfigResponse
updateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'


instance NFData UpdateUserPhoneConfigResponse where
