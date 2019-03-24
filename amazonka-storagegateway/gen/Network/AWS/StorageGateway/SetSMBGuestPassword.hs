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
-- Module      : Network.AWS.StorageGateway.SetSMBGuestPassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for the guest user @smbguest@ . The @smbguest@ user is the user when the authentication method for the file share is set to @GuestAccess@ .
--
--
module Network.AWS.StorageGateway.SetSMBGuestPassword
    (
    -- * Creating a Request
      setSMBGuestPassword
    , SetSMBGuestPassword
    -- * Request Lenses
    , ssmbgpGatewayARN
    , ssmbgpPassword

    -- * Destructuring the Response
    , setSMBGuestPasswordResponse
    , SetSMBGuestPasswordResponse
    -- * Response Lenses
    , ssmbgprsGatewayARN
    , ssmbgprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | SetSMBGuestPasswordInput
--
--
--
-- /See:/ 'setSMBGuestPassword' smart constructor.
data SetSMBGuestPassword = SetSMBGuestPassword'
  { _ssmbgpGatewayARN :: !Text
  , _ssmbgpPassword   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSMBGuestPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssmbgpGatewayARN' - The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
--
-- * 'ssmbgpPassword' - The password that you want to set for your SMB Server.
setSMBGuestPassword
    :: Text -- ^ 'ssmbgpGatewayARN'
    -> Text -- ^ 'ssmbgpPassword'
    -> SetSMBGuestPassword
setSMBGuestPassword pGatewayARN_ pPassword_ =
  SetSMBGuestPassword'
    { _ssmbgpGatewayARN = pGatewayARN_
    , _ssmbgpPassword = _Sensitive # pPassword_
    }


-- | The Amazon Resource Name (ARN) of the file gateway the SMB file share is associated with.
ssmbgpGatewayARN :: Lens' SetSMBGuestPassword Text
ssmbgpGatewayARN = lens _ssmbgpGatewayARN (\ s a -> s{_ssmbgpGatewayARN = a})

-- | The password that you want to set for your SMB Server.
ssmbgpPassword :: Lens' SetSMBGuestPassword Text
ssmbgpPassword = lens _ssmbgpPassword (\ s a -> s{_ssmbgpPassword = a}) . _Sensitive

instance AWSRequest SetSMBGuestPassword where
        type Rs SetSMBGuestPassword =
             SetSMBGuestPasswordResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 SetSMBGuestPasswordResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable SetSMBGuestPassword where

instance NFData SetSMBGuestPassword where

instance ToHeaders SetSMBGuestPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.SetSMBGuestPassword" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetSMBGuestPassword where
        toJSON SetSMBGuestPassword'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _ssmbgpGatewayARN),
                  Just ("Password" .= _ssmbgpPassword)])

instance ToPath SetSMBGuestPassword where
        toPath = const "/"

instance ToQuery SetSMBGuestPassword where
        toQuery = const mempty

-- | /See:/ 'setSMBGuestPasswordResponse' smart constructor.
data SetSMBGuestPasswordResponse = SetSMBGuestPasswordResponse'
  { _ssmbgprsGatewayARN     :: !(Maybe Text)
  , _ssmbgprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSMBGuestPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssmbgprsGatewayARN' - Undocumented member.
--
-- * 'ssmbgprsResponseStatus' - -- | The response status code.
setSMBGuestPasswordResponse
    :: Int -- ^ 'ssmbgprsResponseStatus'
    -> SetSMBGuestPasswordResponse
setSMBGuestPasswordResponse pResponseStatus_ =
  SetSMBGuestPasswordResponse'
    {_ssmbgprsGatewayARN = Nothing, _ssmbgprsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ssmbgprsGatewayARN :: Lens' SetSMBGuestPasswordResponse (Maybe Text)
ssmbgprsGatewayARN = lens _ssmbgprsGatewayARN (\ s a -> s{_ssmbgprsGatewayARN = a})

-- | -- | The response status code.
ssmbgprsResponseStatus :: Lens' SetSMBGuestPasswordResponse Int
ssmbgprsResponseStatus = lens _ssmbgprsResponseStatus (\ s a -> s{_ssmbgprsResponseStatus = a})

instance NFData SetSMBGuestPasswordResponse where
