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
-- Module      : Network.AWS.StorageGateway.SetLocalConsolePassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the password for your VM local console. When you log in to the local console for the first time, you log in to the VM with the default credentials. We recommend that you set a new password. You don't need to know the default password to set a new password.
--
--
module Network.AWS.StorageGateway.SetLocalConsolePassword
    (
    -- * Creating a Request
      setLocalConsolePassword
    , SetLocalConsolePassword
    -- * Request Lenses
    , slcpGatewayARN
    , slcpLocalConsolePassword

    -- * Destructuring the Response
    , setLocalConsolePasswordResponse
    , SetLocalConsolePasswordResponse
    -- * Response Lenses
    , slcprsGatewayARN
    , slcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | SetLocalConsolePasswordInput
--
--
--
-- /See:/ 'setLocalConsolePassword' smart constructor.
data SetLocalConsolePassword = SetLocalConsolePassword'
  { _slcpGatewayARN           :: !Text
  , _slcpLocalConsolePassword :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLocalConsolePassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcpGatewayARN' - Undocumented member.
--
-- * 'slcpLocalConsolePassword' - The password you want to set for your VM local console.
setLocalConsolePassword
    :: Text -- ^ 'slcpGatewayARN'
    -> Text -- ^ 'slcpLocalConsolePassword'
    -> SetLocalConsolePassword
setLocalConsolePassword pGatewayARN_ pLocalConsolePassword_ =
  SetLocalConsolePassword'
    { _slcpGatewayARN = pGatewayARN_
    , _slcpLocalConsolePassword = _Sensitive # pLocalConsolePassword_
    }


-- | Undocumented member.
slcpGatewayARN :: Lens' SetLocalConsolePassword Text
slcpGatewayARN = lens _slcpGatewayARN (\ s a -> s{_slcpGatewayARN = a})

-- | The password you want to set for your VM local console.
slcpLocalConsolePassword :: Lens' SetLocalConsolePassword Text
slcpLocalConsolePassword = lens _slcpLocalConsolePassword (\ s a -> s{_slcpLocalConsolePassword = a}) . _Sensitive

instance AWSRequest SetLocalConsolePassword where
        type Rs SetLocalConsolePassword =
             SetLocalConsolePasswordResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 SetLocalConsolePasswordResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable SetLocalConsolePassword where

instance NFData SetLocalConsolePassword where

instance ToHeaders SetLocalConsolePassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.SetLocalConsolePassword" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetLocalConsolePassword where
        toJSON SetLocalConsolePassword'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _slcpGatewayARN),
                  Just
                    ("LocalConsolePassword" .=
                       _slcpLocalConsolePassword)])

instance ToPath SetLocalConsolePassword where
        toPath = const "/"

instance ToQuery SetLocalConsolePassword where
        toQuery = const mempty

-- | /See:/ 'setLocalConsolePasswordResponse' smart constructor.
data SetLocalConsolePasswordResponse = SetLocalConsolePasswordResponse'
  { _slcprsGatewayARN     :: !(Maybe Text)
  , _slcprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLocalConsolePasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcprsGatewayARN' - Undocumented member.
--
-- * 'slcprsResponseStatus' - -- | The response status code.
setLocalConsolePasswordResponse
    :: Int -- ^ 'slcprsResponseStatus'
    -> SetLocalConsolePasswordResponse
setLocalConsolePasswordResponse pResponseStatus_ =
  SetLocalConsolePasswordResponse'
    {_slcprsGatewayARN = Nothing, _slcprsResponseStatus = pResponseStatus_}


-- | Undocumented member.
slcprsGatewayARN :: Lens' SetLocalConsolePasswordResponse (Maybe Text)
slcprsGatewayARN = lens _slcprsGatewayARN (\ s a -> s{_slcprsGatewayARN = a})

-- | -- | The response status code.
slcprsResponseStatus :: Lens' SetLocalConsolePasswordResponse Int
slcprsResponseStatus = lens _slcprsResponseStatus (\ s a -> s{_slcprsResponseStatus = a})

instance NFData SetLocalConsolePasswordResponse where
