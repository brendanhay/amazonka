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
-- Module      : Network.AWS.CognitoIdentityProvider.ListDevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the devices.
--
--
module Network.AWS.CognitoIdentityProvider.ListDevices
    (
    -- * Creating a Request
      listDevices
    , ListDevices
    -- * Request Lenses
    , ldPaginationToken
    , ldLimit
    , ldAccessToken

    -- * Destructuring the Response
    , listDevicesResponse
    , ListDevicesResponse
    -- * Response Lenses
    , ldrsPaginationToken
    , ldrsDevices
    , ldrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list the devices.
--
--
--
-- /See:/ 'listDevices' smart constructor.
data ListDevices = ListDevices'
  { _ldPaginationToken :: !(Maybe Text)
  , _ldLimit           :: !(Maybe Nat)
  , _ldAccessToken     :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldPaginationToken' - The pagination token for the list request.
--
-- * 'ldLimit' - The limit of the device request.
--
-- * 'ldAccessToken' - The access tokens for the request to list devices.
listDevices
    :: Text -- ^ 'ldAccessToken'
    -> ListDevices
listDevices pAccessToken_ =
  ListDevices'
    { _ldPaginationToken = Nothing
    , _ldLimit = Nothing
    , _ldAccessToken = _Sensitive # pAccessToken_
    }


-- | The pagination token for the list request.
ldPaginationToken :: Lens' ListDevices (Maybe Text)
ldPaginationToken = lens _ldPaginationToken (\ s a -> s{_ldPaginationToken = a})

-- | The limit of the device request.
ldLimit :: Lens' ListDevices (Maybe Natural)
ldLimit = lens _ldLimit (\ s a -> s{_ldLimit = a}) . mapping _Nat

-- | The access tokens for the request to list devices.
ldAccessToken :: Lens' ListDevices Text
ldAccessToken = lens _ldAccessToken (\ s a -> s{_ldAccessToken = a}) . _Sensitive

instance AWSRequest ListDevices where
        type Rs ListDevices = ListDevicesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ListDevicesResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "Devices" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDevices where

instance NFData ListDevices where

instance ToHeaders ListDevices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ListDevices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDevices where
        toJSON ListDevices'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _ldPaginationToken,
                  ("Limit" .=) <$> _ldLimit,
                  Just ("AccessToken" .= _ldAccessToken)])

instance ToPath ListDevices where
        toPath = const "/"

instance ToQuery ListDevices where
        toQuery = const mempty

-- | Represents the response to list devices.
--
--
--
-- /See:/ 'listDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { _ldrsPaginationToken :: !(Maybe Text)
  , _ldrsDevices         :: !(Maybe [DeviceType])
  , _ldrsResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsPaginationToken' - The pagination token for the list device response.
--
-- * 'ldrsDevices' - The devices returned in the list devices response.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDevicesResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDevicesResponse
listDevicesResponse pResponseStatus_ =
  ListDevicesResponse'
    { _ldrsPaginationToken = Nothing
    , _ldrsDevices = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    }


-- | The pagination token for the list device response.
ldrsPaginationToken :: Lens' ListDevicesResponse (Maybe Text)
ldrsPaginationToken = lens _ldrsPaginationToken (\ s a -> s{_ldrsPaginationToken = a})

-- | The devices returned in the list devices response.
ldrsDevices :: Lens' ListDevicesResponse [DeviceType]
ldrsDevices = lens _ldrsDevices (\ s a -> s{_ldrsDevices = a}) . _Default . _Coerce

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDevicesResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDevicesResponse where
