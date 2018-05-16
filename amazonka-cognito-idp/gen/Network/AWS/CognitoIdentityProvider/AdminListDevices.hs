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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminListDevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists devices, as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminListDevices
    (
    -- * Creating a Request
      adminListDevices
    , AdminListDevices
    -- * Request Lenses
    , aldPaginationToken
    , aldLimit
    , aldUserPoolId
    , aldUsername

    -- * Destructuring the Response
    , adminListDevicesResponse
    , AdminListDevicesResponse
    -- * Response Lenses
    , aldrsPaginationToken
    , aldrsDevices
    , aldrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to list devices, as an administrator.
--
--
--
-- /See:/ 'adminListDevices' smart constructor.
data AdminListDevices = AdminListDevices'
  { _aldPaginationToken :: !(Maybe Text)
  , _aldLimit           :: !(Maybe Nat)
  , _aldUserPoolId      :: !Text
  , _aldUsername        :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminListDevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aldPaginationToken' - The pagination token.
--
-- * 'aldLimit' - The limit of the devices request.
--
-- * 'aldUserPoolId' - The user pool ID.
--
-- * 'aldUsername' - The user name.
adminListDevices
    :: Text -- ^ 'aldUserPoolId'
    -> Text -- ^ 'aldUsername'
    -> AdminListDevices
adminListDevices pUserPoolId_ pUsername_ =
  AdminListDevices'
    { _aldPaginationToken = Nothing
    , _aldLimit = Nothing
    , _aldUserPoolId = pUserPoolId_
    , _aldUsername = _Sensitive # pUsername_
    }


-- | The pagination token.
aldPaginationToken :: Lens' AdminListDevices (Maybe Text)
aldPaginationToken = lens _aldPaginationToken (\ s a -> s{_aldPaginationToken = a})

-- | The limit of the devices request.
aldLimit :: Lens' AdminListDevices (Maybe Natural)
aldLimit = lens _aldLimit (\ s a -> s{_aldLimit = a}) . mapping _Nat

-- | The user pool ID.
aldUserPoolId :: Lens' AdminListDevices Text
aldUserPoolId = lens _aldUserPoolId (\ s a -> s{_aldUserPoolId = a})

-- | The user name.
aldUsername :: Lens' AdminListDevices Text
aldUsername = lens _aldUsername (\ s a -> s{_aldUsername = a}) . _Sensitive

instance AWSRequest AdminListDevices where
        type Rs AdminListDevices = AdminListDevicesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminListDevicesResponse' <$>
                   (x .?> "PaginationToken") <*>
                     (x .?> "Devices" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable AdminListDevices where

instance NFData AdminListDevices where

instance ToHeaders AdminListDevices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminListDevices"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminListDevices where
        toJSON AdminListDevices'{..}
          = object
              (catMaybes
                 [("PaginationToken" .=) <$> _aldPaginationToken,
                  ("Limit" .=) <$> _aldLimit,
                  Just ("UserPoolId" .= _aldUserPoolId),
                  Just ("Username" .= _aldUsername)])

instance ToPath AdminListDevices where
        toPath = const "/"

instance ToQuery AdminListDevices where
        toQuery = const mempty

-- | Lists the device's response, as an administrator.
--
--
--
-- /See:/ 'adminListDevicesResponse' smart constructor.
data AdminListDevicesResponse = AdminListDevicesResponse'
  { _aldrsPaginationToken :: !(Maybe Text)
  , _aldrsDevices         :: !(Maybe [DeviceType])
  , _aldrsResponseStatus  :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminListDevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aldrsPaginationToken' - The pagination token.
--
-- * 'aldrsDevices' - The devices in the list of devices response.
--
-- * 'aldrsResponseStatus' - -- | The response status code.
adminListDevicesResponse
    :: Int -- ^ 'aldrsResponseStatus'
    -> AdminListDevicesResponse
adminListDevicesResponse pResponseStatus_ =
  AdminListDevicesResponse'
    { _aldrsPaginationToken = Nothing
    , _aldrsDevices = Nothing
    , _aldrsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
aldrsPaginationToken :: Lens' AdminListDevicesResponse (Maybe Text)
aldrsPaginationToken = lens _aldrsPaginationToken (\ s a -> s{_aldrsPaginationToken = a})

-- | The devices in the list of devices response.
aldrsDevices :: Lens' AdminListDevicesResponse [DeviceType]
aldrsDevices = lens _aldrsDevices (\ s a -> s{_aldrsDevices = a}) . _Default . _Coerce

-- | -- | The response status code.
aldrsResponseStatus :: Lens' AdminListDevicesResponse Int
aldrsResponseStatus = lens _aldrsResponseStatus (\ s a -> s{_aldrsResponseStatus = a})

instance NFData AdminListDevicesResponse where
