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
-- Module      : Network.AWS.StorageGateway.DescribeSMBSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a Server Message Block (SMB) file share settings from a file gateway. This operation is only supported for file gateways.
--
--
module Network.AWS.StorageGateway.DescribeSMBSettings
    (
    -- * Creating a Request
      describeSMBSettings
    , DescribeSMBSettings
    -- * Request Lenses
    , dsmbsGatewayARN

    -- * Destructuring the Response
    , describeSMBSettingsResponse
    , DescribeSMBSettingsResponse
    -- * Response Lenses
    , dsmbsrsGatewayARN
    , dsmbsrsDomainName
    , dsmbsrsSMBGuestPasswordSet
    , dsmbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'describeSMBSettings' smart constructor.
newtype DescribeSMBSettings = DescribeSMBSettings'
  { _dsmbsGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSMBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmbsGatewayARN' - Undocumented member.
describeSMBSettings
    :: Text -- ^ 'dsmbsGatewayARN'
    -> DescribeSMBSettings
describeSMBSettings pGatewayARN_ =
  DescribeSMBSettings' {_dsmbsGatewayARN = pGatewayARN_}


-- | Undocumented member.
dsmbsGatewayARN :: Lens' DescribeSMBSettings Text
dsmbsGatewayARN = lens _dsmbsGatewayARN (\ s a -> s{_dsmbsGatewayARN = a})

instance AWSRequest DescribeSMBSettings where
        type Rs DescribeSMBSettings =
             DescribeSMBSettingsResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSMBSettingsResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "DomainName") <*>
                     (x .?> "SMBGuestPasswordSet")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSMBSettings where

instance NFData DescribeSMBSettings where

instance ToHeaders DescribeSMBSettings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeSMBSettings" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSMBSettings where
        toJSON DescribeSMBSettings'{..}
          = object
              (catMaybes [Just ("GatewayARN" .= _dsmbsGatewayARN)])

instance ToPath DescribeSMBSettings where
        toPath = const "/"

instance ToQuery DescribeSMBSettings where
        toQuery = const mempty

-- | /See:/ 'describeSMBSettingsResponse' smart constructor.
data DescribeSMBSettingsResponse = DescribeSMBSettingsResponse'
  { _dsmbsrsGatewayARN          :: !(Maybe Text)
  , _dsmbsrsDomainName          :: !(Maybe Text)
  , _dsmbsrsSMBGuestPasswordSet :: !(Maybe Bool)
  , _dsmbsrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSMBSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmbsrsGatewayARN' - Undocumented member.
--
-- * 'dsmbsrsDomainName' - The name of the domain that the gateway is joined to.
--
-- * 'dsmbsrsSMBGuestPasswordSet' - This value is true if a password for the guest user
