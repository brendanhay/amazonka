{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSMBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a Server Message Block (SMB) file share settings from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeSMBSettings
  ( -- * Creating a Request
    describeSMBSettings,
    DescribeSMBSettings,

    -- * Request Lenses
    dsmbsGatewayARN,

    -- * Destructuring the Response
    describeSMBSettingsResponse,
    DescribeSMBSettingsResponse,

    -- * Response Lenses
    dsmbsrsGatewayARN,
    dsmbsrsFileSharesVisible,
    dsmbsrsActiveDirectoryStatus,
    dsmbsrsDomainName,
    dsmbsrsSMBGuestPasswordSet,
    dsmbsrsSMBSecurityStrategy,
    dsmbsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeSMBSettings' smart constructor.
newtype DescribeSMBSettings = DescribeSMBSettings'
  { _dsmbsGatewayARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSMBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmbsGatewayARN' - Undocumented member.
describeSMBSettings ::
  -- | 'dsmbsGatewayARN'
  Text ->
  DescribeSMBSettings
describeSMBSettings pGatewayARN_ =
  DescribeSMBSettings' {_dsmbsGatewayARN = pGatewayARN_}

-- | Undocumented member.
dsmbsGatewayARN :: Lens' DescribeSMBSettings Text
dsmbsGatewayARN = lens _dsmbsGatewayARN (\s a -> s {_dsmbsGatewayARN = a})

instance AWSRequest DescribeSMBSettings where
  type Rs DescribeSMBSettings = DescribeSMBSettingsResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DescribeSMBSettingsResponse'
            <$> (x .?> "GatewayARN")
            <*> (x .?> "FileSharesVisible")
            <*> (x .?> "ActiveDirectoryStatus")
            <*> (x .?> "DomainName")
            <*> (x .?> "SMBGuestPasswordSet")
            <*> (x .?> "SMBSecurityStrategy")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSMBSettings

instance NFData DescribeSMBSettings

instance ToHeaders DescribeSMBSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.DescribeSMBSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeSMBSettings where
  toJSON DescribeSMBSettings' {..} =
    object (catMaybes [Just ("GatewayARN" .= _dsmbsGatewayARN)])

instance ToPath DescribeSMBSettings where
  toPath = const "/"

instance ToQuery DescribeSMBSettings where
  toQuery = const mempty

-- | /See:/ 'describeSMBSettingsResponse' smart constructor.
data DescribeSMBSettingsResponse = DescribeSMBSettingsResponse'
  { _dsmbsrsGatewayARN ::
      !(Maybe Text),
    _dsmbsrsFileSharesVisible ::
      !(Maybe Bool),
    _dsmbsrsActiveDirectoryStatus ::
      !(Maybe ActiveDirectoryStatus),
    _dsmbsrsDomainName :: !(Maybe Text),
    _dsmbsrsSMBGuestPasswordSet ::
      !(Maybe Bool),
    _dsmbsrsSMBSecurityStrategy ::
      !(Maybe SMBSecurityStrategy),
    _dsmbsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSMBSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsmbsrsGatewayARN' - Undocumented member.
--
-- * 'dsmbsrsFileSharesVisible' - The shares on this gateway appear when listing shares.
--
-- * 'dsmbsrsActiveDirectoryStatus' - Indicates the status of a gateway that is a member of the Active Directory domain.     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.     * @DETACHED@ : Indicates that gateway is not joined to a domain.     * @JOINED@ : Indicates that the gateway has successfully joined a domain.     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
--
-- * 'dsmbsrsDomainName' - The name of the domain that the gateway is joined to.
--
-- * 'dsmbsrsSMBGuestPasswordSet' - This value is @true@ if a password for the guest user @smbguest@ is set, otherwise @false@ . Valid Values: @true@ | @false@
--
-- * 'dsmbsrsSMBSecurityStrategy' - The type of security strategy that was specified for file gateway.     * @ClientSpecified@ : If you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.     * @MandatorySigning@ : If you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.     * @MandatoryEncryption@ : If you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
--
-- * 'dsmbsrsResponseStatus' - -- | The response status code.
describeSMBSettingsResponse ::
  -- | 'dsmbsrsResponseStatus'
  Int ->
  DescribeSMBSettingsResponse
describeSMBSettingsResponse pResponseStatus_ =
  DescribeSMBSettingsResponse'
    { _dsmbsrsGatewayARN = Nothing,
      _dsmbsrsFileSharesVisible = Nothing,
      _dsmbsrsActiveDirectoryStatus = Nothing,
      _dsmbsrsDomainName = Nothing,
      _dsmbsrsSMBGuestPasswordSet = Nothing,
      _dsmbsrsSMBSecurityStrategy = Nothing,
      _dsmbsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dsmbsrsGatewayARN :: Lens' DescribeSMBSettingsResponse (Maybe Text)
dsmbsrsGatewayARN = lens _dsmbsrsGatewayARN (\s a -> s {_dsmbsrsGatewayARN = a})

-- | The shares on this gateway appear when listing shares.
dsmbsrsFileSharesVisible :: Lens' DescribeSMBSettingsResponse (Maybe Bool)
dsmbsrsFileSharesVisible = lens _dsmbsrsFileSharesVisible (\s a -> s {_dsmbsrsFileSharesVisible = a})

-- | Indicates the status of a gateway that is a member of the Active Directory domain.     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.     * @DETACHED@ : Indicates that gateway is not joined to a domain.     * @JOINED@ : Indicates that the gateway has successfully joined a domain.     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
dsmbsrsActiveDirectoryStatus :: Lens' DescribeSMBSettingsResponse (Maybe ActiveDirectoryStatus)
dsmbsrsActiveDirectoryStatus = lens _dsmbsrsActiveDirectoryStatus (\s a -> s {_dsmbsrsActiveDirectoryStatus = a})

-- | The name of the domain that the gateway is joined to.
dsmbsrsDomainName :: Lens' DescribeSMBSettingsResponse (Maybe Text)
dsmbsrsDomainName = lens _dsmbsrsDomainName (\s a -> s {_dsmbsrsDomainName = a})

-- | This value is @true@ if a password for the guest user @smbguest@ is set, otherwise @false@ . Valid Values: @true@ | @false@
dsmbsrsSMBGuestPasswordSet :: Lens' DescribeSMBSettingsResponse (Maybe Bool)
dsmbsrsSMBGuestPasswordSet = lens _dsmbsrsSMBGuestPasswordSet (\s a -> s {_dsmbsrsSMBGuestPasswordSet = a})

-- | The type of security strategy that was specified for file gateway.     * @ClientSpecified@ : If you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.     * @MandatorySigning@ : If you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.     * @MandatoryEncryption@ : If you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
dsmbsrsSMBSecurityStrategy :: Lens' DescribeSMBSettingsResponse (Maybe SMBSecurityStrategy)
dsmbsrsSMBSecurityStrategy = lens _dsmbsrsSMBSecurityStrategy (\s a -> s {_dsmbsrsSMBSecurityStrategy = a})

-- | -- | The response status code.
dsmbsrsResponseStatus :: Lens' DescribeSMBSettingsResponse Int
dsmbsrsResponseStatus = lens _dsmbsrsResponseStatus (\s a -> s {_dsmbsrsResponseStatus = a})

instance NFData DescribeSMBSettingsResponse
