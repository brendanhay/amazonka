{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    DescribeSMBSettings (..),
    mkDescribeSMBSettings,

    -- ** Request lenses
    dsmbsGatewayARN,

    -- * Destructuring the response
    DescribeSMBSettingsResponse (..),
    mkDescribeSMBSettingsResponse,

    -- ** Response lenses
    dsmbsrsGatewayARN,
    dsmbsrsFileSharesVisible,
    dsmbsrsActiveDirectoryStatus,
    dsmbsrsDomainName,
    dsmbsrsSMBGuestPasswordSet,
    dsmbsrsSMBSecurityStrategy,
    dsmbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeSMBSettings' smart constructor.
newtype DescribeSMBSettings = DescribeSMBSettings'
  { gatewayARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSMBSettings' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
mkDescribeSMBSettings ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeSMBSettings
mkDescribeSMBSettings pGatewayARN_ =
  DescribeSMBSettings' {gatewayARN = pGatewayARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsGatewayARN :: Lens.Lens' DescribeSMBSettings Lude.Text
dsmbsGatewayARN = Lens.lens (gatewayARN :: DescribeSMBSettings -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeSMBSettings)
{-# DEPRECATED dsmbsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest DescribeSMBSettings where
  type Rs DescribeSMBSettings = DescribeSMBSettingsResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSMBSettingsResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "FileSharesVisible")
            Lude.<*> (x Lude..?> "ActiveDirectoryStatus")
            Lude.<*> (x Lude..?> "DomainName")
            Lude.<*> (x Lude..?> "SMBGuestPasswordSet")
            Lude.<*> (x Lude..?> "SMBSecurityStrategy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSMBSettings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DescribeSMBSettings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSMBSettings where
  toJSON DescribeSMBSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GatewayARN" Lude..= gatewayARN)])

instance Lude.ToPath DescribeSMBSettings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSMBSettings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSMBSettingsResponse' smart constructor.
data DescribeSMBSettingsResponse = DescribeSMBSettingsResponse'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | The shares on this gateway appear when listing shares.
    fileSharesVisible :: Lude.Maybe Lude.Bool,
    -- | Indicates the status of a gateway that is a member of the Active Directory domain.
    --
    --
    --     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.
    --
    --
    --     * @DETACHED@ : Indicates that gateway is not joined to a domain.
    --
    --
    --     * @JOINED@ : Indicates that the gateway has successfully joined a domain.
    --
    --
    --     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.
    --
    --
    --     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.
    --
    --
    --     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.
    --
    --
    --     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
    activeDirectoryStatus :: Lude.Maybe ActiveDirectoryStatus,
    -- | The name of the domain that the gateway is joined to.
    domainName :: Lude.Maybe Lude.Text,
    -- | This value is @true@ if a password for the guest user @smbguest@ is set, otherwise @false@ .
    --
    -- Valid Values: @true@ | @false@
    sMBGuestPasswordSet :: Lude.Maybe Lude.Bool,
    -- | The type of security strategy that was specified for file gateway.
    --
    --
    --     * @ClientSpecified@ : If you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
    --
    --
    --     * @MandatorySigning@ : If you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
    --
    --
    --     * @MandatoryEncryption@ : If you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
    sMBSecurityStrategy :: Lude.Maybe SMBSecurityStrategy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSMBSettingsResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'fileSharesVisible' - The shares on this gateway appear when listing shares.
-- * 'activeDirectoryStatus' - Indicates the status of a gateway that is a member of the Active Directory domain.
--
--
--     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.
--
--
--     * @DETACHED@ : Indicates that gateway is not joined to a domain.
--
--
--     * @JOINED@ : Indicates that the gateway has successfully joined a domain.
--
--
--     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.
--
--
--     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.
--
--
--     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.
--
--
--     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
--
--
-- * 'domainName' - The name of the domain that the gateway is joined to.
-- * 'sMBGuestPasswordSet' - This value is @true@ if a password for the guest user @smbguest@ is set, otherwise @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'sMBSecurityStrategy' - The type of security strategy that was specified for file gateway.
--
--
--     * @ClientSpecified@ : If you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
--
--
--     * @MandatorySigning@ : If you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
--
--
--     * @MandatoryEncryption@ : If you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
--
--
-- * 'responseStatus' - The response status code.
mkDescribeSMBSettingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSMBSettingsResponse
mkDescribeSMBSettingsResponse pResponseStatus_ =
  DescribeSMBSettingsResponse'
    { gatewayARN = Lude.Nothing,
      fileSharesVisible = Lude.Nothing,
      activeDirectoryStatus = Lude.Nothing,
      domainName = Lude.Nothing,
      sMBGuestPasswordSet = Lude.Nothing,
      sMBSecurityStrategy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsGatewayARN :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe Lude.Text)
dsmbsrsGatewayARN = Lens.lens (gatewayARN :: DescribeSMBSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The shares on this gateway appear when listing shares.
--
-- /Note:/ Consider using 'fileSharesVisible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsFileSharesVisible :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe Lude.Bool)
dsmbsrsFileSharesVisible = Lens.lens (fileSharesVisible :: DescribeSMBSettingsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {fileSharesVisible = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsFileSharesVisible "Use generic-lens or generic-optics with 'fileSharesVisible' instead." #-}

-- | Indicates the status of a gateway that is a member of the Active Directory domain.
--
--
--     * @ACCESS_DENIED@ : Indicates that the @JoinDomain@ operation failed due to an authentication error.
--
--
--     * @DETACHED@ : Indicates that gateway is not joined to a domain.
--
--
--     * @JOINED@ : Indicates that the gateway has successfully joined a domain.
--
--
--     * @JOINING@ : Indicates that a @JoinDomain@ operation is in progress.
--
--
--     * @NETWORK_ERROR@ : Indicates that @JoinDomain@ operation failed due to a network or connectivity error.
--
--
--     * @TIMEOUT@ : Indicates that the @JoinDomain@ operation failed because the operation didn't complete within the allotted time.
--
--
--     * @UNKNOWN_ERROR@ : Indicates that the @JoinDomain@ operation failed due to another type of error.
--
--
--
-- /Note:/ Consider using 'activeDirectoryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsActiveDirectoryStatus :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe ActiveDirectoryStatus)
dsmbsrsActiveDirectoryStatus = Lens.lens (activeDirectoryStatus :: DescribeSMBSettingsResponse -> Lude.Maybe ActiveDirectoryStatus) (\s a -> s {activeDirectoryStatus = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsActiveDirectoryStatus "Use generic-lens or generic-optics with 'activeDirectoryStatus' instead." #-}

-- | The name of the domain that the gateway is joined to.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsDomainName :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe Lude.Text)
dsmbsrsDomainName = Lens.lens (domainName :: DescribeSMBSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | This value is @true@ if a password for the guest user @smbguest@ is set, otherwise @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'sMBGuestPasswordSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsSMBGuestPasswordSet :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe Lude.Bool)
dsmbsrsSMBGuestPasswordSet = Lens.lens (sMBGuestPasswordSet :: DescribeSMBSettingsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {sMBGuestPasswordSet = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsSMBGuestPasswordSet "Use generic-lens or generic-optics with 'sMBGuestPasswordSet' instead." #-}

-- | The type of security strategy that was specified for file gateway.
--
--
--     * @ClientSpecified@ : If you use this option, requests are established based on what is negotiated by the client. This option is recommended when you want to maximize compatibility across different clients in your environment.
--
--
--     * @MandatorySigning@ : If you use this option, file gateway only allows connections from SMBv2 or SMBv3 clients that have signing enabled. This option works with SMB clients on Microsoft Windows Vista, Windows Server 2008 or newer.
--
--
--     * @MandatoryEncryption@ : If you use this option, file gateway only allows connections from SMBv3 clients that have encryption enabled. This option is highly recommended for environments that handle sensitive data. This option works with SMB clients on Microsoft Windows 8, Windows Server 2012 or newer.
--
--
--
-- /Note:/ Consider using 'sMBSecurityStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsSMBSecurityStrategy :: Lens.Lens' DescribeSMBSettingsResponse (Lude.Maybe SMBSecurityStrategy)
dsmbsrsSMBSecurityStrategy = Lens.lens (sMBSecurityStrategy :: DescribeSMBSettingsResponse -> Lude.Maybe SMBSecurityStrategy) (\s a -> s {sMBSecurityStrategy = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsSMBSecurityStrategy "Use generic-lens or generic-optics with 'sMBSecurityStrategy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbsrsResponseStatus :: Lens.Lens' DescribeSMBSettingsResponse Lude.Int
dsmbsrsResponseStatus = Lens.lens (responseStatus :: DescribeSMBSettingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSMBSettingsResponse)
{-# DEPRECATED dsmbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
