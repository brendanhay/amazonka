{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.DescribeSMBSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a Server Message Block (SMB) file share settings
-- from a file gateway. This operation is only supported for file gateways.
module Amazonka.StorageGateway.DescribeSMBSettings
  ( -- * Creating a Request
    DescribeSMBSettings (..),
    newDescribeSMBSettings,

    -- * Request Lenses
    describeSMBSettings_gatewayARN,

    -- * Destructuring the Response
    DescribeSMBSettingsResponse (..),
    newDescribeSMBSettingsResponse,

    -- * Response Lenses
    describeSMBSettingsResponse_sMBSecurityStrategy,
    describeSMBSettingsResponse_domainName,
    describeSMBSettingsResponse_sMBLocalGroups,
    describeSMBSettingsResponse_gatewayARN,
    describeSMBSettingsResponse_sMBGuestPasswordSet,
    describeSMBSettingsResponse_fileSharesVisible,
    describeSMBSettingsResponse_activeDirectoryStatus,
    describeSMBSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDescribeSMBSettings' smart constructor.
data DescribeSMBSettings = DescribeSMBSettings'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSMBSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeSMBSettings_gatewayARN' - Undocumented member.
newDescribeSMBSettings ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeSMBSettings
newDescribeSMBSettings pGatewayARN_ =
  DescribeSMBSettings' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeSMBSettings_gatewayARN :: Lens.Lens' DescribeSMBSettings Prelude.Text
describeSMBSettings_gatewayARN = Lens.lens (\DescribeSMBSettings' {gatewayARN} -> gatewayARN) (\s@DescribeSMBSettings' {} a -> s {gatewayARN = a} :: DescribeSMBSettings)

instance Core.AWSRequest DescribeSMBSettings where
  type
    AWSResponse DescribeSMBSettings =
      DescribeSMBSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSMBSettingsResponse'
            Prelude.<$> (x Data..?> "SMBSecurityStrategy")
            Prelude.<*> (x Data..?> "DomainName")
            Prelude.<*> (x Data..?> "SMBLocalGroups")
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (x Data..?> "SMBGuestPasswordSet")
            Prelude.<*> (x Data..?> "FileSharesVisible")
            Prelude.<*> (x Data..?> "ActiveDirectoryStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSMBSettings where
  hashWithSalt _salt DescribeSMBSettings' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DescribeSMBSettings where
  rnf DescribeSMBSettings' {..} = Prelude.rnf gatewayARN

instance Data.ToHeaders DescribeSMBSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeSMBSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSMBSettings where
  toJSON DescribeSMBSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DescribeSMBSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSMBSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSMBSettingsResponse' smart constructor.
data DescribeSMBSettingsResponse = DescribeSMBSettingsResponse'
  { -- | The type of security strategy that was specified for file gateway.
    --
    -- -   @ClientSpecified@: If you use this option, requests are established
    --     based on what is negotiated by the client. This option is
    --     recommended when you want to maximize compatibility across different
    --     clients in your environment. Only supported for S3 File Gateways.
    --
    -- -   @MandatorySigning@: If you use this option, file gateway only allows
    --     connections from SMBv2 or SMBv3 clients that have signing enabled.
    --     This option works with SMB clients on Microsoft Windows Vista,
    --     Windows Server 2008 or newer.
    --
    -- -   @MandatoryEncryption@: If you use this option, file gateway only
    --     allows connections from SMBv3 clients that have encryption enabled.
    --     This option is highly recommended for environments that handle
    --     sensitive data. This option works with SMB clients on Microsoft
    --     Windows 8, Windows Server 2012 or newer.
    sMBSecurityStrategy :: Prelude.Maybe SMBSecurityStrategy,
    -- | The name of the domain that the gateway is joined to.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | A list of Active Directory users and groups that have special
    -- permissions for SMB file shares on the gateway.
    sMBLocalGroups :: Prelude.Maybe SMBLocalGroups,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | This value is @true@ if a password for the guest user @smbguest@ is set,
    -- otherwise @false@. Only supported for S3 File Gateways.
    --
    -- Valid Values: @true@ | @false@
    sMBGuestPasswordSet :: Prelude.Maybe Prelude.Bool,
    -- | The shares on this gateway appear when listing shares. Only supported
    -- for S3 File Gateways.
    fileSharesVisible :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the status of a gateway that is a member of the Active
    -- Directory domain.
    --
    -- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
    --     due to an authentication error.
    --
    -- -   @DETACHED@: Indicates that gateway is not joined to a domain.
    --
    -- -   @JOINED@: Indicates that the gateway has successfully joined a
    --     domain.
    --
    -- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
    --
    -- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
    --     a network or connectivity error.
    --
    -- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
    --     the operation didn\'t complete within the allotted time.
    --
    -- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
    --     due to another type of error.
    activeDirectoryStatus :: Prelude.Maybe ActiveDirectoryStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSMBSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMBSecurityStrategy', 'describeSMBSettingsResponse_sMBSecurityStrategy' - The type of security strategy that was specified for file gateway.
--
-- -   @ClientSpecified@: If you use this option, requests are established
--     based on what is negotiated by the client. This option is
--     recommended when you want to maximize compatibility across different
--     clients in your environment. Only supported for S3 File Gateways.
--
-- -   @MandatorySigning@: If you use this option, file gateway only allows
--     connections from SMBv2 or SMBv3 clients that have signing enabled.
--     This option works with SMB clients on Microsoft Windows Vista,
--     Windows Server 2008 or newer.
--
-- -   @MandatoryEncryption@: If you use this option, file gateway only
--     allows connections from SMBv3 clients that have encryption enabled.
--     This option is highly recommended for environments that handle
--     sensitive data. This option works with SMB clients on Microsoft
--     Windows 8, Windows Server 2012 or newer.
--
-- 'domainName', 'describeSMBSettingsResponse_domainName' - The name of the domain that the gateway is joined to.
--
-- 'sMBLocalGroups', 'describeSMBSettingsResponse_sMBLocalGroups' - A list of Active Directory users and groups that have special
-- permissions for SMB file shares on the gateway.
--
-- 'gatewayARN', 'describeSMBSettingsResponse_gatewayARN' - Undocumented member.
--
-- 'sMBGuestPasswordSet', 'describeSMBSettingsResponse_sMBGuestPasswordSet' - This value is @true@ if a password for the guest user @smbguest@ is set,
-- otherwise @false@. Only supported for S3 File Gateways.
--
-- Valid Values: @true@ | @false@
--
-- 'fileSharesVisible', 'describeSMBSettingsResponse_fileSharesVisible' - The shares on this gateway appear when listing shares. Only supported
-- for S3 File Gateways.
--
-- 'activeDirectoryStatus', 'describeSMBSettingsResponse_activeDirectoryStatus' - Indicates the status of a gateway that is a member of the Active
-- Directory domain.
--
-- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
--     due to an authentication error.
--
-- -   @DETACHED@: Indicates that gateway is not joined to a domain.
--
-- -   @JOINED@: Indicates that the gateway has successfully joined a
--     domain.
--
-- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
--
-- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
--     a network or connectivity error.
--
-- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
--     the operation didn\'t complete within the allotted time.
--
-- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
--     due to another type of error.
--
-- 'httpStatus', 'describeSMBSettingsResponse_httpStatus' - The response's http status code.
newDescribeSMBSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSMBSettingsResponse
newDescribeSMBSettingsResponse pHttpStatus_ =
  DescribeSMBSettingsResponse'
    { sMBSecurityStrategy =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      sMBLocalGroups = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      sMBGuestPasswordSet = Prelude.Nothing,
      fileSharesVisible = Prelude.Nothing,
      activeDirectoryStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of security strategy that was specified for file gateway.
--
-- -   @ClientSpecified@: If you use this option, requests are established
--     based on what is negotiated by the client. This option is
--     recommended when you want to maximize compatibility across different
--     clients in your environment. Only supported for S3 File Gateways.
--
-- -   @MandatorySigning@: If you use this option, file gateway only allows
--     connections from SMBv2 or SMBv3 clients that have signing enabled.
--     This option works with SMB clients on Microsoft Windows Vista,
--     Windows Server 2008 or newer.
--
-- -   @MandatoryEncryption@: If you use this option, file gateway only
--     allows connections from SMBv3 clients that have encryption enabled.
--     This option is highly recommended for environments that handle
--     sensitive data. This option works with SMB clients on Microsoft
--     Windows 8, Windows Server 2012 or newer.
describeSMBSettingsResponse_sMBSecurityStrategy :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe SMBSecurityStrategy)
describeSMBSettingsResponse_sMBSecurityStrategy = Lens.lens (\DescribeSMBSettingsResponse' {sMBSecurityStrategy} -> sMBSecurityStrategy) (\s@DescribeSMBSettingsResponse' {} a -> s {sMBSecurityStrategy = a} :: DescribeSMBSettingsResponse)

-- | The name of the domain that the gateway is joined to.
describeSMBSettingsResponse_domainName :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe Prelude.Text)
describeSMBSettingsResponse_domainName = Lens.lens (\DescribeSMBSettingsResponse' {domainName} -> domainName) (\s@DescribeSMBSettingsResponse' {} a -> s {domainName = a} :: DescribeSMBSettingsResponse)

-- | A list of Active Directory users and groups that have special
-- permissions for SMB file shares on the gateway.
describeSMBSettingsResponse_sMBLocalGroups :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe SMBLocalGroups)
describeSMBSettingsResponse_sMBLocalGroups = Lens.lens (\DescribeSMBSettingsResponse' {sMBLocalGroups} -> sMBLocalGroups) (\s@DescribeSMBSettingsResponse' {} a -> s {sMBLocalGroups = a} :: DescribeSMBSettingsResponse)

-- | Undocumented member.
describeSMBSettingsResponse_gatewayARN :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe Prelude.Text)
describeSMBSettingsResponse_gatewayARN = Lens.lens (\DescribeSMBSettingsResponse' {gatewayARN} -> gatewayARN) (\s@DescribeSMBSettingsResponse' {} a -> s {gatewayARN = a} :: DescribeSMBSettingsResponse)

-- | This value is @true@ if a password for the guest user @smbguest@ is set,
-- otherwise @false@. Only supported for S3 File Gateways.
--
-- Valid Values: @true@ | @false@
describeSMBSettingsResponse_sMBGuestPasswordSet :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe Prelude.Bool)
describeSMBSettingsResponse_sMBGuestPasswordSet = Lens.lens (\DescribeSMBSettingsResponse' {sMBGuestPasswordSet} -> sMBGuestPasswordSet) (\s@DescribeSMBSettingsResponse' {} a -> s {sMBGuestPasswordSet = a} :: DescribeSMBSettingsResponse)

-- | The shares on this gateway appear when listing shares. Only supported
-- for S3 File Gateways.
describeSMBSettingsResponse_fileSharesVisible :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe Prelude.Bool)
describeSMBSettingsResponse_fileSharesVisible = Lens.lens (\DescribeSMBSettingsResponse' {fileSharesVisible} -> fileSharesVisible) (\s@DescribeSMBSettingsResponse' {} a -> s {fileSharesVisible = a} :: DescribeSMBSettingsResponse)

-- | Indicates the status of a gateway that is a member of the Active
-- Directory domain.
--
-- -   @ACCESS_DENIED@: Indicates that the @JoinDomain@ operation failed
--     due to an authentication error.
--
-- -   @DETACHED@: Indicates that gateway is not joined to a domain.
--
-- -   @JOINED@: Indicates that the gateway has successfully joined a
--     domain.
--
-- -   @JOINING@: Indicates that a @JoinDomain@ operation is in progress.
--
-- -   @NETWORK_ERROR@: Indicates that @JoinDomain@ operation failed due to
--     a network or connectivity error.
--
-- -   @TIMEOUT@: Indicates that the @JoinDomain@ operation failed because
--     the operation didn\'t complete within the allotted time.
--
-- -   @UNKNOWN_ERROR@: Indicates that the @JoinDomain@ operation failed
--     due to another type of error.
describeSMBSettingsResponse_activeDirectoryStatus :: Lens.Lens' DescribeSMBSettingsResponse (Prelude.Maybe ActiveDirectoryStatus)
describeSMBSettingsResponse_activeDirectoryStatus = Lens.lens (\DescribeSMBSettingsResponse' {activeDirectoryStatus} -> activeDirectoryStatus) (\s@DescribeSMBSettingsResponse' {} a -> s {activeDirectoryStatus = a} :: DescribeSMBSettingsResponse)

-- | The response's http status code.
describeSMBSettingsResponse_httpStatus :: Lens.Lens' DescribeSMBSettingsResponse Prelude.Int
describeSMBSettingsResponse_httpStatus = Lens.lens (\DescribeSMBSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeSMBSettingsResponse' {} a -> s {httpStatus = a} :: DescribeSMBSettingsResponse)

instance Prelude.NFData DescribeSMBSettingsResponse where
  rnf DescribeSMBSettingsResponse' {..} =
    Prelude.rnf sMBSecurityStrategy
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf sMBLocalGroups
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf sMBGuestPasswordSet
      `Prelude.seq` Prelude.rnf fileSharesVisible
      `Prelude.seq` Prelude.rnf activeDirectoryStatus
      `Prelude.seq` Prelude.rnf httpStatus
