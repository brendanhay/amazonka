{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LicenseManager.Types.LicenseOperationFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.LicenseOperationFailure where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types.Metadata
import Network.AWS.LicenseManager.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes the failure of a license operation.
--
-- /See:/ 'newLicenseOperationFailure' smart constructor.
data LicenseOperationFailure = LicenseOperationFailure'
  { -- | Resource type.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The requester is \"License Manager Automated Discovery\".
    operationRequestedBy :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    metadataList :: Prelude.Maybe [Metadata],
    -- | Name of the operation.
    operationName :: Prelude.Maybe Prelude.Text,
    -- | Failure time.
    failureTime :: Prelude.Maybe Core.POSIX,
    -- | Error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | ID of the Amazon Web Services account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseOperationFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'licenseOperationFailure_resourceType' - Resource type.
--
-- 'operationRequestedBy', 'licenseOperationFailure_operationRequestedBy' - The requester is \"License Manager Automated Discovery\".
--
-- 'resourceArn', 'licenseOperationFailure_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'metadataList', 'licenseOperationFailure_metadataList' - Reserved.
--
-- 'operationName', 'licenseOperationFailure_operationName' - Name of the operation.
--
-- 'failureTime', 'licenseOperationFailure_failureTime' - Failure time.
--
-- 'errorMessage', 'licenseOperationFailure_errorMessage' - Error message.
--
-- 'resourceOwnerId', 'licenseOperationFailure_resourceOwnerId' - ID of the Amazon Web Services account that owns the resource.
newLicenseOperationFailure ::
  LicenseOperationFailure
newLicenseOperationFailure =
  LicenseOperationFailure'
    { resourceType =
        Prelude.Nothing,
      operationRequestedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      metadataList = Prelude.Nothing,
      operationName = Prelude.Nothing,
      failureTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing
    }

-- | Resource type.
licenseOperationFailure_resourceType :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe ResourceType)
licenseOperationFailure_resourceType = Lens.lens (\LicenseOperationFailure' {resourceType} -> resourceType) (\s@LicenseOperationFailure' {} a -> s {resourceType = a} :: LicenseOperationFailure)

-- | The requester is \"License Manager Automated Discovery\".
licenseOperationFailure_operationRequestedBy :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_operationRequestedBy = Lens.lens (\LicenseOperationFailure' {operationRequestedBy} -> operationRequestedBy) (\s@LicenseOperationFailure' {} a -> s {operationRequestedBy = a} :: LicenseOperationFailure)

-- | Amazon Resource Name (ARN) of the resource.
licenseOperationFailure_resourceArn :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_resourceArn = Lens.lens (\LicenseOperationFailure' {resourceArn} -> resourceArn) (\s@LicenseOperationFailure' {} a -> s {resourceArn = a} :: LicenseOperationFailure)

-- | Reserved.
licenseOperationFailure_metadataList :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe [Metadata])
licenseOperationFailure_metadataList = Lens.lens (\LicenseOperationFailure' {metadataList} -> metadataList) (\s@LicenseOperationFailure' {} a -> s {metadataList = a} :: LicenseOperationFailure) Prelude.. Lens.mapping Lens.coerced

-- | Name of the operation.
licenseOperationFailure_operationName :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_operationName = Lens.lens (\LicenseOperationFailure' {operationName} -> operationName) (\s@LicenseOperationFailure' {} a -> s {operationName = a} :: LicenseOperationFailure)

-- | Failure time.
licenseOperationFailure_failureTime :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.UTCTime)
licenseOperationFailure_failureTime = Lens.lens (\LicenseOperationFailure' {failureTime} -> failureTime) (\s@LicenseOperationFailure' {} a -> s {failureTime = a} :: LicenseOperationFailure) Prelude.. Lens.mapping Core._Time

-- | Error message.
licenseOperationFailure_errorMessage :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_errorMessage = Lens.lens (\LicenseOperationFailure' {errorMessage} -> errorMessage) (\s@LicenseOperationFailure' {} a -> s {errorMessage = a} :: LicenseOperationFailure)

-- | ID of the Amazon Web Services account that owns the resource.
licenseOperationFailure_resourceOwnerId :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_resourceOwnerId = Lens.lens (\LicenseOperationFailure' {resourceOwnerId} -> resourceOwnerId) (\s@LicenseOperationFailure' {} a -> s {resourceOwnerId = a} :: LicenseOperationFailure)

instance Core.FromJSON LicenseOperationFailure where
  parseJSON =
    Core.withObject
      "LicenseOperationFailure"
      ( \x ->
          LicenseOperationFailure'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "OperationRequestedBy")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "MetadataList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OperationName")
            Prelude.<*> (x Core..:? "FailureTime")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ResourceOwnerId")
      )

instance Prelude.Hashable LicenseOperationFailure

instance Prelude.NFData LicenseOperationFailure
