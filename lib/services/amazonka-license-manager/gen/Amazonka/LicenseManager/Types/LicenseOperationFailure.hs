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
-- Module      : Amazonka.LicenseManager.Types.LicenseOperationFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseOperationFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.Metadata
import Amazonka.LicenseManager.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the failure of a license operation.
--
-- /See:/ 'newLicenseOperationFailure' smart constructor.
data LicenseOperationFailure = LicenseOperationFailure'
  { -- | Error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Failure time.
    failureTime :: Prelude.Maybe Data.POSIX,
    -- | Reserved.
    metadataList :: Prelude.Maybe [Metadata],
    -- | Name of the operation.
    operationName :: Prelude.Maybe Prelude.Text,
    -- | The requester is \"License Manager Automated Discovery\".
    operationRequestedBy :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | ID of the Amazon Web Services account that owns the resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Resource type.
    resourceType :: Prelude.Maybe ResourceType
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
-- 'errorMessage', 'licenseOperationFailure_errorMessage' - Error message.
--
-- 'failureTime', 'licenseOperationFailure_failureTime' - Failure time.
--
-- 'metadataList', 'licenseOperationFailure_metadataList' - Reserved.
--
-- 'operationName', 'licenseOperationFailure_operationName' - Name of the operation.
--
-- 'operationRequestedBy', 'licenseOperationFailure_operationRequestedBy' - The requester is \"License Manager Automated Discovery\".
--
-- 'resourceArn', 'licenseOperationFailure_resourceArn' - Amazon Resource Name (ARN) of the resource.
--
-- 'resourceOwnerId', 'licenseOperationFailure_resourceOwnerId' - ID of the Amazon Web Services account that owns the resource.
--
-- 'resourceType', 'licenseOperationFailure_resourceType' - Resource type.
newLicenseOperationFailure ::
  LicenseOperationFailure
newLicenseOperationFailure =
  LicenseOperationFailure'
    { errorMessage =
        Prelude.Nothing,
      failureTime = Prelude.Nothing,
      metadataList = Prelude.Nothing,
      operationName = Prelude.Nothing,
      operationRequestedBy = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Error message.
licenseOperationFailure_errorMessage :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_errorMessage = Lens.lens (\LicenseOperationFailure' {errorMessage} -> errorMessage) (\s@LicenseOperationFailure' {} a -> s {errorMessage = a} :: LicenseOperationFailure)

-- | Failure time.
licenseOperationFailure_failureTime :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.UTCTime)
licenseOperationFailure_failureTime = Lens.lens (\LicenseOperationFailure' {failureTime} -> failureTime) (\s@LicenseOperationFailure' {} a -> s {failureTime = a} :: LicenseOperationFailure) Prelude.. Lens.mapping Data._Time

-- | Reserved.
licenseOperationFailure_metadataList :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe [Metadata])
licenseOperationFailure_metadataList = Lens.lens (\LicenseOperationFailure' {metadataList} -> metadataList) (\s@LicenseOperationFailure' {} a -> s {metadataList = a} :: LicenseOperationFailure) Prelude.. Lens.mapping Lens.coerced

-- | Name of the operation.
licenseOperationFailure_operationName :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_operationName = Lens.lens (\LicenseOperationFailure' {operationName} -> operationName) (\s@LicenseOperationFailure' {} a -> s {operationName = a} :: LicenseOperationFailure)

-- | The requester is \"License Manager Automated Discovery\".
licenseOperationFailure_operationRequestedBy :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_operationRequestedBy = Lens.lens (\LicenseOperationFailure' {operationRequestedBy} -> operationRequestedBy) (\s@LicenseOperationFailure' {} a -> s {operationRequestedBy = a} :: LicenseOperationFailure)

-- | Amazon Resource Name (ARN) of the resource.
licenseOperationFailure_resourceArn :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_resourceArn = Lens.lens (\LicenseOperationFailure' {resourceArn} -> resourceArn) (\s@LicenseOperationFailure' {} a -> s {resourceArn = a} :: LicenseOperationFailure)

-- | ID of the Amazon Web Services account that owns the resource.
licenseOperationFailure_resourceOwnerId :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe Prelude.Text)
licenseOperationFailure_resourceOwnerId = Lens.lens (\LicenseOperationFailure' {resourceOwnerId} -> resourceOwnerId) (\s@LicenseOperationFailure' {} a -> s {resourceOwnerId = a} :: LicenseOperationFailure)

-- | Resource type.
licenseOperationFailure_resourceType :: Lens.Lens' LicenseOperationFailure (Prelude.Maybe ResourceType)
licenseOperationFailure_resourceType = Lens.lens (\LicenseOperationFailure' {resourceType} -> resourceType) (\s@LicenseOperationFailure' {} a -> s {resourceType = a} :: LicenseOperationFailure)

instance Data.FromJSON LicenseOperationFailure where
  parseJSON =
    Data.withObject
      "LicenseOperationFailure"
      ( \x ->
          LicenseOperationFailure'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "FailureTime")
            Prelude.<*> (x Data..:? "MetadataList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OperationName")
            Prelude.<*> (x Data..:? "OperationRequestedBy")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "ResourceOwnerId")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance Prelude.Hashable LicenseOperationFailure where
  hashWithSalt _salt LicenseOperationFailure' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` failureTime
      `Prelude.hashWithSalt` metadataList
      `Prelude.hashWithSalt` operationName
      `Prelude.hashWithSalt` operationRequestedBy
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceOwnerId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData LicenseOperationFailure where
  rnf LicenseOperationFailure' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf failureTime
      `Prelude.seq` Prelude.rnf metadataList
      `Prelude.seq` Prelude.rnf operationName
      `Prelude.seq` Prelude.rnf operationRequestedBy
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwnerId
      `Prelude.seq` Prelude.rnf resourceType
