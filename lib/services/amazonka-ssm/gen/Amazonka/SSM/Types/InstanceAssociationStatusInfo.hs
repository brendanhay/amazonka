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
-- Module      : Amazonka.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceAssociationStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InstanceAssociationOutputUrl

-- | Status information about the association.
--
-- /See:/ 'newInstanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the association applied to the managed node.
    associationName :: Prelude.Maybe Prelude.Text,
    -- | The version of the association applied to the managed node.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | Detailed status information about the association.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The association document versions.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | An error code returned by the request to create the association.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The date the association ran.
    executionDate :: Prelude.Maybe Data.POSIX,
    -- | Summary information about association execution.
    executionSummary :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID where the association was created.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the association.
    name :: Prelude.Maybe Prelude.Text,
    -- | A URL for an S3 bucket where you want to store the results of this
    -- request.
    outputUrl :: Prelude.Maybe InstanceAssociationOutputUrl,
    -- | Status information about the association.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceAssociationStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'instanceAssociationStatusInfo_associationId' - The association ID.
--
-- 'associationName', 'instanceAssociationStatusInfo_associationName' - The name of the association applied to the managed node.
--
-- 'associationVersion', 'instanceAssociationStatusInfo_associationVersion' - The version of the association applied to the managed node.
--
-- 'detailedStatus', 'instanceAssociationStatusInfo_detailedStatus' - Detailed status information about the association.
--
-- 'documentVersion', 'instanceAssociationStatusInfo_documentVersion' - The association document versions.
--
-- 'errorCode', 'instanceAssociationStatusInfo_errorCode' - An error code returned by the request to create the association.
--
-- 'executionDate', 'instanceAssociationStatusInfo_executionDate' - The date the association ran.
--
-- 'executionSummary', 'instanceAssociationStatusInfo_executionSummary' - Summary information about association execution.
--
-- 'instanceId', 'instanceAssociationStatusInfo_instanceId' - The managed node ID where the association was created.
--
-- 'name', 'instanceAssociationStatusInfo_name' - The name of the association.
--
-- 'outputUrl', 'instanceAssociationStatusInfo_outputUrl' - A URL for an S3 bucket where you want to store the results of this
-- request.
--
-- 'status', 'instanceAssociationStatusInfo_status' - Status information about the association.
newInstanceAssociationStatusInfo ::
  InstanceAssociationStatusInfo
newInstanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { associationId =
        Prelude.Nothing,
      associationName = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      executionDate = Prelude.Nothing,
      executionSummary = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      name = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The association ID.
instanceAssociationStatusInfo_associationId :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationId = Lens.lens (\InstanceAssociationStatusInfo' {associationId} -> associationId) (\s@InstanceAssociationStatusInfo' {} a -> s {associationId = a} :: InstanceAssociationStatusInfo)

-- | The name of the association applied to the managed node.
instanceAssociationStatusInfo_associationName :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationName = Lens.lens (\InstanceAssociationStatusInfo' {associationName} -> associationName) (\s@InstanceAssociationStatusInfo' {} a -> s {associationName = a} :: InstanceAssociationStatusInfo)

-- | The version of the association applied to the managed node.
instanceAssociationStatusInfo_associationVersion :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationVersion = Lens.lens (\InstanceAssociationStatusInfo' {associationVersion} -> associationVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {associationVersion = a} :: InstanceAssociationStatusInfo)

-- | Detailed status information about the association.
instanceAssociationStatusInfo_detailedStatus :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_detailedStatus = Lens.lens (\InstanceAssociationStatusInfo' {detailedStatus} -> detailedStatus) (\s@InstanceAssociationStatusInfo' {} a -> s {detailedStatus = a} :: InstanceAssociationStatusInfo)

-- | The association document versions.
instanceAssociationStatusInfo_documentVersion :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_documentVersion = Lens.lens (\InstanceAssociationStatusInfo' {documentVersion} -> documentVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {documentVersion = a} :: InstanceAssociationStatusInfo)

-- | An error code returned by the request to create the association.
instanceAssociationStatusInfo_errorCode :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_errorCode = Lens.lens (\InstanceAssociationStatusInfo' {errorCode} -> errorCode) (\s@InstanceAssociationStatusInfo' {} a -> s {errorCode = a} :: InstanceAssociationStatusInfo)

-- | The date the association ran.
instanceAssociationStatusInfo_executionDate :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.UTCTime)
instanceAssociationStatusInfo_executionDate = Lens.lens (\InstanceAssociationStatusInfo' {executionDate} -> executionDate) (\s@InstanceAssociationStatusInfo' {} a -> s {executionDate = a} :: InstanceAssociationStatusInfo) Prelude.. Lens.mapping Data._Time

-- | Summary information about association execution.
instanceAssociationStatusInfo_executionSummary :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_executionSummary = Lens.lens (\InstanceAssociationStatusInfo' {executionSummary} -> executionSummary) (\s@InstanceAssociationStatusInfo' {} a -> s {executionSummary = a} :: InstanceAssociationStatusInfo)

-- | The managed node ID where the association was created.
instanceAssociationStatusInfo_instanceId :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_instanceId = Lens.lens (\InstanceAssociationStatusInfo' {instanceId} -> instanceId) (\s@InstanceAssociationStatusInfo' {} a -> s {instanceId = a} :: InstanceAssociationStatusInfo)

-- | The name of the association.
instanceAssociationStatusInfo_name :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_name = Lens.lens (\InstanceAssociationStatusInfo' {name} -> name) (\s@InstanceAssociationStatusInfo' {} a -> s {name = a} :: InstanceAssociationStatusInfo)

-- | A URL for an S3 bucket where you want to store the results of this
-- request.
instanceAssociationStatusInfo_outputUrl :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe InstanceAssociationOutputUrl)
instanceAssociationStatusInfo_outputUrl = Lens.lens (\InstanceAssociationStatusInfo' {outputUrl} -> outputUrl) (\s@InstanceAssociationStatusInfo' {} a -> s {outputUrl = a} :: InstanceAssociationStatusInfo)

-- | Status information about the association.
instanceAssociationStatusInfo_status :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_status = Lens.lens (\InstanceAssociationStatusInfo' {status} -> status) (\s@InstanceAssociationStatusInfo' {} a -> s {status = a} :: InstanceAssociationStatusInfo)

instance Data.FromJSON InstanceAssociationStatusInfo where
  parseJSON =
    Data.withObject
      "InstanceAssociationStatusInfo"
      ( \x ->
          InstanceAssociationStatusInfo'
            Prelude.<$> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "AssociationName")
            Prelude.<*> (x Data..:? "AssociationVersion")
            Prelude.<*> (x Data..:? "DetailedStatus")
            Prelude.<*> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ExecutionDate")
            Prelude.<*> (x Data..:? "ExecutionSummary")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutputUrl")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    InstanceAssociationStatusInfo
  where
  hashWithSalt _salt InstanceAssociationStatusInfo' {..} =
    _salt `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` executionDate
      `Prelude.hashWithSalt` executionSummary
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputUrl
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceAssociationStatusInfo where
  rnf InstanceAssociationStatusInfo' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf executionDate
      `Prelude.seq` Prelude.rnf executionSummary
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputUrl
      `Prelude.seq` Prelude.rnf status
