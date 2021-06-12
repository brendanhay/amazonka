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
-- Module      : Network.AWS.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationStatusInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.InstanceAssociationOutputUrl

-- | Status information about the instance association.
--
-- /See:/ 'newInstanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { -- | Status information about the instance association.
    status :: Core.Maybe Core.Text,
    -- | The instance ID where the association was created.
    instanceId :: Core.Maybe Core.Text,
    -- | Detailed status information about the instance association.
    detailedStatus :: Core.Maybe Core.Text,
    -- | The name of the association.
    name :: Core.Maybe Core.Text,
    -- | A URL for an S3 bucket where you want to store the results of this
    -- request.
    outputUrl :: Core.Maybe InstanceAssociationOutputUrl,
    -- | The association ID.
    associationId :: Core.Maybe Core.Text,
    -- | The name of the association applied to the instance.
    associationName :: Core.Maybe Core.Text,
    -- | The date the instance association ran.
    executionDate :: Core.Maybe Core.POSIX,
    -- | Summary information about association execution.
    executionSummary :: Core.Maybe Core.Text,
    -- | The version of the association applied to the instance.
    associationVersion :: Core.Maybe Core.Text,
    -- | The association document versions.
    documentVersion :: Core.Maybe Core.Text,
    -- | An error code returned by the request to create the association.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceAssociationStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'instanceAssociationStatusInfo_status' - Status information about the instance association.
--
-- 'instanceId', 'instanceAssociationStatusInfo_instanceId' - The instance ID where the association was created.
--
-- 'detailedStatus', 'instanceAssociationStatusInfo_detailedStatus' - Detailed status information about the instance association.
--
-- 'name', 'instanceAssociationStatusInfo_name' - The name of the association.
--
-- 'outputUrl', 'instanceAssociationStatusInfo_outputUrl' - A URL for an S3 bucket where you want to store the results of this
-- request.
--
-- 'associationId', 'instanceAssociationStatusInfo_associationId' - The association ID.
--
-- 'associationName', 'instanceAssociationStatusInfo_associationName' - The name of the association applied to the instance.
--
-- 'executionDate', 'instanceAssociationStatusInfo_executionDate' - The date the instance association ran.
--
-- 'executionSummary', 'instanceAssociationStatusInfo_executionSummary' - Summary information about association execution.
--
-- 'associationVersion', 'instanceAssociationStatusInfo_associationVersion' - The version of the association applied to the instance.
--
-- 'documentVersion', 'instanceAssociationStatusInfo_documentVersion' - The association document versions.
--
-- 'errorCode', 'instanceAssociationStatusInfo_errorCode' - An error code returned by the request to create the association.
newInstanceAssociationStatusInfo ::
  InstanceAssociationStatusInfo
newInstanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { status =
        Core.Nothing,
      instanceId = Core.Nothing,
      detailedStatus = Core.Nothing,
      name = Core.Nothing,
      outputUrl = Core.Nothing,
      associationId = Core.Nothing,
      associationName = Core.Nothing,
      executionDate = Core.Nothing,
      executionSummary = Core.Nothing,
      associationVersion = Core.Nothing,
      documentVersion = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | Status information about the instance association.
instanceAssociationStatusInfo_status :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_status = Lens.lens (\InstanceAssociationStatusInfo' {status} -> status) (\s@InstanceAssociationStatusInfo' {} a -> s {status = a} :: InstanceAssociationStatusInfo)

-- | The instance ID where the association was created.
instanceAssociationStatusInfo_instanceId :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_instanceId = Lens.lens (\InstanceAssociationStatusInfo' {instanceId} -> instanceId) (\s@InstanceAssociationStatusInfo' {} a -> s {instanceId = a} :: InstanceAssociationStatusInfo)

-- | Detailed status information about the instance association.
instanceAssociationStatusInfo_detailedStatus :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_detailedStatus = Lens.lens (\InstanceAssociationStatusInfo' {detailedStatus} -> detailedStatus) (\s@InstanceAssociationStatusInfo' {} a -> s {detailedStatus = a} :: InstanceAssociationStatusInfo)

-- | The name of the association.
instanceAssociationStatusInfo_name :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_name = Lens.lens (\InstanceAssociationStatusInfo' {name} -> name) (\s@InstanceAssociationStatusInfo' {} a -> s {name = a} :: InstanceAssociationStatusInfo)

-- | A URL for an S3 bucket where you want to store the results of this
-- request.
instanceAssociationStatusInfo_outputUrl :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe InstanceAssociationOutputUrl)
instanceAssociationStatusInfo_outputUrl = Lens.lens (\InstanceAssociationStatusInfo' {outputUrl} -> outputUrl) (\s@InstanceAssociationStatusInfo' {} a -> s {outputUrl = a} :: InstanceAssociationStatusInfo)

-- | The association ID.
instanceAssociationStatusInfo_associationId :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_associationId = Lens.lens (\InstanceAssociationStatusInfo' {associationId} -> associationId) (\s@InstanceAssociationStatusInfo' {} a -> s {associationId = a} :: InstanceAssociationStatusInfo)

-- | The name of the association applied to the instance.
instanceAssociationStatusInfo_associationName :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_associationName = Lens.lens (\InstanceAssociationStatusInfo' {associationName} -> associationName) (\s@InstanceAssociationStatusInfo' {} a -> s {associationName = a} :: InstanceAssociationStatusInfo)

-- | The date the instance association ran.
instanceAssociationStatusInfo_executionDate :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.UTCTime)
instanceAssociationStatusInfo_executionDate = Lens.lens (\InstanceAssociationStatusInfo' {executionDate} -> executionDate) (\s@InstanceAssociationStatusInfo' {} a -> s {executionDate = a} :: InstanceAssociationStatusInfo) Core.. Lens.mapping Core._Time

-- | Summary information about association execution.
instanceAssociationStatusInfo_executionSummary :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_executionSummary = Lens.lens (\InstanceAssociationStatusInfo' {executionSummary} -> executionSummary) (\s@InstanceAssociationStatusInfo' {} a -> s {executionSummary = a} :: InstanceAssociationStatusInfo)

-- | The version of the association applied to the instance.
instanceAssociationStatusInfo_associationVersion :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_associationVersion = Lens.lens (\InstanceAssociationStatusInfo' {associationVersion} -> associationVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {associationVersion = a} :: InstanceAssociationStatusInfo)

-- | The association document versions.
instanceAssociationStatusInfo_documentVersion :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_documentVersion = Lens.lens (\InstanceAssociationStatusInfo' {documentVersion} -> documentVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {documentVersion = a} :: InstanceAssociationStatusInfo)

-- | An error code returned by the request to create the association.
instanceAssociationStatusInfo_errorCode :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.Text)
instanceAssociationStatusInfo_errorCode = Lens.lens (\InstanceAssociationStatusInfo' {errorCode} -> errorCode) (\s@InstanceAssociationStatusInfo' {} a -> s {errorCode = a} :: InstanceAssociationStatusInfo)

instance Core.FromJSON InstanceAssociationStatusInfo where
  parseJSON =
    Core.withObject
      "InstanceAssociationStatusInfo"
      ( \x ->
          InstanceAssociationStatusInfo'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "DetailedStatus")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "OutputUrl")
            Core.<*> (x Core..:? "AssociationId")
            Core.<*> (x Core..:? "AssociationName")
            Core.<*> (x Core..:? "ExecutionDate")
            Core.<*> (x Core..:? "ExecutionSummary")
            Core.<*> (x Core..:? "AssociationVersion")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable InstanceAssociationStatusInfo

instance Core.NFData InstanceAssociationStatusInfo
