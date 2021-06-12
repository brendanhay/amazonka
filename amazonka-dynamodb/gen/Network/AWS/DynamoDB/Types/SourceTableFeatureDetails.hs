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
-- Module      : Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SourceTableFeatureDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import qualified Network.AWS.Lens as Lens

-- | Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
--
-- /See:/ 'newSourceTableFeatureDetails' smart constructor.
data SourceTableFeatureDetails = SourceTableFeatureDetails'
  { -- | Represents the LSI properties for the table when the backup was created.
    -- It includes the IndexName, KeySchema and Projection for the LSIs on the
    -- table at the time of backup.
    localSecondaryIndexes :: Core.Maybe [LocalSecondaryIndexInfo],
    -- | Represents the GSI properties for the table when the backup was created.
    -- It includes the IndexName, KeySchema, Projection, and
    -- ProvisionedThroughput for the GSIs on the table at the time of backup.
    globalSecondaryIndexes :: Core.Maybe [GlobalSecondaryIndexInfo],
    -- | Time to Live settings on the table when the backup was created.
    timeToLiveDescription :: Core.Maybe TimeToLiveDescription,
    -- | The description of the server-side encryption status on the table when
    -- the backup was created.
    sSEDescription :: Core.Maybe SSEDescription,
    -- | Stream settings on the table when the backup was created.
    streamDescription :: Core.Maybe StreamSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceTableFeatureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localSecondaryIndexes', 'sourceTableFeatureDetails_localSecondaryIndexes' - Represents the LSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema and Projection for the LSIs on the
-- table at the time of backup.
--
-- 'globalSecondaryIndexes', 'sourceTableFeatureDetails_globalSecondaryIndexes' - Represents the GSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema, Projection, and
-- ProvisionedThroughput for the GSIs on the table at the time of backup.
--
-- 'timeToLiveDescription', 'sourceTableFeatureDetails_timeToLiveDescription' - Time to Live settings on the table when the backup was created.
--
-- 'sSEDescription', 'sourceTableFeatureDetails_sSEDescription' - The description of the server-side encryption status on the table when
-- the backup was created.
--
-- 'streamDescription', 'sourceTableFeatureDetails_streamDescription' - Stream settings on the table when the backup was created.
newSourceTableFeatureDetails ::
  SourceTableFeatureDetails
newSourceTableFeatureDetails =
  SourceTableFeatureDetails'
    { localSecondaryIndexes =
        Core.Nothing,
      globalSecondaryIndexes = Core.Nothing,
      timeToLiveDescription = Core.Nothing,
      sSEDescription = Core.Nothing,
      streamDescription = Core.Nothing
    }

-- | Represents the LSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema and Projection for the LSIs on the
-- table at the time of backup.
sourceTableFeatureDetails_localSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe [LocalSecondaryIndexInfo])
sourceTableFeatureDetails_localSecondaryIndexes = Lens.lens (\SourceTableFeatureDetails' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@SourceTableFeatureDetails' {} a -> s {localSecondaryIndexes = a} :: SourceTableFeatureDetails) Core.. Lens.mapping Lens._Coerce

-- | Represents the GSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema, Projection, and
-- ProvisionedThroughput for the GSIs on the table at the time of backup.
sourceTableFeatureDetails_globalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe [GlobalSecondaryIndexInfo])
sourceTableFeatureDetails_globalSecondaryIndexes = Lens.lens (\SourceTableFeatureDetails' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@SourceTableFeatureDetails' {} a -> s {globalSecondaryIndexes = a} :: SourceTableFeatureDetails) Core.. Lens.mapping Lens._Coerce

-- | Time to Live settings on the table when the backup was created.
sourceTableFeatureDetails_timeToLiveDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe TimeToLiveDescription)
sourceTableFeatureDetails_timeToLiveDescription = Lens.lens (\SourceTableFeatureDetails' {timeToLiveDescription} -> timeToLiveDescription) (\s@SourceTableFeatureDetails' {} a -> s {timeToLiveDescription = a} :: SourceTableFeatureDetails)

-- | The description of the server-side encryption status on the table when
-- the backup was created.
sourceTableFeatureDetails_sSEDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe SSEDescription)
sourceTableFeatureDetails_sSEDescription = Lens.lens (\SourceTableFeatureDetails' {sSEDescription} -> sSEDescription) (\s@SourceTableFeatureDetails' {} a -> s {sSEDescription = a} :: SourceTableFeatureDetails)

-- | Stream settings on the table when the backup was created.
sourceTableFeatureDetails_streamDescription :: Lens.Lens' SourceTableFeatureDetails (Core.Maybe StreamSpecification)
sourceTableFeatureDetails_streamDescription = Lens.lens (\SourceTableFeatureDetails' {streamDescription} -> streamDescription) (\s@SourceTableFeatureDetails' {} a -> s {streamDescription = a} :: SourceTableFeatureDetails)

instance Core.FromJSON SourceTableFeatureDetails where
  parseJSON =
    Core.withObject
      "SourceTableFeatureDetails"
      ( \x ->
          SourceTableFeatureDetails'
            Core.<$> ( x Core..:? "LocalSecondaryIndexes"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "GlobalSecondaryIndexes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "TimeToLiveDescription")
            Core.<*> (x Core..:? "SSEDescription")
            Core.<*> (x Core..:? "StreamDescription")
      )

instance Core.Hashable SourceTableFeatureDetails

instance Core.NFData SourceTableFeatureDetails
