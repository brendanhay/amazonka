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
-- Module      : Amazonka.DynamoDB.Types.SourceTableFeatureDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SourceTableFeatureDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexInfo
import Amazonka.DynamoDB.Types.LocalSecondaryIndexInfo
import Amazonka.DynamoDB.Types.SSEDescription
import Amazonka.DynamoDB.Types.StreamSpecification
import Amazonka.DynamoDB.Types.TimeToLiveDescription
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the features enabled on the table when the
-- backup was created. For example, LSIs, GSIs, streams, TTL.
--
-- /See:/ 'newSourceTableFeatureDetails' smart constructor.
data SourceTableFeatureDetails = SourceTableFeatureDetails'
  { -- | Represents the GSI properties for the table when the backup was created.
    -- It includes the IndexName, KeySchema, Projection, and
    -- ProvisionedThroughput for the GSIs on the table at the time of backup.
    globalSecondaryIndexes :: Prelude.Maybe [GlobalSecondaryIndexInfo],
    -- | Represents the LSI properties for the table when the backup was created.
    -- It includes the IndexName, KeySchema and Projection for the LSIs on the
    -- table at the time of backup.
    localSecondaryIndexes :: Prelude.Maybe [LocalSecondaryIndexInfo],
    -- | The description of the server-side encryption status on the table when
    -- the backup was created.
    sSEDescription :: Prelude.Maybe SSEDescription,
    -- | Stream settings on the table when the backup was created.
    streamDescription :: Prelude.Maybe StreamSpecification,
    -- | Time to Live settings on the table when the backup was created.
    timeToLiveDescription :: Prelude.Maybe TimeToLiveDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceTableFeatureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalSecondaryIndexes', 'sourceTableFeatureDetails_globalSecondaryIndexes' - Represents the GSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema, Projection, and
-- ProvisionedThroughput for the GSIs on the table at the time of backup.
--
-- 'localSecondaryIndexes', 'sourceTableFeatureDetails_localSecondaryIndexes' - Represents the LSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema and Projection for the LSIs on the
-- table at the time of backup.
--
-- 'sSEDescription', 'sourceTableFeatureDetails_sSEDescription' - The description of the server-side encryption status on the table when
-- the backup was created.
--
-- 'streamDescription', 'sourceTableFeatureDetails_streamDescription' - Stream settings on the table when the backup was created.
--
-- 'timeToLiveDescription', 'sourceTableFeatureDetails_timeToLiveDescription' - Time to Live settings on the table when the backup was created.
newSourceTableFeatureDetails ::
  SourceTableFeatureDetails
newSourceTableFeatureDetails =
  SourceTableFeatureDetails'
    { globalSecondaryIndexes =
        Prelude.Nothing,
      localSecondaryIndexes = Prelude.Nothing,
      sSEDescription = Prelude.Nothing,
      streamDescription = Prelude.Nothing,
      timeToLiveDescription = Prelude.Nothing
    }

-- | Represents the GSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema, Projection, and
-- ProvisionedThroughput for the GSIs on the table at the time of backup.
sourceTableFeatureDetails_globalSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Prelude.Maybe [GlobalSecondaryIndexInfo])
sourceTableFeatureDetails_globalSecondaryIndexes = Lens.lens (\SourceTableFeatureDetails' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@SourceTableFeatureDetails' {} a -> s {globalSecondaryIndexes = a} :: SourceTableFeatureDetails) Prelude.. Lens.mapping Lens.coerced

-- | Represents the LSI properties for the table when the backup was created.
-- It includes the IndexName, KeySchema and Projection for the LSIs on the
-- table at the time of backup.
sourceTableFeatureDetails_localSecondaryIndexes :: Lens.Lens' SourceTableFeatureDetails (Prelude.Maybe [LocalSecondaryIndexInfo])
sourceTableFeatureDetails_localSecondaryIndexes = Lens.lens (\SourceTableFeatureDetails' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@SourceTableFeatureDetails' {} a -> s {localSecondaryIndexes = a} :: SourceTableFeatureDetails) Prelude.. Lens.mapping Lens.coerced

-- | The description of the server-side encryption status on the table when
-- the backup was created.
sourceTableFeatureDetails_sSEDescription :: Lens.Lens' SourceTableFeatureDetails (Prelude.Maybe SSEDescription)
sourceTableFeatureDetails_sSEDescription = Lens.lens (\SourceTableFeatureDetails' {sSEDescription} -> sSEDescription) (\s@SourceTableFeatureDetails' {} a -> s {sSEDescription = a} :: SourceTableFeatureDetails)

-- | Stream settings on the table when the backup was created.
sourceTableFeatureDetails_streamDescription :: Lens.Lens' SourceTableFeatureDetails (Prelude.Maybe StreamSpecification)
sourceTableFeatureDetails_streamDescription = Lens.lens (\SourceTableFeatureDetails' {streamDescription} -> streamDescription) (\s@SourceTableFeatureDetails' {} a -> s {streamDescription = a} :: SourceTableFeatureDetails)

-- | Time to Live settings on the table when the backup was created.
sourceTableFeatureDetails_timeToLiveDescription :: Lens.Lens' SourceTableFeatureDetails (Prelude.Maybe TimeToLiveDescription)
sourceTableFeatureDetails_timeToLiveDescription = Lens.lens (\SourceTableFeatureDetails' {timeToLiveDescription} -> timeToLiveDescription) (\s@SourceTableFeatureDetails' {} a -> s {timeToLiveDescription = a} :: SourceTableFeatureDetails)

instance Data.FromJSON SourceTableFeatureDetails where
  parseJSON =
    Data.withObject
      "SourceTableFeatureDetails"
      ( \x ->
          SourceTableFeatureDetails'
            Prelude.<$> ( x Data..:? "GlobalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "LocalSecondaryIndexes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SSEDescription")
            Prelude.<*> (x Data..:? "StreamDescription")
            Prelude.<*> (x Data..:? "TimeToLiveDescription")
      )

instance Prelude.Hashable SourceTableFeatureDetails where
  hashWithSalt _salt SourceTableFeatureDetails' {..} =
    _salt `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` localSecondaryIndexes
      `Prelude.hashWithSalt` sSEDescription
      `Prelude.hashWithSalt` streamDescription
      `Prelude.hashWithSalt` timeToLiveDescription

instance Prelude.NFData SourceTableFeatureDetails where
  rnf SourceTableFeatureDetails' {..} =
    Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf localSecondaryIndexes
      `Prelude.seq` Prelude.rnf sSEDescription
      `Prelude.seq` Prelude.rnf streamDescription
      `Prelude.seq` Prelude.rnf timeToLiveDescription
