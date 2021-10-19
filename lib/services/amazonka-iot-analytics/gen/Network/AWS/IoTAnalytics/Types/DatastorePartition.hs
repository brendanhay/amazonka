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
-- Module      : Network.AWS.IoTAnalytics.Types.DatastorePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastorePartition where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.Partition
import Network.AWS.IoTAnalytics.Types.TimestampPartition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A single dimension to partition a data store. The dimension must be an
-- @AttributePartition@ or a @TimestampPartition@.
--
-- /See:/ 'newDatastorePartition' smart constructor.
data DatastorePartition = DatastorePartition'
  { -- | A partition dimension defined by an @attributeName@.
    attributePartition :: Prelude.Maybe Partition,
    -- | A partition dimension defined by a timestamp attribute.
    timestampPartition :: Prelude.Maybe TimestampPartition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastorePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePartition', 'datastorePartition_attributePartition' - A partition dimension defined by an @attributeName@.
--
-- 'timestampPartition', 'datastorePartition_timestampPartition' - A partition dimension defined by a timestamp attribute.
newDatastorePartition ::
  DatastorePartition
newDatastorePartition =
  DatastorePartition'
    { attributePartition =
        Prelude.Nothing,
      timestampPartition = Prelude.Nothing
    }

-- | A partition dimension defined by an @attributeName@.
datastorePartition_attributePartition :: Lens.Lens' DatastorePartition (Prelude.Maybe Partition)
datastorePartition_attributePartition = Lens.lens (\DatastorePartition' {attributePartition} -> attributePartition) (\s@DatastorePartition' {} a -> s {attributePartition = a} :: DatastorePartition)

-- | A partition dimension defined by a timestamp attribute.
datastorePartition_timestampPartition :: Lens.Lens' DatastorePartition (Prelude.Maybe TimestampPartition)
datastorePartition_timestampPartition = Lens.lens (\DatastorePartition' {timestampPartition} -> timestampPartition) (\s@DatastorePartition' {} a -> s {timestampPartition = a} :: DatastorePartition)

instance Core.FromJSON DatastorePartition where
  parseJSON =
    Core.withObject
      "DatastorePartition"
      ( \x ->
          DatastorePartition'
            Prelude.<$> (x Core..:? "attributePartition")
            Prelude.<*> (x Core..:? "timestampPartition")
      )

instance Prelude.Hashable DatastorePartition

instance Prelude.NFData DatastorePartition

instance Core.ToJSON DatastorePartition where
  toJSON DatastorePartition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("attributePartition" Core..=)
              Prelude.<$> attributePartition,
            ("timestampPartition" Core..=)
              Prelude.<$> timestampPartition
          ]
      )
