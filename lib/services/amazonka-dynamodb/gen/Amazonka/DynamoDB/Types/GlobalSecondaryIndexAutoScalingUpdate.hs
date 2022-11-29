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
-- Module      : Amazonka.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings of a global secondary index for a
-- global table that will be modified.
--
-- /See:/ 'newGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data GlobalSecondaryIndexAutoScalingUpdate = GlobalSecondaryIndexAutoScalingUpdate'
  { provisionedWriteCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalSecondaryIndexAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedWriteCapacityAutoScalingUpdate', 'globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate' - Undocumented member.
--
-- 'indexName', 'globalSecondaryIndexAutoScalingUpdate_indexName' - The name of the global secondary index.
newGlobalSecondaryIndexAutoScalingUpdate ::
  GlobalSecondaryIndexAutoScalingUpdate
newGlobalSecondaryIndexAutoScalingUpdate =
  GlobalSecondaryIndexAutoScalingUpdate'
    { provisionedWriteCapacityAutoScalingUpdate =
        Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | Undocumented member.
globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
globalSecondaryIndexAutoScalingUpdate_provisionedWriteCapacityAutoScalingUpdate = Lens.lens (\GlobalSecondaryIndexAutoScalingUpdate' {provisionedWriteCapacityAutoScalingUpdate} -> provisionedWriteCapacityAutoScalingUpdate) (\s@GlobalSecondaryIndexAutoScalingUpdate' {} a -> s {provisionedWriteCapacityAutoScalingUpdate = a} :: GlobalSecondaryIndexAutoScalingUpdate)

-- | The name of the global secondary index.
globalSecondaryIndexAutoScalingUpdate_indexName :: Lens.Lens' GlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe Prelude.Text)
globalSecondaryIndexAutoScalingUpdate_indexName = Lens.lens (\GlobalSecondaryIndexAutoScalingUpdate' {indexName} -> indexName) (\s@GlobalSecondaryIndexAutoScalingUpdate' {} a -> s {indexName = a} :: GlobalSecondaryIndexAutoScalingUpdate)

instance
  Prelude.Hashable
    GlobalSecondaryIndexAutoScalingUpdate
  where
  hashWithSalt
    _salt
    GlobalSecondaryIndexAutoScalingUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` provisionedWriteCapacityAutoScalingUpdate
        `Prelude.hashWithSalt` indexName

instance
  Prelude.NFData
    GlobalSecondaryIndexAutoScalingUpdate
  where
  rnf GlobalSecondaryIndexAutoScalingUpdate' {..} =
    Prelude.rnf
      provisionedWriteCapacityAutoScalingUpdate
      `Prelude.seq` Prelude.rnf indexName

instance
  Core.ToJSON
    GlobalSecondaryIndexAutoScalingUpdate
  where
  toJSON GlobalSecondaryIndexAutoScalingUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "ProvisionedWriteCapacityAutoScalingUpdate"
                Core..=
            )
              Prelude.<$> provisionedWriteCapacityAutoScalingUpdate,
            ("IndexName" Core..=) Prelude.<$> indexName
          ]
      )
