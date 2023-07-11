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
-- Module      : Amazonka.CostExplorer.Types.CostAllocationTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostAllocationTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostAllocationTagStatus
import Amazonka.CostExplorer.Types.CostAllocationTagType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The cost allocation tag structure. This includes detailed metadata for
-- the @CostAllocationTag@ object.
--
-- /See:/ 'newCostAllocationTag' smart constructor.
data CostAllocationTag = CostAllocationTag'
  { -- | The key for the cost allocation tag.
    tagKey :: Prelude.Text,
    -- | The type of cost allocation tag. You can use @AWSGenerated@ or
    -- @UserDefined@ type tags. @AWSGenerated@ type tags are tags that Amazon
    -- Web Services defines and applies to support Amazon Web Services
    -- resources for cost allocation purposes. @UserDefined@ type tags are tags
    -- that you define, create, and apply to resources.
    type' :: CostAllocationTagType,
    -- | The status of a cost allocation tag.
    status :: CostAllocationTagStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostAllocationTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'costAllocationTag_tagKey' - The key for the cost allocation tag.
--
-- 'type'', 'costAllocationTag_type' - The type of cost allocation tag. You can use @AWSGenerated@ or
-- @UserDefined@ type tags. @AWSGenerated@ type tags are tags that Amazon
-- Web Services defines and applies to support Amazon Web Services
-- resources for cost allocation purposes. @UserDefined@ type tags are tags
-- that you define, create, and apply to resources.
--
-- 'status', 'costAllocationTag_status' - The status of a cost allocation tag.
newCostAllocationTag ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'type''
  CostAllocationTagType ->
  -- | 'status'
  CostAllocationTagStatus ->
  CostAllocationTag
newCostAllocationTag pTagKey_ pType_ pStatus_ =
  CostAllocationTag'
    { tagKey = pTagKey_,
      type' = pType_,
      status = pStatus_
    }

-- | The key for the cost allocation tag.
costAllocationTag_tagKey :: Lens.Lens' CostAllocationTag Prelude.Text
costAllocationTag_tagKey = Lens.lens (\CostAllocationTag' {tagKey} -> tagKey) (\s@CostAllocationTag' {} a -> s {tagKey = a} :: CostAllocationTag)

-- | The type of cost allocation tag. You can use @AWSGenerated@ or
-- @UserDefined@ type tags. @AWSGenerated@ type tags are tags that Amazon
-- Web Services defines and applies to support Amazon Web Services
-- resources for cost allocation purposes. @UserDefined@ type tags are tags
-- that you define, create, and apply to resources.
costAllocationTag_type :: Lens.Lens' CostAllocationTag CostAllocationTagType
costAllocationTag_type = Lens.lens (\CostAllocationTag' {type'} -> type') (\s@CostAllocationTag' {} a -> s {type' = a} :: CostAllocationTag)

-- | The status of a cost allocation tag.
costAllocationTag_status :: Lens.Lens' CostAllocationTag CostAllocationTagStatus
costAllocationTag_status = Lens.lens (\CostAllocationTag' {status} -> status) (\s@CostAllocationTag' {} a -> s {status = a} :: CostAllocationTag)

instance Data.FromJSON CostAllocationTag where
  parseJSON =
    Data.withObject
      "CostAllocationTag"
      ( \x ->
          CostAllocationTag'
            Prelude.<$> (x Data..: "TagKey")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable CostAllocationTag where
  hashWithSalt _salt CostAllocationTag' {..} =
    _salt
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` status

instance Prelude.NFData CostAllocationTag where
  rnf CostAllocationTag' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
