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
-- Module      : Amazonka.LakeFormation.Types.LFTagPolicyResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTagPolicyResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.LFTag
import Amazonka.LakeFormation.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | A structure containing a list of LF-tag conditions that apply to a
-- resource\'s LF-tag policy.
--
-- /See:/ 'newLFTagPolicyResource' smart constructor.
data LFTagPolicyResource = LFTagPolicyResource'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The resource type for which the LF-tag policy applies.
    resourceType :: ResourceType,
    -- | A list of LF-tag conditions that apply to the resource\'s LF-tag policy.
    expression :: [LFTag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTagPolicyResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'lFTagPolicyResource_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'resourceType', 'lFTagPolicyResource_resourceType' - The resource type for which the LF-tag policy applies.
--
-- 'expression', 'lFTagPolicyResource_expression' - A list of LF-tag conditions that apply to the resource\'s LF-tag policy.
newLFTagPolicyResource ::
  -- | 'resourceType'
  ResourceType ->
  LFTagPolicyResource
newLFTagPolicyResource pResourceType_ =
  LFTagPolicyResource'
    { catalogId = Prelude.Nothing,
      resourceType = pResourceType_,
      expression = Prelude.mempty
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
lFTagPolicyResource_catalogId :: Lens.Lens' LFTagPolicyResource (Prelude.Maybe Prelude.Text)
lFTagPolicyResource_catalogId = Lens.lens (\LFTagPolicyResource' {catalogId} -> catalogId) (\s@LFTagPolicyResource' {} a -> s {catalogId = a} :: LFTagPolicyResource)

-- | The resource type for which the LF-tag policy applies.
lFTagPolicyResource_resourceType :: Lens.Lens' LFTagPolicyResource ResourceType
lFTagPolicyResource_resourceType = Lens.lens (\LFTagPolicyResource' {resourceType} -> resourceType) (\s@LFTagPolicyResource' {} a -> s {resourceType = a} :: LFTagPolicyResource)

-- | A list of LF-tag conditions that apply to the resource\'s LF-tag policy.
lFTagPolicyResource_expression :: Lens.Lens' LFTagPolicyResource [LFTag]
lFTagPolicyResource_expression = Lens.lens (\LFTagPolicyResource' {expression} -> expression) (\s@LFTagPolicyResource' {} a -> s {expression = a} :: LFTagPolicyResource) Prelude.. Lens.coerced

instance Data.FromJSON LFTagPolicyResource where
  parseJSON =
    Data.withObject
      "LFTagPolicyResource"
      ( \x ->
          LFTagPolicyResource'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..: "ResourceType")
            Prelude.<*> (x Data..:? "Expression" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LFTagPolicyResource where
  hashWithSalt _salt LFTagPolicyResource' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` expression

instance Prelude.NFData LFTagPolicyResource where
  rnf LFTagPolicyResource' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON LFTagPolicyResource where
  toJSON LFTagPolicyResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
