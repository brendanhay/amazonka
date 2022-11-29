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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.LFTagPolicyResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    expression :: Prelude.NonEmpty LFTag
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
  -- | 'expression'
  Prelude.NonEmpty LFTag ->
  LFTagPolicyResource
newLFTagPolicyResource pResourceType_ pExpression_ =
  LFTagPolicyResource'
    { catalogId = Prelude.Nothing,
      resourceType = pResourceType_,
      expression = Lens.coerced Lens.# pExpression_
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
lFTagPolicyResource_expression :: Lens.Lens' LFTagPolicyResource (Prelude.NonEmpty LFTag)
lFTagPolicyResource_expression = Lens.lens (\LFTagPolicyResource' {expression} -> expression) (\s@LFTagPolicyResource' {} a -> s {expression = a} :: LFTagPolicyResource) Prelude.. Lens.coerced

instance Core.FromJSON LFTagPolicyResource where
  parseJSON =
    Core.withObject
      "LFTagPolicyResource"
      ( \x ->
          LFTagPolicyResource'
            Prelude.<$> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..: "ResourceType")
            Prelude.<*> (x Core..: "Expression")
      )

instance Prelude.Hashable LFTagPolicyResource where
  hashWithSalt _salt LFTagPolicyResource' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` expression

instance Prelude.NFData LFTagPolicyResource where
  rnf LFTagPolicyResource' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf expression

instance Core.ToJSON LFTagPolicyResource where
  toJSON LFTagPolicyResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("ResourceType" Core..= resourceType),
            Prelude.Just ("Expression" Core..= expression)
          ]
      )
