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
-- Module      : Amazonka.MacieV2.Types.CriterionAdditionalProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CriterionAdditionalProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the operator to use in a property-based condition that filters
-- the results of a query for findings. For detailed information and
-- examples of each operator, see
-- <https://docs.aws.amazon.com/macie/latest/user/findings-filter-basics.html Fundamentals of filtering findings>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newCriterionAdditionalProperties' smart constructor.
data CriterionAdditionalProperties = CriterionAdditionalProperties'
  { -- | The value for the property doesn\'t match (doesn\'t equal) the specified
    -- value. If you specify multiple values, Macie uses OR logic to join the
    -- values.
    neq :: Prelude.Maybe [Prelude.Text],
    -- | The value for the property is less than or equal to the specified value.
    lte :: Prelude.Maybe Prelude.Integer,
    -- | The value for the property is less than the specified value.
    lt :: Prelude.Maybe Prelude.Integer,
    -- | The value for the property is greater than or equal to the specified
    -- value.
    gte :: Prelude.Maybe Prelude.Integer,
    -- | The value for the property matches (equals) the specified value. If you
    -- specify multiple values, Macie uses OR logic to join the values.
    eq :: Prelude.Maybe [Prelude.Text],
    -- | The value for the property is greater than the specified value.
    gt :: Prelude.Maybe Prelude.Integer,
    -- | The value for the property exclusively matches (equals an exact match
    -- for) all the specified values. If you specify multiple values, Amazon
    -- Macie uses AND logic to join the values.
    --
    -- You can use this operator with the following properties:
    -- customDataIdentifiers.detections.arn,
    -- customDataIdentifiers.detections.name,
    -- resourcesAffected.s3Bucket.tags.key,
    -- resourcesAffected.s3Bucket.tags.value,
    -- resourcesAffected.s3Object.tags.key,
    -- resourcesAffected.s3Object.tags.value, sensitiveData.category, and
    -- sensitiveData.detections.type.
    eqExactMatch :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CriterionAdditionalProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'neq', 'criterionAdditionalProperties_neq' - The value for the property doesn\'t match (doesn\'t equal) the specified
-- value. If you specify multiple values, Macie uses OR logic to join the
-- values.
--
-- 'lte', 'criterionAdditionalProperties_lte' - The value for the property is less than or equal to the specified value.
--
-- 'lt', 'criterionAdditionalProperties_lt' - The value for the property is less than the specified value.
--
-- 'gte', 'criterionAdditionalProperties_gte' - The value for the property is greater than or equal to the specified
-- value.
--
-- 'eq', 'criterionAdditionalProperties_eq' - The value for the property matches (equals) the specified value. If you
-- specify multiple values, Macie uses OR logic to join the values.
--
-- 'gt', 'criterionAdditionalProperties_gt' - The value for the property is greater than the specified value.
--
-- 'eqExactMatch', 'criterionAdditionalProperties_eqExactMatch' - The value for the property exclusively matches (equals an exact match
-- for) all the specified values. If you specify multiple values, Amazon
-- Macie uses AND logic to join the values.
--
-- You can use this operator with the following properties:
-- customDataIdentifiers.detections.arn,
-- customDataIdentifiers.detections.name,
-- resourcesAffected.s3Bucket.tags.key,
-- resourcesAffected.s3Bucket.tags.value,
-- resourcesAffected.s3Object.tags.key,
-- resourcesAffected.s3Object.tags.value, sensitiveData.category, and
-- sensitiveData.detections.type.
newCriterionAdditionalProperties ::
  CriterionAdditionalProperties
newCriterionAdditionalProperties =
  CriterionAdditionalProperties'
    { neq =
        Prelude.Nothing,
      lte = Prelude.Nothing,
      lt = Prelude.Nothing,
      gte = Prelude.Nothing,
      eq = Prelude.Nothing,
      gt = Prelude.Nothing,
      eqExactMatch = Prelude.Nothing
    }

-- | The value for the property doesn\'t match (doesn\'t equal) the specified
-- value. If you specify multiple values, Macie uses OR logic to join the
-- values.
criterionAdditionalProperties_neq :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe [Prelude.Text])
criterionAdditionalProperties_neq = Lens.lens (\CriterionAdditionalProperties' {neq} -> neq) (\s@CriterionAdditionalProperties' {} a -> s {neq = a} :: CriterionAdditionalProperties) Prelude.. Lens.mapping Lens.coerced

-- | The value for the property is less than or equal to the specified value.
criterionAdditionalProperties_lte :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe Prelude.Integer)
criterionAdditionalProperties_lte = Lens.lens (\CriterionAdditionalProperties' {lte} -> lte) (\s@CriterionAdditionalProperties' {} a -> s {lte = a} :: CriterionAdditionalProperties)

-- | The value for the property is less than the specified value.
criterionAdditionalProperties_lt :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe Prelude.Integer)
criterionAdditionalProperties_lt = Lens.lens (\CriterionAdditionalProperties' {lt} -> lt) (\s@CriterionAdditionalProperties' {} a -> s {lt = a} :: CriterionAdditionalProperties)

-- | The value for the property is greater than or equal to the specified
-- value.
criterionAdditionalProperties_gte :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe Prelude.Integer)
criterionAdditionalProperties_gte = Lens.lens (\CriterionAdditionalProperties' {gte} -> gte) (\s@CriterionAdditionalProperties' {} a -> s {gte = a} :: CriterionAdditionalProperties)

-- | The value for the property matches (equals) the specified value. If you
-- specify multiple values, Macie uses OR logic to join the values.
criterionAdditionalProperties_eq :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe [Prelude.Text])
criterionAdditionalProperties_eq = Lens.lens (\CriterionAdditionalProperties' {eq} -> eq) (\s@CriterionAdditionalProperties' {} a -> s {eq = a} :: CriterionAdditionalProperties) Prelude.. Lens.mapping Lens.coerced

-- | The value for the property is greater than the specified value.
criterionAdditionalProperties_gt :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe Prelude.Integer)
criterionAdditionalProperties_gt = Lens.lens (\CriterionAdditionalProperties' {gt} -> gt) (\s@CriterionAdditionalProperties' {} a -> s {gt = a} :: CriterionAdditionalProperties)

-- | The value for the property exclusively matches (equals an exact match
-- for) all the specified values. If you specify multiple values, Amazon
-- Macie uses AND logic to join the values.
--
-- You can use this operator with the following properties:
-- customDataIdentifiers.detections.arn,
-- customDataIdentifiers.detections.name,
-- resourcesAffected.s3Bucket.tags.key,
-- resourcesAffected.s3Bucket.tags.value,
-- resourcesAffected.s3Object.tags.key,
-- resourcesAffected.s3Object.tags.value, sensitiveData.category, and
-- sensitiveData.detections.type.
criterionAdditionalProperties_eqExactMatch :: Lens.Lens' CriterionAdditionalProperties (Prelude.Maybe [Prelude.Text])
criterionAdditionalProperties_eqExactMatch = Lens.lens (\CriterionAdditionalProperties' {eqExactMatch} -> eqExactMatch) (\s@CriterionAdditionalProperties' {} a -> s {eqExactMatch = a} :: CriterionAdditionalProperties) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CriterionAdditionalProperties where
  parseJSON =
    Core.withObject
      "CriterionAdditionalProperties"
      ( \x ->
          CriterionAdditionalProperties'
            Prelude.<$> (x Core..:? "neq" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "lte")
            Prelude.<*> (x Core..:? "lt")
            Prelude.<*> (x Core..:? "gte")
            Prelude.<*> (x Core..:? "eq" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "gt")
            Prelude.<*> (x Core..:? "eqExactMatch" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    CriterionAdditionalProperties
  where
  hashWithSalt _salt CriterionAdditionalProperties' {..} =
    _salt `Prelude.hashWithSalt` neq
      `Prelude.hashWithSalt` lte
      `Prelude.hashWithSalt` lt
      `Prelude.hashWithSalt` gte
      `Prelude.hashWithSalt` eq
      `Prelude.hashWithSalt` gt
      `Prelude.hashWithSalt` eqExactMatch

instance Prelude.NFData CriterionAdditionalProperties where
  rnf CriterionAdditionalProperties' {..} =
    Prelude.rnf neq
      `Prelude.seq` Prelude.rnf lte
      `Prelude.seq` Prelude.rnf lt
      `Prelude.seq` Prelude.rnf gte
      `Prelude.seq` Prelude.rnf eq
      `Prelude.seq` Prelude.rnf gt
      `Prelude.seq` Prelude.rnf eqExactMatch

instance Core.ToJSON CriterionAdditionalProperties where
  toJSON CriterionAdditionalProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("neq" Core..=) Prelude.<$> neq,
            ("lte" Core..=) Prelude.<$> lte,
            ("lt" Core..=) Prelude.<$> lt,
            ("gte" Core..=) Prelude.<$> gte,
            ("eq" Core..=) Prelude.<$> eq,
            ("gt" Core..=) Prelude.<$> gt,
            ("eqExactMatch" Core..=) Prelude.<$> eqExactMatch
          ]
      )
