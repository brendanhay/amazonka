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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionPlacementConstraintsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A placement constraint object to use for tasks.
--
-- /See:/ 'newAwsEcsTaskDefinitionPlacementConstraintsDetails' smart constructor.
data AwsEcsTaskDefinitionPlacementConstraintsDetails = AwsEcsTaskDefinitionPlacementConstraintsDetails'
  { -- | The type of constraint.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A cluster query language expression to apply to the constraint.
    expression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionPlacementConstraintsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEcsTaskDefinitionPlacementConstraintsDetails_type' - The type of constraint.
--
-- 'expression', 'awsEcsTaskDefinitionPlacementConstraintsDetails_expression' - A cluster query language expression to apply to the constraint.
newAwsEcsTaskDefinitionPlacementConstraintsDetails ::
  AwsEcsTaskDefinitionPlacementConstraintsDetails
newAwsEcsTaskDefinitionPlacementConstraintsDetails =
  AwsEcsTaskDefinitionPlacementConstraintsDetails'
    { type' =
        Prelude.Nothing,
      expression =
        Prelude.Nothing
    }

-- | The type of constraint.
awsEcsTaskDefinitionPlacementConstraintsDetails_type :: Lens.Lens' AwsEcsTaskDefinitionPlacementConstraintsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionPlacementConstraintsDetails_type = Lens.lens (\AwsEcsTaskDefinitionPlacementConstraintsDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionPlacementConstraintsDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionPlacementConstraintsDetails)

-- | A cluster query language expression to apply to the constraint.
awsEcsTaskDefinitionPlacementConstraintsDetails_expression :: Lens.Lens' AwsEcsTaskDefinitionPlacementConstraintsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionPlacementConstraintsDetails_expression = Lens.lens (\AwsEcsTaskDefinitionPlacementConstraintsDetails' {expression} -> expression) (\s@AwsEcsTaskDefinitionPlacementConstraintsDetails' {} a -> s {expression = a} :: AwsEcsTaskDefinitionPlacementConstraintsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionPlacementConstraintsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionPlacementConstraintsDetails"
      ( \x ->
          AwsEcsTaskDefinitionPlacementConstraintsDetails'
            Prelude.<$> (x Core..:? "Type")
              Prelude.<*> (x Core..:? "Expression")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionPlacementConstraintsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionPlacementConstraintsDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` expression

instance
  Prelude.NFData
    AwsEcsTaskDefinitionPlacementConstraintsDetails
  where
  rnf
    AwsEcsTaskDefinitionPlacementConstraintsDetails' {..} =
      Prelude.rnf type'
        `Prelude.seq` Prelude.rnf expression

instance
  Core.ToJSON
    AwsEcsTaskDefinitionPlacementConstraintsDetails
  where
  toJSON
    AwsEcsTaskDefinitionPlacementConstraintsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Type" Core..=) Prelude.<$> type',
              ("Expression" Core..=) Prelude.<$> expression
            ]
        )
