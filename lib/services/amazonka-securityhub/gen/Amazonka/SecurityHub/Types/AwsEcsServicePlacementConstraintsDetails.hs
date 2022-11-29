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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsServicePlacementConstraintsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A placement constraint for the tasks in the service.
--
-- /See:/ 'newAwsEcsServicePlacementConstraintsDetails' smart constructor.
data AwsEcsServicePlacementConstraintsDetails = AwsEcsServicePlacementConstraintsDetails'
  { -- | The type of constraint. Use @distinctInstance@ to run each task in a
    -- particular group on a different container instance. Use @memberOf@ to
    -- restrict the selection to a group of valid candidates.
    --
    -- Valid values: @distinctInstance@ | @memberOf@
    type' :: Prelude.Maybe Prelude.Text,
    -- | A cluster query language expression to apply to the constraint. You
    -- cannot specify an expression if the constraint type is
    -- @distinctInstance@.
    expression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsServicePlacementConstraintsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEcsServicePlacementConstraintsDetails_type' - The type of constraint. Use @distinctInstance@ to run each task in a
-- particular group on a different container instance. Use @memberOf@ to
-- restrict the selection to a group of valid candidates.
--
-- Valid values: @distinctInstance@ | @memberOf@
--
-- 'expression', 'awsEcsServicePlacementConstraintsDetails_expression' - A cluster query language expression to apply to the constraint. You
-- cannot specify an expression if the constraint type is
-- @distinctInstance@.
newAwsEcsServicePlacementConstraintsDetails ::
  AwsEcsServicePlacementConstraintsDetails
newAwsEcsServicePlacementConstraintsDetails =
  AwsEcsServicePlacementConstraintsDetails'
    { type' =
        Prelude.Nothing,
      expression = Prelude.Nothing
    }

-- | The type of constraint. Use @distinctInstance@ to run each task in a
-- particular group on a different container instance. Use @memberOf@ to
-- restrict the selection to a group of valid candidates.
--
-- Valid values: @distinctInstance@ | @memberOf@
awsEcsServicePlacementConstraintsDetails_type :: Lens.Lens' AwsEcsServicePlacementConstraintsDetails (Prelude.Maybe Prelude.Text)
awsEcsServicePlacementConstraintsDetails_type = Lens.lens (\AwsEcsServicePlacementConstraintsDetails' {type'} -> type') (\s@AwsEcsServicePlacementConstraintsDetails' {} a -> s {type' = a} :: AwsEcsServicePlacementConstraintsDetails)

-- | A cluster query language expression to apply to the constraint. You
-- cannot specify an expression if the constraint type is
-- @distinctInstance@.
awsEcsServicePlacementConstraintsDetails_expression :: Lens.Lens' AwsEcsServicePlacementConstraintsDetails (Prelude.Maybe Prelude.Text)
awsEcsServicePlacementConstraintsDetails_expression = Lens.lens (\AwsEcsServicePlacementConstraintsDetails' {expression} -> expression) (\s@AwsEcsServicePlacementConstraintsDetails' {} a -> s {expression = a} :: AwsEcsServicePlacementConstraintsDetails)

instance
  Core.FromJSON
    AwsEcsServicePlacementConstraintsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsServicePlacementConstraintsDetails"
      ( \x ->
          AwsEcsServicePlacementConstraintsDetails'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Expression")
      )

instance
  Prelude.Hashable
    AwsEcsServicePlacementConstraintsDetails
  where
  hashWithSalt
    _salt
    AwsEcsServicePlacementConstraintsDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` expression

instance
  Prelude.NFData
    AwsEcsServicePlacementConstraintsDetails
  where
  rnf AwsEcsServicePlacementConstraintsDetails' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf expression

instance
  Core.ToJSON
    AwsEcsServicePlacementConstraintsDetails
  where
  toJSON AwsEcsServicePlacementConstraintsDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Type" Core..=) Prelude.<$> type',
            ("Expression" Core..=) Prelude.<$> expression
          ]
      )
