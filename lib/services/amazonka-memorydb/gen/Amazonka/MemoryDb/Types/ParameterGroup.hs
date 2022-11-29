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
-- Module      : Amazonka.MemoryDb.Types.ParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a CreateParameterGroup operation. A parameter
-- group represents a combination of specific values for the parameters
-- that are passed to the engine software during startup.
--
-- /See:/ 'newParameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { -- | The name of the parameter group
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the parameter group
    arn :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter group
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group family that this parameter group is
    -- compatible with.
    family :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parameterGroup_name' - The name of the parameter group
--
-- 'arn', 'parameterGroup_arn' - The Amazon Resource Name (ARN) of the parameter group
--
-- 'description', 'parameterGroup_description' - A description of the parameter group
--
-- 'family', 'parameterGroup_family' - The name of the parameter group family that this parameter group is
-- compatible with.
newParameterGroup ::
  ParameterGroup
newParameterGroup =
  ParameterGroup'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      family = Prelude.Nothing
    }

-- | The name of the parameter group
parameterGroup_name :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_name = Lens.lens (\ParameterGroup' {name} -> name) (\s@ParameterGroup' {} a -> s {name = a} :: ParameterGroup)

-- | The Amazon Resource Name (ARN) of the parameter group
parameterGroup_arn :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_arn = Lens.lens (\ParameterGroup' {arn} -> arn) (\s@ParameterGroup' {} a -> s {arn = a} :: ParameterGroup)

-- | A description of the parameter group
parameterGroup_description :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_description = Lens.lens (\ParameterGroup' {description} -> description) (\s@ParameterGroup' {} a -> s {description = a} :: ParameterGroup)

-- | The name of the parameter group family that this parameter group is
-- compatible with.
parameterGroup_family :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_family = Lens.lens (\ParameterGroup' {family} -> family) (\s@ParameterGroup' {} a -> s {family = a} :: ParameterGroup)

instance Core.FromJSON ParameterGroup where
  parseJSON =
    Core.withObject
      "ParameterGroup"
      ( \x ->
          ParameterGroup'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Family")
      )

instance Prelude.Hashable ParameterGroup where
  hashWithSalt _salt ParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` family

instance Prelude.NFData ParameterGroup where
  rnf ParameterGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf family
