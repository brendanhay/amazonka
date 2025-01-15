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
-- Module      : Amazonka.DAX.Types.ParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.ParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A named set of parameters that are applied to all of the nodes in a DAX
-- cluster.
--
-- /See:/ 'newParameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { -- | A description of the parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text
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
-- 'description', 'parameterGroup_description' - A description of the parameter group.
--
-- 'parameterGroupName', 'parameterGroup_parameterGroupName' - The name of the parameter group.
newParameterGroup ::
  ParameterGroup
newParameterGroup =
  ParameterGroup'
    { description = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing
    }

-- | A description of the parameter group.
parameterGroup_description :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_description = Lens.lens (\ParameterGroup' {description} -> description) (\s@ParameterGroup' {} a -> s {description = a} :: ParameterGroup)

-- | The name of the parameter group.
parameterGroup_parameterGroupName :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_parameterGroupName = Lens.lens (\ParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ParameterGroup' {} a -> s {parameterGroupName = a} :: ParameterGroup)

instance Data.FromJSON ParameterGroup where
  parseJSON =
    Data.withObject
      "ParameterGroup"
      ( \x ->
          ParameterGroup'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ParameterGroupName")
      )

instance Prelude.Hashable ParameterGroup where
  hashWithSalt _salt ParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData ParameterGroup where
  rnf ParameterGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf parameterGroupName
