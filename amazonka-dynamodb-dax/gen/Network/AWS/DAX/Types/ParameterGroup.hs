{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DAX.Types.ParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A named set of parameters that are applied to all of the nodes in a DAX
-- cluster.
--
-- /See:/ 'newParameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { -- | The name of the parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter group.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'parameterGroup_parameterGroupName' - The name of the parameter group.
--
-- 'description', 'parameterGroup_description' - A description of the parameter group.
newParameterGroup ::
  ParameterGroup
newParameterGroup =
  ParameterGroup'
    { parameterGroupName =
        Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the parameter group.
parameterGroup_parameterGroupName :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_parameterGroupName = Lens.lens (\ParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ParameterGroup' {} a -> s {parameterGroupName = a} :: ParameterGroup)

-- | A description of the parameter group.
parameterGroup_description :: Lens.Lens' ParameterGroup (Prelude.Maybe Prelude.Text)
parameterGroup_description = Lens.lens (\ParameterGroup' {description} -> description) (\s@ParameterGroup' {} a -> s {description = a} :: ParameterGroup)

instance Prelude.FromJSON ParameterGroup where
  parseJSON =
    Prelude.withObject
      "ParameterGroup"
      ( \x ->
          ParameterGroup'
            Prelude.<$> (x Prelude..:? "ParameterGroupName")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable ParameterGroup

instance Prelude.NFData ParameterGroup
