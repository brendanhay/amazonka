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
-- Module      : Amazonka.EC2.Types.InstanceTypeInfoFromInstanceRequirements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceTypeInfoFromInstanceRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The list of instance types with the specified instance attributes.
--
-- /See:/ 'newInstanceTypeInfoFromInstanceRequirements' smart constructor.
data InstanceTypeInfoFromInstanceRequirements = InstanceTypeInfoFromInstanceRequirements'
  { -- | The matching instance type.
    instanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeInfoFromInstanceRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceTypeInfoFromInstanceRequirements_instanceType' - The matching instance type.
newInstanceTypeInfoFromInstanceRequirements ::
  InstanceTypeInfoFromInstanceRequirements
newInstanceTypeInfoFromInstanceRequirements =
  InstanceTypeInfoFromInstanceRequirements'
    { instanceType =
        Prelude.Nothing
    }

-- | The matching instance type.
instanceTypeInfoFromInstanceRequirements_instanceType :: Lens.Lens' InstanceTypeInfoFromInstanceRequirements (Prelude.Maybe Prelude.Text)
instanceTypeInfoFromInstanceRequirements_instanceType = Lens.lens (\InstanceTypeInfoFromInstanceRequirements' {instanceType} -> instanceType) (\s@InstanceTypeInfoFromInstanceRequirements' {} a -> s {instanceType = a} :: InstanceTypeInfoFromInstanceRequirements)

instance
  Data.FromXML
    InstanceTypeInfoFromInstanceRequirements
  where
  parseXML x =
    InstanceTypeInfoFromInstanceRequirements'
      Prelude.<$> (x Data..@? "instanceType")

instance
  Prelude.Hashable
    InstanceTypeInfoFromInstanceRequirements
  where
  hashWithSalt
    _salt
    InstanceTypeInfoFromInstanceRequirements' {..} =
      _salt `Prelude.hashWithSalt` instanceType

instance
  Prelude.NFData
    InstanceTypeInfoFromInstanceRequirements
  where
  rnf InstanceTypeInfoFromInstanceRequirements' {..} =
    Prelude.rnf instanceType
