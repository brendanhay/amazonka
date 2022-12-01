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
-- Module      : Amazonka.SSM.Types.OpsResultAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsResultAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The OpsItem data type to return.
--
-- /See:/ 'newOpsResultAttribute' smart constructor.
data OpsResultAttribute = OpsResultAttribute'
  { -- | Name of the data type. Valid value: @AWS:OpsItem@,
    -- @AWS:EC2InstanceInformation@, @AWS:OpsItemTrendline@, or
    -- @AWS:ComplianceSummary@.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsResultAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'opsResultAttribute_typeName' - Name of the data type. Valid value: @AWS:OpsItem@,
-- @AWS:EC2InstanceInformation@, @AWS:OpsItemTrendline@, or
-- @AWS:ComplianceSummary@.
newOpsResultAttribute ::
  -- | 'typeName'
  Prelude.Text ->
  OpsResultAttribute
newOpsResultAttribute pTypeName_ =
  OpsResultAttribute' {typeName = pTypeName_}

-- | Name of the data type. Valid value: @AWS:OpsItem@,
-- @AWS:EC2InstanceInformation@, @AWS:OpsItemTrendline@, or
-- @AWS:ComplianceSummary@.
opsResultAttribute_typeName :: Lens.Lens' OpsResultAttribute Prelude.Text
opsResultAttribute_typeName = Lens.lens (\OpsResultAttribute' {typeName} -> typeName) (\s@OpsResultAttribute' {} a -> s {typeName = a} :: OpsResultAttribute)

instance Prelude.Hashable OpsResultAttribute where
  hashWithSalt _salt OpsResultAttribute' {..} =
    _salt `Prelude.hashWithSalt` typeName

instance Prelude.NFData OpsResultAttribute where
  rnf OpsResultAttribute' {..} = Prelude.rnf typeName

instance Core.ToJSON OpsResultAttribute where
  toJSON OpsResultAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TypeName" Core..= typeName)]
      )
