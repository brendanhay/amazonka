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
-- Module      : Network.AWS.SSM.Types.OpsResultAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsResultAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The OpsItem data type to return.
--
-- /See:/ 'newOpsResultAttribute' smart constructor.
data OpsResultAttribute = OpsResultAttribute'
  { -- | Name of the data type. Valid value: AWS:OpsItem,
    -- AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or
    -- AWS:ComplianceSummary.
    typeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsResultAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'opsResultAttribute_typeName' - Name of the data type. Valid value: AWS:OpsItem,
-- AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or
-- AWS:ComplianceSummary.
newOpsResultAttribute ::
  -- | 'typeName'
  Core.Text ->
  OpsResultAttribute
newOpsResultAttribute pTypeName_ =
  OpsResultAttribute' {typeName = pTypeName_}

-- | Name of the data type. Valid value: AWS:OpsItem,
-- AWS:EC2InstanceInformation, AWS:OpsItemTrendline, or
-- AWS:ComplianceSummary.
opsResultAttribute_typeName :: Lens.Lens' OpsResultAttribute Core.Text
opsResultAttribute_typeName = Lens.lens (\OpsResultAttribute' {typeName} -> typeName) (\s@OpsResultAttribute' {} a -> s {typeName = a} :: OpsResultAttribute)

instance Core.Hashable OpsResultAttribute

instance Core.NFData OpsResultAttribute

instance Core.ToJSON OpsResultAttribute where
  toJSON OpsResultAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TypeName" Core..= typeName)]
      )
