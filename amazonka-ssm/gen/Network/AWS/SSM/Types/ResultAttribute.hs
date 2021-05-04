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
-- Module      : Network.AWS.SSM.Types.ResultAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResultAttribute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The inventory item result attribute.
--
-- /See:/ 'newResultAttribute' smart constructor.
data ResultAttribute = ResultAttribute'
  { -- | Name of the inventory item type. Valid value: AWS:InstanceInformation.
    -- Default Value: AWS:InstanceInformation.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResultAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'resultAttribute_typeName' - Name of the inventory item type. Valid value: AWS:InstanceInformation.
-- Default Value: AWS:InstanceInformation.
newResultAttribute ::
  -- | 'typeName'
  Prelude.Text ->
  ResultAttribute
newResultAttribute pTypeName_ =
  ResultAttribute' {typeName = pTypeName_}

-- | Name of the inventory item type. Valid value: AWS:InstanceInformation.
-- Default Value: AWS:InstanceInformation.
resultAttribute_typeName :: Lens.Lens' ResultAttribute Prelude.Text
resultAttribute_typeName = Lens.lens (\ResultAttribute' {typeName} -> typeName) (\s@ResultAttribute' {} a -> s {typeName = a} :: ResultAttribute)

instance Prelude.Hashable ResultAttribute

instance Prelude.NFData ResultAttribute

instance Prelude.ToJSON ResultAttribute where
  toJSON ResultAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TypeName" Prelude..= typeName)]
      )
