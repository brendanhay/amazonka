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
-- Module      : Network.AWS.AutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AdjustmentType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a policy adjustment type.
--
-- /See:/ 'newAdjustmentType' smart constructor.
data AdjustmentType = AdjustmentType'
  { -- | The policy adjustment type. The valid values are @ChangeInCapacity@,
    -- @ExactCapacity@, and @PercentChangeInCapacity@.
    adjustmentType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdjustmentType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustmentType', 'adjustmentType_adjustmentType' - The policy adjustment type. The valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
newAdjustmentType ::
  AdjustmentType
newAdjustmentType =
  AdjustmentType' {adjustmentType = Prelude.Nothing}

-- | The policy adjustment type. The valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
adjustmentType_adjustmentType :: Lens.Lens' AdjustmentType (Prelude.Maybe Prelude.Text)
adjustmentType_adjustmentType = Lens.lens (\AdjustmentType' {adjustmentType} -> adjustmentType) (\s@AdjustmentType' {} a -> s {adjustmentType = a} :: AdjustmentType)

instance Prelude.FromXML AdjustmentType where
  parseXML x =
    AdjustmentType'
      Prelude.<$> (x Prelude..@? "AdjustmentType")

instance Prelude.Hashable AdjustmentType

instance Prelude.NFData AdjustmentType
