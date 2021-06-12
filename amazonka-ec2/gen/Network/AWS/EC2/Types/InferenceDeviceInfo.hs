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
-- Module      : Network.AWS.EC2.Types.InferenceDeviceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceDeviceInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'newInferenceDeviceInfo' smart constructor.
data InferenceDeviceInfo = InferenceDeviceInfo'
  { -- | The manufacturer of the Inference accelerator.
    manufacturer :: Core.Maybe Core.Text,
    -- | The name of the Inference accelerator.
    name :: Core.Maybe Core.Text,
    -- | The number of Inference accelerators for the instance type.
    count :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InferenceDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manufacturer', 'inferenceDeviceInfo_manufacturer' - The manufacturer of the Inference accelerator.
--
-- 'name', 'inferenceDeviceInfo_name' - The name of the Inference accelerator.
--
-- 'count', 'inferenceDeviceInfo_count' - The number of Inference accelerators for the instance type.
newInferenceDeviceInfo ::
  InferenceDeviceInfo
newInferenceDeviceInfo =
  InferenceDeviceInfo'
    { manufacturer = Core.Nothing,
      name = Core.Nothing,
      count = Core.Nothing
    }

-- | The manufacturer of the Inference accelerator.
inferenceDeviceInfo_manufacturer :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Core.Text)
inferenceDeviceInfo_manufacturer = Lens.lens (\InferenceDeviceInfo' {manufacturer} -> manufacturer) (\s@InferenceDeviceInfo' {} a -> s {manufacturer = a} :: InferenceDeviceInfo)

-- | The name of the Inference accelerator.
inferenceDeviceInfo_name :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Core.Text)
inferenceDeviceInfo_name = Lens.lens (\InferenceDeviceInfo' {name} -> name) (\s@InferenceDeviceInfo' {} a -> s {name = a} :: InferenceDeviceInfo)

-- | The number of Inference accelerators for the instance type.
inferenceDeviceInfo_count :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Core.Int)
inferenceDeviceInfo_count = Lens.lens (\InferenceDeviceInfo' {count} -> count) (\s@InferenceDeviceInfo' {} a -> s {count = a} :: InferenceDeviceInfo)

instance Core.FromXML InferenceDeviceInfo where
  parseXML x =
    InferenceDeviceInfo'
      Core.<$> (x Core..@? "manufacturer")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "count")

instance Core.Hashable InferenceDeviceInfo

instance Core.NFData InferenceDeviceInfo
