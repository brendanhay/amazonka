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
-- Module      : Network.AWS.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelSpec where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The specification of a Jupyter kernel.
--
-- /See:/ 'newKernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { -- | The display name of the kernel.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the kernel.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KernelSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'kernelSpec_displayName' - The display name of the kernel.
--
-- 'name', 'kernelSpec_name' - The name of the kernel.
newKernelSpec ::
  -- | 'name'
  Core.Text ->
  KernelSpec
newKernelSpec pName_ =
  KernelSpec'
    { displayName = Core.Nothing,
      name = pName_
    }

-- | The display name of the kernel.
kernelSpec_displayName :: Lens.Lens' KernelSpec (Core.Maybe Core.Text)
kernelSpec_displayName = Lens.lens (\KernelSpec' {displayName} -> displayName) (\s@KernelSpec' {} a -> s {displayName = a} :: KernelSpec)

-- | The name of the kernel.
kernelSpec_name :: Lens.Lens' KernelSpec Core.Text
kernelSpec_name = Lens.lens (\KernelSpec' {name} -> name) (\s@KernelSpec' {} a -> s {name = a} :: KernelSpec)

instance Core.FromJSON KernelSpec where
  parseJSON =
    Core.withObject
      "KernelSpec"
      ( \x ->
          KernelSpec'
            Core.<$> (x Core..:? "DisplayName")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable KernelSpec

instance Core.NFData KernelSpec

instance Core.ToJSON KernelSpec where
  toJSON KernelSpec' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DisplayName" Core..=) Core.<$> displayName,
            Core.Just ("Name" Core..= name)
          ]
      )
