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
-- Module      : Network.AWS.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelSpec where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The specification of a Jupyter kernel.
--
-- /See:/ 'newKernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { -- | The display name of the kernel.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the kernel.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  KernelSpec
newKernelSpec pName_ =
  KernelSpec'
    { displayName = Prelude.Nothing,
      name = pName_
    }

-- | The display name of the kernel.
kernelSpec_displayName :: Lens.Lens' KernelSpec (Prelude.Maybe Prelude.Text)
kernelSpec_displayName = Lens.lens (\KernelSpec' {displayName} -> displayName) (\s@KernelSpec' {} a -> s {displayName = a} :: KernelSpec)

-- | The name of the kernel.
kernelSpec_name :: Lens.Lens' KernelSpec Prelude.Text
kernelSpec_name = Lens.lens (\KernelSpec' {name} -> name) (\s@KernelSpec' {} a -> s {name = a} :: KernelSpec)

instance Prelude.FromJSON KernelSpec where
  parseJSON =
    Prelude.withObject
      "KernelSpec"
      ( \x ->
          KernelSpec'
            Prelude.<$> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable KernelSpec

instance Prelude.NFData KernelSpec

instance Prelude.ToJSON KernelSpec where
  toJSON KernelSpec' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DisplayName" Prelude..=) Prelude.<$> displayName,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
