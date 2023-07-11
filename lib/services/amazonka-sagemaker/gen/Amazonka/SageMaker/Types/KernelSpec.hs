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
-- Module      : Amazonka.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.KernelSpec where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The specification of a Jupyter kernel.
--
-- /See:/ 'newKernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { -- | The display name of the kernel.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Jupyter kernel in the image. This value is case
    -- sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'name', 'kernelSpec_name' - The name of the Jupyter kernel in the image. This value is case
-- sensitive.
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

-- | The name of the Jupyter kernel in the image. This value is case
-- sensitive.
kernelSpec_name :: Lens.Lens' KernelSpec Prelude.Text
kernelSpec_name = Lens.lens (\KernelSpec' {name} -> name) (\s@KernelSpec' {} a -> s {name = a} :: KernelSpec)

instance Data.FromJSON KernelSpec where
  parseJSON =
    Data.withObject
      "KernelSpec"
      ( \x ->
          KernelSpec'
            Prelude.<$> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable KernelSpec where
  hashWithSalt _salt KernelSpec' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` name

instance Prelude.NFData KernelSpec where
  rnf KernelSpec' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON KernelSpec where
  toJSON KernelSpec' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            Prelude.Just ("Name" Data..= name)
          ]
      )
