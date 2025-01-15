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
-- Module      : Amazonka.SageMaker.Types.KernelGatewayImageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.KernelGatewayImageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FileSystemConfig
import Amazonka.SageMaker.Types.KernelSpec

-- | The configuration for the file system and kernels in a SageMaker image
-- running as a KernelGateway app.
--
-- /See:/ 'newKernelGatewayImageConfig' smart constructor.
data KernelGatewayImageConfig = KernelGatewayImageConfig'
  { -- | The Amazon Elastic File System (EFS) storage configuration for a
    -- SageMaker image.
    fileSystemConfig :: Prelude.Maybe FileSystemConfig,
    -- | The specification of the Jupyter kernels in the image.
    kernelSpecs :: Prelude.NonEmpty KernelSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KernelGatewayImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemConfig', 'kernelGatewayImageConfig_fileSystemConfig' - The Amazon Elastic File System (EFS) storage configuration for a
-- SageMaker image.
--
-- 'kernelSpecs', 'kernelGatewayImageConfig_kernelSpecs' - The specification of the Jupyter kernels in the image.
newKernelGatewayImageConfig ::
  -- | 'kernelSpecs'
  Prelude.NonEmpty KernelSpec ->
  KernelGatewayImageConfig
newKernelGatewayImageConfig pKernelSpecs_ =
  KernelGatewayImageConfig'
    { fileSystemConfig =
        Prelude.Nothing,
      kernelSpecs = Lens.coerced Lens.# pKernelSpecs_
    }

-- | The Amazon Elastic File System (EFS) storage configuration for a
-- SageMaker image.
kernelGatewayImageConfig_fileSystemConfig :: Lens.Lens' KernelGatewayImageConfig (Prelude.Maybe FileSystemConfig)
kernelGatewayImageConfig_fileSystemConfig = Lens.lens (\KernelGatewayImageConfig' {fileSystemConfig} -> fileSystemConfig) (\s@KernelGatewayImageConfig' {} a -> s {fileSystemConfig = a} :: KernelGatewayImageConfig)

-- | The specification of the Jupyter kernels in the image.
kernelGatewayImageConfig_kernelSpecs :: Lens.Lens' KernelGatewayImageConfig (Prelude.NonEmpty KernelSpec)
kernelGatewayImageConfig_kernelSpecs = Lens.lens (\KernelGatewayImageConfig' {kernelSpecs} -> kernelSpecs) (\s@KernelGatewayImageConfig' {} a -> s {kernelSpecs = a} :: KernelGatewayImageConfig) Prelude.. Lens.coerced

instance Data.FromJSON KernelGatewayImageConfig where
  parseJSON =
    Data.withObject
      "KernelGatewayImageConfig"
      ( \x ->
          KernelGatewayImageConfig'
            Prelude.<$> (x Data..:? "FileSystemConfig")
            Prelude.<*> (x Data..: "KernelSpecs")
      )

instance Prelude.Hashable KernelGatewayImageConfig where
  hashWithSalt _salt KernelGatewayImageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemConfig
      `Prelude.hashWithSalt` kernelSpecs

instance Prelude.NFData KernelGatewayImageConfig where
  rnf KernelGatewayImageConfig' {..} =
    Prelude.rnf fileSystemConfig `Prelude.seq`
      Prelude.rnf kernelSpecs

instance Data.ToJSON KernelGatewayImageConfig where
  toJSON KernelGatewayImageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FileSystemConfig" Data..=)
              Prelude.<$> fileSystemConfig,
            Prelude.Just ("KernelSpecs" Data..= kernelSpecs)
          ]
      )
