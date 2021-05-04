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
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelGatewayImageConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.FileSystemConfig
import Network.AWS.SageMaker.Types.KernelSpec

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      kernelSpecs =
        Prelude._Coerce Lens.# pKernelSpecs_
    }

-- | The Amazon Elastic File System (EFS) storage configuration for a
-- SageMaker image.
kernelGatewayImageConfig_fileSystemConfig :: Lens.Lens' KernelGatewayImageConfig (Prelude.Maybe FileSystemConfig)
kernelGatewayImageConfig_fileSystemConfig = Lens.lens (\KernelGatewayImageConfig' {fileSystemConfig} -> fileSystemConfig) (\s@KernelGatewayImageConfig' {} a -> s {fileSystemConfig = a} :: KernelGatewayImageConfig)

-- | The specification of the Jupyter kernels in the image.
kernelGatewayImageConfig_kernelSpecs :: Lens.Lens' KernelGatewayImageConfig (Prelude.NonEmpty KernelSpec)
kernelGatewayImageConfig_kernelSpecs = Lens.lens (\KernelGatewayImageConfig' {kernelSpecs} -> kernelSpecs) (\s@KernelGatewayImageConfig' {} a -> s {kernelSpecs = a} :: KernelGatewayImageConfig) Prelude.. Prelude._Coerce

instance Prelude.FromJSON KernelGatewayImageConfig where
  parseJSON =
    Prelude.withObject
      "KernelGatewayImageConfig"
      ( \x ->
          KernelGatewayImageConfig'
            Prelude.<$> (x Prelude..:? "FileSystemConfig")
            Prelude.<*> (x Prelude..: "KernelSpecs")
      )

instance Prelude.Hashable KernelGatewayImageConfig

instance Prelude.NFData KernelGatewayImageConfig

instance Prelude.ToJSON KernelGatewayImageConfig where
  toJSON KernelGatewayImageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FileSystemConfig" Prelude..=)
              Prelude.<$> fileSystemConfig,
            Prelude.Just ("KernelSpecs" Prelude..= kernelSpecs)
          ]
      )
