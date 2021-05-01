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
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayAppSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelGatewayAppSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CustomImage
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The KernelGateway app settings.
--
-- /See:/ 'newKernelGatewayAppSettings' smart constructor.
data KernelGatewayAppSettings = KernelGatewayAppSettings'
  { -- | A list of custom SageMaker images that are configured to run as a
    -- KernelGateway app.
    customImages :: Prelude.Maybe [CustomImage],
    -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- default SageMaker image used by the KernelGateway app.
    defaultResourceSpec :: Prelude.Maybe ResourceSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KernelGatewayAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customImages', 'kernelGatewayAppSettings_customImages' - A list of custom SageMaker images that are configured to run as a
-- KernelGateway app.
--
-- 'defaultResourceSpec', 'kernelGatewayAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the KernelGateway app.
newKernelGatewayAppSettings ::
  KernelGatewayAppSettings
newKernelGatewayAppSettings =
  KernelGatewayAppSettings'
    { customImages =
        Prelude.Nothing,
      defaultResourceSpec = Prelude.Nothing
    }

-- | A list of custom SageMaker images that are configured to run as a
-- KernelGateway app.
kernelGatewayAppSettings_customImages :: Lens.Lens' KernelGatewayAppSettings (Prelude.Maybe [CustomImage])
kernelGatewayAppSettings_customImages = Lens.lens (\KernelGatewayAppSettings' {customImages} -> customImages) (\s@KernelGatewayAppSettings' {} a -> s {customImages = a} :: KernelGatewayAppSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the KernelGateway app.
kernelGatewayAppSettings_defaultResourceSpec :: Lens.Lens' KernelGatewayAppSettings (Prelude.Maybe ResourceSpec)
kernelGatewayAppSettings_defaultResourceSpec = Lens.lens (\KernelGatewayAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@KernelGatewayAppSettings' {} a -> s {defaultResourceSpec = a} :: KernelGatewayAppSettings)

instance Prelude.FromJSON KernelGatewayAppSettings where
  parseJSON =
    Prelude.withObject
      "KernelGatewayAppSettings"
      ( \x ->
          KernelGatewayAppSettings'
            Prelude.<$> ( x Prelude..:? "CustomImages"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DefaultResourceSpec")
      )

instance Prelude.Hashable KernelGatewayAppSettings

instance Prelude.NFData KernelGatewayAppSettings

instance Prelude.ToJSON KernelGatewayAppSettings where
  toJSON KernelGatewayAppSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CustomImages" Prelude..=)
              Prelude.<$> customImages,
            ("DefaultResourceSpec" Prelude..=)
              Prelude.<$> defaultResourceSpec
          ]
      )
