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
-- Module      : Network.AWS.SageMaker.Types.TensorBoardAppSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardAppSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The TensorBoard app settings.
--
-- /See:/ 'newTensorBoardAppSettings' smart constructor.
data TensorBoardAppSettings = TensorBoardAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- SageMaker image created on the instance.
    defaultResourceSpec :: Core.Maybe ResourceSpec
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TensorBoardAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'tensorBoardAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- SageMaker image created on the instance.
newTensorBoardAppSettings ::
  TensorBoardAppSettings
newTensorBoardAppSettings =
  TensorBoardAppSettings'
    { defaultResourceSpec =
        Core.Nothing
    }

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- SageMaker image created on the instance.
tensorBoardAppSettings_defaultResourceSpec :: Lens.Lens' TensorBoardAppSettings (Core.Maybe ResourceSpec)
tensorBoardAppSettings_defaultResourceSpec = Lens.lens (\TensorBoardAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@TensorBoardAppSettings' {} a -> s {defaultResourceSpec = a} :: TensorBoardAppSettings)

instance Core.FromJSON TensorBoardAppSettings where
  parseJSON =
    Core.withObject
      "TensorBoardAppSettings"
      ( \x ->
          TensorBoardAppSettings'
            Core.<$> (x Core..:? "DefaultResourceSpec")
      )

instance Core.Hashable TensorBoardAppSettings

instance Core.NFData TensorBoardAppSettings

instance Core.ToJSON TensorBoardAppSettings where
  toJSON TensorBoardAppSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultResourceSpec" Core..=)
              Core.<$> defaultResourceSpec
          ]
      )
