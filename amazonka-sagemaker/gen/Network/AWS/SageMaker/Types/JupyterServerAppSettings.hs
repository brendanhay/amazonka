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
-- Module      : Network.AWS.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.JupyterServerAppSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The JupyterServer app settings.
--
-- /See:/ 'newJupyterServerAppSettings' smart constructor.
data JupyterServerAppSettings = JupyterServerAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- default SageMaker image used by the JupyterServer app.
    defaultResourceSpec :: Prelude.Maybe ResourceSpec
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JupyterServerAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'jupyterServerAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app.
newJupyterServerAppSettings ::
  JupyterServerAppSettings
newJupyterServerAppSettings =
  JupyterServerAppSettings'
    { defaultResourceSpec =
        Prelude.Nothing
    }

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app.
jupyterServerAppSettings_defaultResourceSpec :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe ResourceSpec)
jupyterServerAppSettings_defaultResourceSpec = Lens.lens (\JupyterServerAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@JupyterServerAppSettings' {} a -> s {defaultResourceSpec = a} :: JupyterServerAppSettings)

instance Prelude.FromJSON JupyterServerAppSettings where
  parseJSON =
    Prelude.withObject
      "JupyterServerAppSettings"
      ( \x ->
          JupyterServerAppSettings'
            Prelude.<$> (x Prelude..:? "DefaultResourceSpec")
      )

instance Prelude.Hashable JupyterServerAppSettings

instance Prelude.NFData JupyterServerAppSettings

instance Prelude.ToJSON JupyterServerAppSettings where
  toJSON JupyterServerAppSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DefaultResourceSpec" Prelude..=)
              Prelude.<$> defaultResourceSpec
          ]
      )
