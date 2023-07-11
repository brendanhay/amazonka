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
-- Module      : Amazonka.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.JupyterServerAppSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CodeRepository
import Amazonka.SageMaker.Types.ResourceSpec

-- | The JupyterServer app settings.
--
-- /See:/ 'newJupyterServerAppSettings' smart constructor.
data JupyterServerAppSettings = JupyterServerAppSettings'
  { -- | A list of Git repositories that SageMaker automatically displays to
    -- users for cloning in the JupyterServer application.
    codeRepositories :: Prelude.Maybe [CodeRepository],
    -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- default SageMaker image used by the JupyterServer app. If you use the
    -- @LifecycleConfigArns@ parameter, then this parameter is also required.
    defaultResourceSpec :: Prelude.Maybe ResourceSpec,
    -- | The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
    -- to the JupyterServerApp. If you use this parameter, the
    -- @DefaultResourceSpec@ parameter is also required.
    --
    -- To remove a Lifecycle Config, you must set @LifecycleConfigArns@ to an
    -- empty list.
    lifecycleConfigArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JupyterServerAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeRepositories', 'jupyterServerAppSettings_codeRepositories' - A list of Git repositories that SageMaker automatically displays to
-- users for cloning in the JupyterServer application.
--
-- 'defaultResourceSpec', 'jupyterServerAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app. If you use the
-- @LifecycleConfigArns@ parameter, then this parameter is also required.
--
-- 'lifecycleConfigArns', 'jupyterServerAppSettings_lifecycleConfigArns' - The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
-- to the JupyterServerApp. If you use this parameter, the
-- @DefaultResourceSpec@ parameter is also required.
--
-- To remove a Lifecycle Config, you must set @LifecycleConfigArns@ to an
-- empty list.
newJupyterServerAppSettings ::
  JupyterServerAppSettings
newJupyterServerAppSettings =
  JupyterServerAppSettings'
    { codeRepositories =
        Prelude.Nothing,
      defaultResourceSpec = Prelude.Nothing,
      lifecycleConfigArns = Prelude.Nothing
    }

-- | A list of Git repositories that SageMaker automatically displays to
-- users for cloning in the JupyterServer application.
jupyterServerAppSettings_codeRepositories :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe [CodeRepository])
jupyterServerAppSettings_codeRepositories = Lens.lens (\JupyterServerAppSettings' {codeRepositories} -> codeRepositories) (\s@JupyterServerAppSettings' {} a -> s {codeRepositories = a} :: JupyterServerAppSettings) Prelude.. Lens.mapping Lens.coerced

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app. If you use the
-- @LifecycleConfigArns@ parameter, then this parameter is also required.
jupyterServerAppSettings_defaultResourceSpec :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe ResourceSpec)
jupyterServerAppSettings_defaultResourceSpec = Lens.lens (\JupyterServerAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@JupyterServerAppSettings' {} a -> s {defaultResourceSpec = a} :: JupyterServerAppSettings)

-- | The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
-- to the JupyterServerApp. If you use this parameter, the
-- @DefaultResourceSpec@ parameter is also required.
--
-- To remove a Lifecycle Config, you must set @LifecycleConfigArns@ to an
-- empty list.
jupyterServerAppSettings_lifecycleConfigArns :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe [Prelude.Text])
jupyterServerAppSettings_lifecycleConfigArns = Lens.lens (\JupyterServerAppSettings' {lifecycleConfigArns} -> lifecycleConfigArns) (\s@JupyterServerAppSettings' {} a -> s {lifecycleConfigArns = a} :: JupyterServerAppSettings) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JupyterServerAppSettings where
  parseJSON =
    Data.withObject
      "JupyterServerAppSettings"
      ( \x ->
          JupyterServerAppSettings'
            Prelude.<$> ( x
                            Data..:? "CodeRepositories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DefaultResourceSpec")
            Prelude.<*> ( x
                            Data..:? "LifecycleConfigArns"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable JupyterServerAppSettings where
  hashWithSalt _salt JupyterServerAppSettings' {..} =
    _salt
      `Prelude.hashWithSalt` codeRepositories
      `Prelude.hashWithSalt` defaultResourceSpec
      `Prelude.hashWithSalt` lifecycleConfigArns

instance Prelude.NFData JupyterServerAppSettings where
  rnf JupyterServerAppSettings' {..} =
    Prelude.rnf codeRepositories
      `Prelude.seq` Prelude.rnf defaultResourceSpec
      `Prelude.seq` Prelude.rnf lifecycleConfigArns

instance Data.ToJSON JupyterServerAppSettings where
  toJSON JupyterServerAppSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeRepositories" Data..=)
              Prelude.<$> codeRepositories,
            ("DefaultResourceSpec" Data..=)
              Prelude.<$> defaultResourceSpec,
            ("LifecycleConfigArns" Data..=)
              Prelude.<$> lifecycleConfigArns
          ]
      )
