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
-- Module      : Amazonka.AppSync.Types.AppSyncRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.AppSyncRuntime where

import Amazonka.AppSync.Types.RuntimeName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a runtime used by an Amazon Web Services AppSync pipeline
-- resolver or Amazon Web Services AppSync function. Specifies the name and
-- version of the runtime to use. Note that if a runtime is specified, code
-- must also be specified.
--
-- /See:/ 'newAppSyncRuntime' smart constructor.
data AppSyncRuntime = AppSyncRuntime'
  { -- | The @name@ of the runtime to use. Currently, the only allowed value is
    -- @APPSYNC_JS@.
    name :: RuntimeName,
    -- | The @version@ of the runtime to use. Currently, the only allowed version
    -- is @1.0.0@.
    runtimeVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppSyncRuntime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'appSyncRuntime_name' - The @name@ of the runtime to use. Currently, the only allowed value is
-- @APPSYNC_JS@.
--
-- 'runtimeVersion', 'appSyncRuntime_runtimeVersion' - The @version@ of the runtime to use. Currently, the only allowed version
-- is @1.0.0@.
newAppSyncRuntime ::
  -- | 'name'
  RuntimeName ->
  -- | 'runtimeVersion'
  Prelude.Text ->
  AppSyncRuntime
newAppSyncRuntime pName_ pRuntimeVersion_ =
  AppSyncRuntime'
    { name = pName_,
      runtimeVersion = pRuntimeVersion_
    }

-- | The @name@ of the runtime to use. Currently, the only allowed value is
-- @APPSYNC_JS@.
appSyncRuntime_name :: Lens.Lens' AppSyncRuntime RuntimeName
appSyncRuntime_name = Lens.lens (\AppSyncRuntime' {name} -> name) (\s@AppSyncRuntime' {} a -> s {name = a} :: AppSyncRuntime)

-- | The @version@ of the runtime to use. Currently, the only allowed version
-- is @1.0.0@.
appSyncRuntime_runtimeVersion :: Lens.Lens' AppSyncRuntime Prelude.Text
appSyncRuntime_runtimeVersion = Lens.lens (\AppSyncRuntime' {runtimeVersion} -> runtimeVersion) (\s@AppSyncRuntime' {} a -> s {runtimeVersion = a} :: AppSyncRuntime)

instance Data.FromJSON AppSyncRuntime where
  parseJSON =
    Data.withObject
      "AppSyncRuntime"
      ( \x ->
          AppSyncRuntime'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "runtimeVersion")
      )

instance Prelude.Hashable AppSyncRuntime where
  hashWithSalt _salt AppSyncRuntime' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runtimeVersion

instance Prelude.NFData AppSyncRuntime where
  rnf AppSyncRuntime' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf runtimeVersion

instance Data.ToJSON AppSyncRuntime where
  toJSON AppSyncRuntime' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("runtimeVersion" Data..= runtimeVersion)
          ]
      )
