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
-- Module      : Amazonka.SimSpaceWeaver.Types.Domain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SimSpaceWeaver.Types.LifecycleManagementStrategy

-- | A collection of app instances that run the same executable app code and
-- have the same launch options and commands.
--
-- For more information about domains, see
-- <https://docs.aws.amazon.com/simspaceweaver/latest/userguide/what-is_key-concepts.html Key concepts>
-- in the /Amazon Web Services SimSpace Weaver User Guide/.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The type of lifecycle management for apps in the domain. This value
    -- indicates whether apps in this domain are /managed/ (SimSpace Weaver
    -- starts and stops the apps) or /unmanaged/ (you must start and stop the
    -- apps).
    --
    -- __Lifecycle types__
    --
    -- -   @PerWorker@ – Managed: SimSpace Weaver starts 1 app on each worker
    --
    -- -   @BySpatialSubdivision@ – Managed: SimSpace Weaver starts 1 app for
    --     each spatial partition
    --
    -- -   @ByRequest@ – Unmanaged: You use the __StartApp__ API to start the
    --     apps and use the __StopApp__ API to stop the apps.
    --
    -- The lifecycle types will change when the service is released for general
    -- availability (GA).
    lifecycle :: Prelude.Maybe LifecycleManagementStrategy,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Domain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'domain_lifecycle' - The type of lifecycle management for apps in the domain. This value
-- indicates whether apps in this domain are /managed/ (SimSpace Weaver
-- starts and stops the apps) or /unmanaged/ (you must start and stop the
-- apps).
--
-- __Lifecycle types__
--
-- -   @PerWorker@ – Managed: SimSpace Weaver starts 1 app on each worker
--
-- -   @BySpatialSubdivision@ – Managed: SimSpace Weaver starts 1 app for
--     each spatial partition
--
-- -   @ByRequest@ – Unmanaged: You use the __StartApp__ API to start the
--     apps and use the __StopApp__ API to stop the apps.
--
-- The lifecycle types will change when the service is released for general
-- availability (GA).
--
-- 'name', 'domain_name' - The name of the domain.
newDomain ::
  Domain
newDomain =
  Domain'
    { lifecycle = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The type of lifecycle management for apps in the domain. This value
-- indicates whether apps in this domain are /managed/ (SimSpace Weaver
-- starts and stops the apps) or /unmanaged/ (you must start and stop the
-- apps).
--
-- __Lifecycle types__
--
-- -   @PerWorker@ – Managed: SimSpace Weaver starts 1 app on each worker
--
-- -   @BySpatialSubdivision@ – Managed: SimSpace Weaver starts 1 app for
--     each spatial partition
--
-- -   @ByRequest@ – Unmanaged: You use the __StartApp__ API to start the
--     apps and use the __StopApp__ API to stop the apps.
--
-- The lifecycle types will change when the service is released for general
-- availability (GA).
domain_lifecycle :: Lens.Lens' Domain (Prelude.Maybe LifecycleManagementStrategy)
domain_lifecycle = Lens.lens (\Domain' {lifecycle} -> lifecycle) (\s@Domain' {} a -> s {lifecycle = a} :: Domain)

-- | The name of the domain.
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain)

instance Data.FromJSON Domain where
  parseJSON =
    Data.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable Domain where
  hashWithSalt _salt Domain' {..} =
    _salt
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` name

instance Prelude.NFData Domain where
  rnf Domain' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf name
