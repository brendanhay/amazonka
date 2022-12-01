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
-- Module      : Amazonka.ECS.Types.SystemControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.SystemControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of namespaced kernel parameters to set in the container. This
-- parameter maps to @Sysctls@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @--sysctl@ option to
-- <https://docs.docker.com/engine/reference/run/#security-configuration docker run>.
--
-- We don\'t recommend that you specify network-related @systemControls@
-- parameters for multiple containers in a single task. This task also uses
-- either the @awsvpc@ or @host@ network mode. It does it for the following
-- reasons.
--
-- -   For tasks that use the @awsvpc@ network mode, if you set
--     @systemControls@ for any container, it applies to all containers in
--     the task. If you set different @systemControls@ for multiple
--     containers in a single task, the container that\'s started last
--     determines which @systemControls@ take effect.
--
-- -   For tasks that use the @host@ network mode, the @systemControls@
--     parameter applies to the container instance\'s kernel parameter and
--     that of all containers of any tasks running on that container
--     instance.
--
-- /See:/ 'newSystemControl' smart constructor.
data SystemControl = SystemControl'
  { -- | The namespaced kernel parameter to set a @value@ for.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The value for the namespaced kernel parameter that\'s specified in
    -- @namespace@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'systemControl_namespace' - The namespaced kernel parameter to set a @value@ for.
--
-- 'value', 'systemControl_value' - The value for the namespaced kernel parameter that\'s specified in
-- @namespace@.
newSystemControl ::
  SystemControl
newSystemControl =
  SystemControl'
    { namespace = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The namespaced kernel parameter to set a @value@ for.
systemControl_namespace :: Lens.Lens' SystemControl (Prelude.Maybe Prelude.Text)
systemControl_namespace = Lens.lens (\SystemControl' {namespace} -> namespace) (\s@SystemControl' {} a -> s {namespace = a} :: SystemControl)

-- | The value for the namespaced kernel parameter that\'s specified in
-- @namespace@.
systemControl_value :: Lens.Lens' SystemControl (Prelude.Maybe Prelude.Text)
systemControl_value = Lens.lens (\SystemControl' {value} -> value) (\s@SystemControl' {} a -> s {value = a} :: SystemControl)

instance Core.FromJSON SystemControl where
  parseJSON =
    Core.withObject
      "SystemControl"
      ( \x ->
          SystemControl'
            Prelude.<$> (x Core..:? "namespace")
            Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable SystemControl where
  hashWithSalt _salt SystemControl' {..} =
    _salt `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` value

instance Prelude.NFData SystemControl where
  rnf SystemControl' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON SystemControl where
  toJSON SystemControl' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("namespace" Core..=) Prelude.<$> namespace,
            ("value" Core..=) Prelude.<$> value
          ]
      )
