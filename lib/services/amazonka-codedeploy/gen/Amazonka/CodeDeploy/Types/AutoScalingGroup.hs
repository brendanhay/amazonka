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
-- Module      : Amazonka.CodeDeploy.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.AutoScalingGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | The Auto Scaling group name.
    name :: Prelude.Maybe Prelude.Text,
    -- | An Auto Scaling lifecycle event hook name.
    hook :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'autoScalingGroup_name' - The Auto Scaling group name.
--
-- 'hook', 'autoScalingGroup_hook' - An Auto Scaling lifecycle event hook name.
newAutoScalingGroup ::
  AutoScalingGroup
newAutoScalingGroup =
  AutoScalingGroup'
    { name = Prelude.Nothing,
      hook = Prelude.Nothing
    }

-- | The Auto Scaling group name.
autoScalingGroup_name :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_name = Lens.lens (\AutoScalingGroup' {name} -> name) (\s@AutoScalingGroup' {} a -> s {name = a} :: AutoScalingGroup)

-- | An Auto Scaling lifecycle event hook name.
autoScalingGroup_hook :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_hook = Lens.lens (\AutoScalingGroup' {hook} -> hook) (\s@AutoScalingGroup' {} a -> s {hook = a} :: AutoScalingGroup)

instance Data.FromJSON AutoScalingGroup where
  parseJSON =
    Data.withObject
      "AutoScalingGroup"
      ( \x ->
          AutoScalingGroup'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "hook")
      )

instance Prelude.Hashable AutoScalingGroup where
  hashWithSalt _salt AutoScalingGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` hook

instance Prelude.NFData AutoScalingGroup where
  rnf AutoScalingGroup' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf hook
