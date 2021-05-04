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
-- Module      : Network.AWS.CodeDeploy.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoScalingGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | An Auto Scaling lifecycle event hook name.
    hook :: Prelude.Maybe Prelude.Text,
    -- | The Auto Scaling group name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hook', 'autoScalingGroup_hook' - An Auto Scaling lifecycle event hook name.
--
-- 'name', 'autoScalingGroup_name' - The Auto Scaling group name.
newAutoScalingGroup ::
  AutoScalingGroup
newAutoScalingGroup =
  AutoScalingGroup'
    { hook = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An Auto Scaling lifecycle event hook name.
autoScalingGroup_hook :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_hook = Lens.lens (\AutoScalingGroup' {hook} -> hook) (\s@AutoScalingGroup' {} a -> s {hook = a} :: AutoScalingGroup)

-- | The Auto Scaling group name.
autoScalingGroup_name :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_name = Lens.lens (\AutoScalingGroup' {name} -> name) (\s@AutoScalingGroup' {} a -> s {name = a} :: AutoScalingGroup)

instance Prelude.FromJSON AutoScalingGroup where
  parseJSON =
    Prelude.withObject
      "AutoScalingGroup"
      ( \x ->
          AutoScalingGroup'
            Prelude.<$> (x Prelude..:? "hook")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable AutoScalingGroup

instance Prelude.NFData AutoScalingGroup
