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
-- Module      : Network.AWS.ECS.Types.Setting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Setting where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.SettingName
import qualified Network.AWS.Lens as Lens

-- | The current account setting for a resource.
--
-- /See:/ 'newSetting' smart constructor.
data Setting = Setting'
  { -- | The Amazon ECS resource name.
    name :: Core.Maybe SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. If this field is omitted, the authenticated user is assumed.
    principalArn :: Core.Maybe Core.Text,
    -- | Whether the account setting is enabled or disabled for the specified
    -- resource.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Setting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'setting_name' - The Amazon ECS resource name.
--
-- 'principalArn', 'setting_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the authenticated user is assumed.
--
-- 'value', 'setting_value' - Whether the account setting is enabled or disabled for the specified
-- resource.
newSetting ::
  Setting
newSetting =
  Setting'
    { name = Core.Nothing,
      principalArn = Core.Nothing,
      value = Core.Nothing
    }

-- | The Amazon ECS resource name.
setting_name :: Lens.Lens' Setting (Core.Maybe SettingName)
setting_name = Lens.lens (\Setting' {name} -> name) (\s@Setting' {} a -> s {name = a} :: Setting)

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the authenticated user is assumed.
setting_principalArn :: Lens.Lens' Setting (Core.Maybe Core.Text)
setting_principalArn = Lens.lens (\Setting' {principalArn} -> principalArn) (\s@Setting' {} a -> s {principalArn = a} :: Setting)

-- | Whether the account setting is enabled or disabled for the specified
-- resource.
setting_value :: Lens.Lens' Setting (Core.Maybe Core.Text)
setting_value = Lens.lens (\Setting' {value} -> value) (\s@Setting' {} a -> s {value = a} :: Setting)

instance Core.FromJSON Setting where
  parseJSON =
    Core.withObject
      "Setting"
      ( \x ->
          Setting'
            Core.<$> (x Core..:? "name")
            Core.<*> (x Core..:? "principalArn")
            Core.<*> (x Core..:? "value")
      )

instance Core.Hashable Setting

instance Core.NFData Setting
