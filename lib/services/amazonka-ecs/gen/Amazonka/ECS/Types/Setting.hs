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
-- Module      : Amazonka.ECS.Types.Setting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Setting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.SettingName
import qualified Amazonka.Prelude as Prelude

-- | The current account setting for a resource.
--
-- /See:/ 'newSetting' smart constructor.
data Setting = Setting'
  { -- | The Amazon ECS resource name.
    name :: Prelude.Maybe SettingName,
    -- | The ARN of the principal. It can be an IAM user, IAM role, or the root
    -- user. If this field is omitted, the authenticated user is assumed.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the account setting is enabled or disabled for the
    -- specified resource.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'principalArn', 'setting_principalArn' - The ARN of the principal. It can be an IAM user, IAM role, or the root
-- user. If this field is omitted, the authenticated user is assumed.
--
-- 'value', 'setting_value' - Determines whether the account setting is enabled or disabled for the
-- specified resource.
newSetting ::
  Setting
newSetting =
  Setting'
    { name = Prelude.Nothing,
      principalArn = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The Amazon ECS resource name.
setting_name :: Lens.Lens' Setting (Prelude.Maybe SettingName)
setting_name = Lens.lens (\Setting' {name} -> name) (\s@Setting' {} a -> s {name = a} :: Setting)

-- | The ARN of the principal. It can be an IAM user, IAM role, or the root
-- user. If this field is omitted, the authenticated user is assumed.
setting_principalArn :: Lens.Lens' Setting (Prelude.Maybe Prelude.Text)
setting_principalArn = Lens.lens (\Setting' {principalArn} -> principalArn) (\s@Setting' {} a -> s {principalArn = a} :: Setting)

-- | Determines whether the account setting is enabled or disabled for the
-- specified resource.
setting_value :: Lens.Lens' Setting (Prelude.Maybe Prelude.Text)
setting_value = Lens.lens (\Setting' {value} -> value) (\s@Setting' {} a -> s {value = a} :: Setting)

instance Data.FromJSON Setting where
  parseJSON =
    Data.withObject
      "Setting"
      ( \x ->
          Setting'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "principalArn")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Setting where
  hashWithSalt _salt Setting' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` value

instance Prelude.NFData Setting where
  rnf Setting' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf value
