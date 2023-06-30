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
-- Module      : Amazonka.OpsWorks.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.EnvironmentVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an app\'s environment variable.
--
-- /See:/ 'newEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { -- | (Optional) Whether the variable\'s value will be returned by the
    -- DescribeApps action. To conceal an environment variable\'s value, set
    -- @Secure@ to @true@. @DescribeApps@ then returns @*****FILTERED*****@
    -- instead of the actual value. The default value for @Secure@ is @false@.
    secure :: Prelude.Maybe Prelude.Bool,
    -- | (Required) The environment variable\'s name, which can consist of up to
    -- 64 characters and must be specified. The name can contain upper- and
    -- lowercase letters, numbers, and underscores (_), but it must start with
    -- a letter or underscore.
    key :: Prelude.Text,
    -- | (Optional) The environment variable\'s value, which can be left empty.
    -- If you specify a value, it can contain up to 256 characters, which must
    -- all be printable.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secure', 'environmentVariable_secure' - (Optional) Whether the variable\'s value will be returned by the
-- DescribeApps action. To conceal an environment variable\'s value, set
-- @Secure@ to @true@. @DescribeApps@ then returns @*****FILTERED*****@
-- instead of the actual value. The default value for @Secure@ is @false@.
--
-- 'key', 'environmentVariable_key' - (Required) The environment variable\'s name, which can consist of up to
-- 64 characters and must be specified. The name can contain upper- and
-- lowercase letters, numbers, and underscores (_), but it must start with
-- a letter or underscore.
--
-- 'value', 'environmentVariable_value' - (Optional) The environment variable\'s value, which can be left empty.
-- If you specify a value, it can contain up to 256 characters, which must
-- all be printable.
newEnvironmentVariable ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  EnvironmentVariable
newEnvironmentVariable pKey_ pValue_ =
  EnvironmentVariable'
    { secure = Prelude.Nothing,
      key = pKey_,
      value = pValue_
    }

-- | (Optional) Whether the variable\'s value will be returned by the
-- DescribeApps action. To conceal an environment variable\'s value, set
-- @Secure@ to @true@. @DescribeApps@ then returns @*****FILTERED*****@
-- instead of the actual value. The default value for @Secure@ is @false@.
environmentVariable_secure :: Lens.Lens' EnvironmentVariable (Prelude.Maybe Prelude.Bool)
environmentVariable_secure = Lens.lens (\EnvironmentVariable' {secure} -> secure) (\s@EnvironmentVariable' {} a -> s {secure = a} :: EnvironmentVariable)

-- | (Required) The environment variable\'s name, which can consist of up to
-- 64 characters and must be specified. The name can contain upper- and
-- lowercase letters, numbers, and underscores (_), but it must start with
-- a letter or underscore.
environmentVariable_key :: Lens.Lens' EnvironmentVariable Prelude.Text
environmentVariable_key = Lens.lens (\EnvironmentVariable' {key} -> key) (\s@EnvironmentVariable' {} a -> s {key = a} :: EnvironmentVariable)

-- | (Optional) The environment variable\'s value, which can be left empty.
-- If you specify a value, it can contain up to 256 characters, which must
-- all be printable.
environmentVariable_value :: Lens.Lens' EnvironmentVariable Prelude.Text
environmentVariable_value = Lens.lens (\EnvironmentVariable' {value} -> value) (\s@EnvironmentVariable' {} a -> s {value = a} :: EnvironmentVariable)

instance Data.FromJSON EnvironmentVariable where
  parseJSON =
    Data.withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            Prelude.<$> (x Data..:? "Secure")
            Prelude.<*> (x Data..: "Key")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable EnvironmentVariable where
  hashWithSalt _salt EnvironmentVariable' {..} =
    _salt
      `Prelude.hashWithSalt` secure
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData EnvironmentVariable where
  rnf EnvironmentVariable' {..} =
    Prelude.rnf secure
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Secure" Data..=) Prelude.<$> secure,
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
