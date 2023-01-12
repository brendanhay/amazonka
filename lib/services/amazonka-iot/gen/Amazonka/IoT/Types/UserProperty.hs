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
-- Module      : Amazonka.IoT.Types.UserProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.UserProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair that you define in the header. Both the key and the
-- value are either literal strings or valid
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-substitution-templates.html substitution templates>.
--
-- /See:/ 'newUserProperty' smart constructor.
data UserProperty = UserProperty'
  { -- | A key to be specified in @UserProperty@.
    key :: Prelude.Text,
    -- | A value to be specified in @UserProperty@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'userProperty_key' - A key to be specified in @UserProperty@.
--
-- 'value', 'userProperty_value' - A value to be specified in @UserProperty@.
newUserProperty ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  UserProperty
newUserProperty pKey_ pValue_ =
  UserProperty' {key = pKey_, value = pValue_}

-- | A key to be specified in @UserProperty@.
userProperty_key :: Lens.Lens' UserProperty Prelude.Text
userProperty_key = Lens.lens (\UserProperty' {key} -> key) (\s@UserProperty' {} a -> s {key = a} :: UserProperty)

-- | A value to be specified in @UserProperty@.
userProperty_value :: Lens.Lens' UserProperty Prelude.Text
userProperty_value = Lens.lens (\UserProperty' {value} -> value) (\s@UserProperty' {} a -> s {value = a} :: UserProperty)

instance Data.FromJSON UserProperty where
  parseJSON =
    Data.withObject
      "UserProperty"
      ( \x ->
          UserProperty'
            Prelude.<$> (x Data..: "key") Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable UserProperty where
  hashWithSalt _salt UserProperty' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData UserProperty where
  rnf UserProperty' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON UserProperty where
  toJSON UserProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("value" Data..= value)
          ]
      )
