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
-- Module      : Amazonka.SES.Types.ExtensionField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ExtensionField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional X-headers to include in the Delivery Status Notification
-- (DSN) when an email that Amazon SES receives on your behalf bounces.
--
-- For information about receiving email through Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newExtensionField' smart constructor.
data ExtensionField = ExtensionField'
  { -- | The name of the header to add. Must be between 1 and 50 characters,
    -- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
    -- dashes only.
    name :: Prelude.Text,
    -- | The value of the header to add. Must be less than 2048 characters, and
    -- must not contain newline characters (\"\\r\" or \"\\n\").
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtensionField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'extensionField_name' - The name of the header to add. Must be between 1 and 50 characters,
-- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
-- dashes only.
--
-- 'value', 'extensionField_value' - The value of the header to add. Must be less than 2048 characters, and
-- must not contain newline characters (\"\\r\" or \"\\n\").
newExtensionField ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  ExtensionField
newExtensionField pName_ pValue_ =
  ExtensionField' {name = pName_, value = pValue_}

-- | The name of the header to add. Must be between 1 and 50 characters,
-- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
-- dashes only.
extensionField_name :: Lens.Lens' ExtensionField Prelude.Text
extensionField_name = Lens.lens (\ExtensionField' {name} -> name) (\s@ExtensionField' {} a -> s {name = a} :: ExtensionField)

-- | The value of the header to add. Must be less than 2048 characters, and
-- must not contain newline characters (\"\\r\" or \"\\n\").
extensionField_value :: Lens.Lens' ExtensionField Prelude.Text
extensionField_value = Lens.lens (\ExtensionField' {value} -> value) (\s@ExtensionField' {} a -> s {value = a} :: ExtensionField)

instance Prelude.Hashable ExtensionField where
  hashWithSalt _salt ExtensionField' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ExtensionField where
  rnf ExtensionField' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToQuery ExtensionField where
  toQuery ExtensionField' {..} =
    Prelude.mconcat
      ["Name" Data.=: name, "Value" Data.=: value]
