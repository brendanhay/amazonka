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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormStyleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormStyleConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration settings for the form\'s style properties.
--
-- /See:/ 'newFormStyleConfig' smart constructor.
data FormStyleConfig = FormStyleConfig'
  { -- | A reference to a design token to use to bind the form\'s style
    -- properties to an existing theme.
    tokenReference :: Prelude.Maybe Prelude.Text,
    -- | The value of the style setting.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormStyleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenReference', 'formStyleConfig_tokenReference' - A reference to a design token to use to bind the form\'s style
-- properties to an existing theme.
--
-- 'value', 'formStyleConfig_value' - The value of the style setting.
newFormStyleConfig ::
  FormStyleConfig
newFormStyleConfig =
  FormStyleConfig'
    { tokenReference = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A reference to a design token to use to bind the form\'s style
-- properties to an existing theme.
formStyleConfig_tokenReference :: Lens.Lens' FormStyleConfig (Prelude.Maybe Prelude.Text)
formStyleConfig_tokenReference = Lens.lens (\FormStyleConfig' {tokenReference} -> tokenReference) (\s@FormStyleConfig' {} a -> s {tokenReference = a} :: FormStyleConfig)

-- | The value of the style setting.
formStyleConfig_value :: Lens.Lens' FormStyleConfig (Prelude.Maybe Prelude.Text)
formStyleConfig_value = Lens.lens (\FormStyleConfig' {value} -> value) (\s@FormStyleConfig' {} a -> s {value = a} :: FormStyleConfig)

instance Data.FromJSON FormStyleConfig where
  parseJSON =
    Data.withObject
      "FormStyleConfig"
      ( \x ->
          FormStyleConfig'
            Prelude.<$> (x Data..:? "tokenReference")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable FormStyleConfig where
  hashWithSalt _salt FormStyleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` tokenReference
      `Prelude.hashWithSalt` value

instance Prelude.NFData FormStyleConfig where
  rnf FormStyleConfig' {..} =
    Prelude.rnf tokenReference
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FormStyleConfig where
  toJSON FormStyleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tokenReference" Data..=)
              Prelude.<$> tokenReference,
            ("value" Data..=) Prelude.<$> value
          ]
      )
