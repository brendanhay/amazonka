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
-- Module      : Amazonka.LexV2Models.Types.MultipleValuesSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.MultipleValuesSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether a slot can return multiple values.
--
-- /See:/ 'newMultipleValuesSetting' smart constructor.
data MultipleValuesSetting = MultipleValuesSetting'
  { -- | Indicates whether a slot can return multiple values. When @true@, the
    -- slot may return more than one value in a response. When @false@, the
    -- slot returns only a single value.
    --
    -- Multi-value slots are only available in the en-US locale. If you set
    -- this value to @true@ in any other locale, Amazon Lex throws a
    -- @ValidationException@.
    --
    -- If the @allowMutlipleValues@ is not set, the default value is @false@.
    allowMultipleValues :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultipleValuesSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowMultipleValues', 'multipleValuesSetting_allowMultipleValues' - Indicates whether a slot can return multiple values. When @true@, the
-- slot may return more than one value in a response. When @false@, the
-- slot returns only a single value.
--
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @allowMutlipleValues@ is not set, the default value is @false@.
newMultipleValuesSetting ::
  MultipleValuesSetting
newMultipleValuesSetting =
  MultipleValuesSetting'
    { allowMultipleValues =
        Prelude.Nothing
    }

-- | Indicates whether a slot can return multiple values. When @true@, the
-- slot may return more than one value in a response. When @false@, the
-- slot returns only a single value.
--
-- Multi-value slots are only available in the en-US locale. If you set
-- this value to @true@ in any other locale, Amazon Lex throws a
-- @ValidationException@.
--
-- If the @allowMutlipleValues@ is not set, the default value is @false@.
multipleValuesSetting_allowMultipleValues :: Lens.Lens' MultipleValuesSetting (Prelude.Maybe Prelude.Bool)
multipleValuesSetting_allowMultipleValues = Lens.lens (\MultipleValuesSetting' {allowMultipleValues} -> allowMultipleValues) (\s@MultipleValuesSetting' {} a -> s {allowMultipleValues = a} :: MultipleValuesSetting)

instance Data.FromJSON MultipleValuesSetting where
  parseJSON =
    Data.withObject
      "MultipleValuesSetting"
      ( \x ->
          MultipleValuesSetting'
            Prelude.<$> (x Data..:? "allowMultipleValues")
      )

instance Prelude.Hashable MultipleValuesSetting where
  hashWithSalt _salt MultipleValuesSetting' {..} =
    _salt `Prelude.hashWithSalt` allowMultipleValues

instance Prelude.NFData MultipleValuesSetting where
  rnf MultipleValuesSetting' {..} =
    Prelude.rnf allowMultipleValues

instance Data.ToJSON MultipleValuesSetting where
  toJSON MultipleValuesSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowMultipleValues" Data..=)
              Prelude.<$> allowMultipleValues
          ]
      )
