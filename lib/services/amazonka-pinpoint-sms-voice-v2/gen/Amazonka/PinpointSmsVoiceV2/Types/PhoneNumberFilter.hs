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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for a phone number that meets a specified criteria.
--
-- /See:/ 'newPhoneNumberFilter' smart constructor.
data PhoneNumberFilter = PhoneNumberFilter'
  { -- | The name of the attribute to filter on.
    name :: PhoneNumberFilterName,
    -- | An array values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'phoneNumberFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'phoneNumberFilter_values' - An array values to filter for.
newPhoneNumberFilter ::
  -- | 'name'
  PhoneNumberFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  PhoneNumberFilter
newPhoneNumberFilter pName_ pValues_ =
  PhoneNumberFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
phoneNumberFilter_name :: Lens.Lens' PhoneNumberFilter PhoneNumberFilterName
phoneNumberFilter_name = Lens.lens (\PhoneNumberFilter' {name} -> name) (\s@PhoneNumberFilter' {} a -> s {name = a} :: PhoneNumberFilter)

-- | An array values to filter for.
phoneNumberFilter_values :: Lens.Lens' PhoneNumberFilter (Prelude.NonEmpty Prelude.Text)
phoneNumberFilter_values = Lens.lens (\PhoneNumberFilter' {values} -> values) (\s@PhoneNumberFilter' {} a -> s {values = a} :: PhoneNumberFilter) Prelude.. Lens.coerced

instance Prelude.Hashable PhoneNumberFilter where
  hashWithSalt _salt PhoneNumberFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData PhoneNumberFilter where
  rnf PhoneNumberFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PhoneNumberFilter where
  toJSON PhoneNumberFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
