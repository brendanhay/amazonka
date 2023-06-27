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
-- Module      : Amazonka.CustomerProfiles.Types.FoundByKeyValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.FoundByKeyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A data type pair that consists of a @KeyName@ and @Values@ list that
-- were used to find a profile returned in response to a
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- request.
--
-- /See:/ 'newFoundByKeyValue' smart constructor.
data FoundByKeyValue = FoundByKeyValue'
  { -- | A searchable identifier of a customer profile.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | A list of key values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FoundByKeyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'foundByKeyValue_keyName' - A searchable identifier of a customer profile.
--
-- 'values', 'foundByKeyValue_values' - A list of key values.
newFoundByKeyValue ::
  FoundByKeyValue
newFoundByKeyValue =
  FoundByKeyValue'
    { keyName = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | A searchable identifier of a customer profile.
foundByKeyValue_keyName :: Lens.Lens' FoundByKeyValue (Prelude.Maybe Prelude.Text)
foundByKeyValue_keyName = Lens.lens (\FoundByKeyValue' {keyName} -> keyName) (\s@FoundByKeyValue' {} a -> s {keyName = a} :: FoundByKeyValue)

-- | A list of key values.
foundByKeyValue_values :: Lens.Lens' FoundByKeyValue (Prelude.Maybe [Prelude.Text])
foundByKeyValue_values = Lens.lens (\FoundByKeyValue' {values} -> values) (\s@FoundByKeyValue' {} a -> s {values = a} :: FoundByKeyValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FoundByKeyValue where
  parseJSON =
    Data.withObject
      "FoundByKeyValue"
      ( \x ->
          FoundByKeyValue'
            Prelude.<$> (x Data..:? "KeyName")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FoundByKeyValue where
  hashWithSalt _salt FoundByKeyValue' {..} =
    _salt
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` values

instance Prelude.NFData FoundByKeyValue where
  rnf FoundByKeyValue' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf values
