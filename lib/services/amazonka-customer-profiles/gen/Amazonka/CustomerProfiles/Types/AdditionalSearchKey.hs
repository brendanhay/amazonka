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
-- Module      : Amazonka.CustomerProfiles.Types.AdditionalSearchKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AdditionalSearchKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A data type pair that consists of a @KeyName@ and @Values@ list that is
-- used in conjunction with the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html#customerprofiles-SearchProfiles-request-KeyName KeyName>
-- and
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html#customerprofiles-SearchProfiles-request-Values Values>
-- parameters to search for profiles using the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- API.
--
-- /See:/ 'newAdditionalSearchKey' smart constructor.
data AdditionalSearchKey = AdditionalSearchKey'
  { -- | A searchable identifier of a customer profile.
    keyName :: Prelude.Text,
    -- | A list of key values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalSearchKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'additionalSearchKey_keyName' - A searchable identifier of a customer profile.
--
-- 'values', 'additionalSearchKey_values' - A list of key values.
newAdditionalSearchKey ::
  -- | 'keyName'
  Prelude.Text ->
  AdditionalSearchKey
newAdditionalSearchKey pKeyName_ =
  AdditionalSearchKey'
    { keyName = pKeyName_,
      values = Prelude.mempty
    }

-- | A searchable identifier of a customer profile.
additionalSearchKey_keyName :: Lens.Lens' AdditionalSearchKey Prelude.Text
additionalSearchKey_keyName = Lens.lens (\AdditionalSearchKey' {keyName} -> keyName) (\s@AdditionalSearchKey' {} a -> s {keyName = a} :: AdditionalSearchKey)

-- | A list of key values.
additionalSearchKey_values :: Lens.Lens' AdditionalSearchKey [Prelude.Text]
additionalSearchKey_values = Lens.lens (\AdditionalSearchKey' {values} -> values) (\s@AdditionalSearchKey' {} a -> s {values = a} :: AdditionalSearchKey) Prelude.. Lens.coerced

instance Prelude.Hashable AdditionalSearchKey where
  hashWithSalt _salt AdditionalSearchKey' {..} =
    _salt `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` values

instance Prelude.NFData AdditionalSearchKey where
  rnf AdditionalSearchKey' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON AdditionalSearchKey where
  toJSON AdditionalSearchKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyName" Data..= keyName),
            Prelude.Just ("Values" Data..= values)
          ]
      )
