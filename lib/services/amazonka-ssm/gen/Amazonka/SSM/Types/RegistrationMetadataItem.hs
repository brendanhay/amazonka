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
-- Module      : Amazonka.SSM.Types.RegistrationMetadataItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.RegistrationMetadataItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Reserved for internal use.
--
-- /See:/ 'newRegistrationMetadataItem' smart constructor.
data RegistrationMetadataItem = RegistrationMetadataItem'
  { -- | Reserved for internal use.
    key :: Prelude.Text,
    -- | Reserved for internal use.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegistrationMetadataItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'registrationMetadataItem_key' - Reserved for internal use.
--
-- 'value', 'registrationMetadataItem_value' - Reserved for internal use.
newRegistrationMetadataItem ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  RegistrationMetadataItem
newRegistrationMetadataItem pKey_ pValue_ =
  RegistrationMetadataItem'
    { key = pKey_,
      value = pValue_
    }

-- | Reserved for internal use.
registrationMetadataItem_key :: Lens.Lens' RegistrationMetadataItem Prelude.Text
registrationMetadataItem_key = Lens.lens (\RegistrationMetadataItem' {key} -> key) (\s@RegistrationMetadataItem' {} a -> s {key = a} :: RegistrationMetadataItem)

-- | Reserved for internal use.
registrationMetadataItem_value :: Lens.Lens' RegistrationMetadataItem Prelude.Text
registrationMetadataItem_value = Lens.lens (\RegistrationMetadataItem' {value} -> value) (\s@RegistrationMetadataItem' {} a -> s {value = a} :: RegistrationMetadataItem)

instance Prelude.Hashable RegistrationMetadataItem where
  hashWithSalt _salt RegistrationMetadataItem' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData RegistrationMetadataItem where
  rnf RegistrationMetadataItem' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON RegistrationMetadataItem where
  toJSON RegistrationMetadataItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
