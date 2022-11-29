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
-- Module      : Amazonka.PrivateNetworks.Types.NameValuePair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.NameValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a name\/value pair.
--
-- /See:/ 'newNameValuePair' smart constructor.
data NameValuePair = NameValuePair'
  { -- | The value of the pair.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of the pair.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NameValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'nameValuePair_value' - The value of the pair.
--
-- 'name', 'nameValuePair_name' - The name of the pair.
newNameValuePair ::
  -- | 'name'
  Prelude.Text ->
  NameValuePair
newNameValuePair pName_ =
  NameValuePair'
    { value = Prelude.Nothing,
      name = pName_
    }

-- | The value of the pair.
nameValuePair_value :: Lens.Lens' NameValuePair (Prelude.Maybe Prelude.Text)
nameValuePair_value = Lens.lens (\NameValuePair' {value} -> value) (\s@NameValuePair' {} a -> s {value = a} :: NameValuePair)

-- | The name of the pair.
nameValuePair_name :: Lens.Lens' NameValuePair Prelude.Text
nameValuePair_name = Lens.lens (\NameValuePair' {name} -> name) (\s@NameValuePair' {} a -> s {name = a} :: NameValuePair)

instance Core.FromJSON NameValuePair where
  parseJSON =
    Core.withObject
      "NameValuePair"
      ( \x ->
          NameValuePair'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable NameValuePair where
  hashWithSalt _salt NameValuePair' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData NameValuePair where
  rnf NameValuePair' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON NameValuePair where
  toJSON NameValuePair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            Prelude.Just ("name" Core..= name)
          ]
      )
