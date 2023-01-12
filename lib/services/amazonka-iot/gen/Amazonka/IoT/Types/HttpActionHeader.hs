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
-- Module      : Amazonka.IoT.Types.HttpActionHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.HttpActionHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The HTTP action header.
--
-- /See:/ 'newHttpActionHeader' smart constructor.
data HttpActionHeader = HttpActionHeader'
  { -- | The HTTP header key.
    key :: Prelude.Text,
    -- | The HTTP header value. Substitution templates are supported.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpActionHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'httpActionHeader_key' - The HTTP header key.
--
-- 'value', 'httpActionHeader_value' - The HTTP header value. Substitution templates are supported.
newHttpActionHeader ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  HttpActionHeader
newHttpActionHeader pKey_ pValue_ =
  HttpActionHeader' {key = pKey_, value = pValue_}

-- | The HTTP header key.
httpActionHeader_key :: Lens.Lens' HttpActionHeader Prelude.Text
httpActionHeader_key = Lens.lens (\HttpActionHeader' {key} -> key) (\s@HttpActionHeader' {} a -> s {key = a} :: HttpActionHeader)

-- | The HTTP header value. Substitution templates are supported.
httpActionHeader_value :: Lens.Lens' HttpActionHeader Prelude.Text
httpActionHeader_value = Lens.lens (\HttpActionHeader' {value} -> value) (\s@HttpActionHeader' {} a -> s {value = a} :: HttpActionHeader)

instance Data.FromJSON HttpActionHeader where
  parseJSON =
    Data.withObject
      "HttpActionHeader"
      ( \x ->
          HttpActionHeader'
            Prelude.<$> (x Data..: "key") Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable HttpActionHeader where
  hashWithSalt _salt HttpActionHeader' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData HttpActionHeader where
  rnf HttpActionHeader' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON HttpActionHeader where
  toJSON HttpActionHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("value" Data..= value)
          ]
      )
