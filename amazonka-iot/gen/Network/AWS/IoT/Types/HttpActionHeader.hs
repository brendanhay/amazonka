{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.HttpActionHeader
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HttpActionHeader where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The HTTP action header.
--
-- /See:/ 'newHttpActionHeader' smart constructor.
data HttpActionHeader = HttpActionHeader'
  { -- | The HTTP header key.
    key :: Prelude.Text,
    -- | The HTTP header value. Substitution templates are supported.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON HttpActionHeader where
  parseJSON =
    Prelude.withObject
      "HttpActionHeader"
      ( \x ->
          HttpActionHeader'
            Prelude.<$> (x Prelude..: "key")
            Prelude.<*> (x Prelude..: "value")
      )

instance Prelude.Hashable HttpActionHeader

instance Prelude.NFData HttpActionHeader

instance Prelude.ToJSON HttpActionHeader where
  toJSON HttpActionHeader' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
