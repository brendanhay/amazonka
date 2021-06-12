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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The HTTP action header.
--
-- /See:/ 'newHttpActionHeader' smart constructor.
data HttpActionHeader = HttpActionHeader'
  { -- | The HTTP header key.
    key :: Core.Text,
    -- | The HTTP header value. Substitution templates are supported.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'value'
  Core.Text ->
  HttpActionHeader
newHttpActionHeader pKey_ pValue_ =
  HttpActionHeader' {key = pKey_, value = pValue_}

-- | The HTTP header key.
httpActionHeader_key :: Lens.Lens' HttpActionHeader Core.Text
httpActionHeader_key = Lens.lens (\HttpActionHeader' {key} -> key) (\s@HttpActionHeader' {} a -> s {key = a} :: HttpActionHeader)

-- | The HTTP header value. Substitution templates are supported.
httpActionHeader_value :: Lens.Lens' HttpActionHeader Core.Text
httpActionHeader_value = Lens.lens (\HttpActionHeader' {value} -> value) (\s@HttpActionHeader' {} a -> s {value = a} :: HttpActionHeader)

instance Core.FromJSON HttpActionHeader where
  parseJSON =
    Core.withObject
      "HttpActionHeader"
      ( \x ->
          HttpActionHeader'
            Core.<$> (x Core..: "key") Core.<*> (x Core..: "value")
      )

instance Core.Hashable HttpActionHeader

instance Core.NFData HttpActionHeader

instance Core.ToJSON HttpActionHeader where
  toJSON HttpActionHeader' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("key" Core..= key),
            Core.Just ("value" Core..= value)
          ]
      )
