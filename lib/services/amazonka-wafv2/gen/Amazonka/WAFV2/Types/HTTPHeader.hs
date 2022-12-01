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
-- Module      : Amazonka.WAFV2.Types.HTTPHeader
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.HTTPHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Part of the response from GetSampledRequests. This is a complex type
-- that appears as @Headers@ in the response syntax. @HTTPHeader@ contains
-- the names and values of all of the headers that appear in one of the web
-- requests.
--
-- /See:/ 'newHTTPHeader' smart constructor.
data HTTPHeader = HTTPHeader'
  { -- | The name of the HTTP header.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the HTTP header.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HTTPHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'hTTPHeader_name' - The name of the HTTP header.
--
-- 'value', 'hTTPHeader_value' - The value of the HTTP header.
newHTTPHeader ::
  HTTPHeader
newHTTPHeader =
  HTTPHeader'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the HTTP header.
hTTPHeader_name :: Lens.Lens' HTTPHeader (Prelude.Maybe Prelude.Text)
hTTPHeader_name = Lens.lens (\HTTPHeader' {name} -> name) (\s@HTTPHeader' {} a -> s {name = a} :: HTTPHeader)

-- | The value of the HTTP header.
hTTPHeader_value :: Lens.Lens' HTTPHeader (Prelude.Maybe Prelude.Text)
hTTPHeader_value = Lens.lens (\HTTPHeader' {value} -> value) (\s@HTTPHeader' {} a -> s {value = a} :: HTTPHeader)

instance Core.FromJSON HTTPHeader where
  parseJSON =
    Core.withObject
      "HTTPHeader"
      ( \x ->
          HTTPHeader'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable HTTPHeader where
  hashWithSalt _salt HTTPHeader' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData HTTPHeader where
  rnf HTTPHeader' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
