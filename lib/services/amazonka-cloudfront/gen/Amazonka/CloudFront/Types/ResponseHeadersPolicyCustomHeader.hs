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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An HTTP response header name and its value. CloudFront includes this
-- header in HTTP responses that it sends for requests that match a cache
-- behavior that\'s associated with this response headers policy.
--
-- /See:/ 'newResponseHeadersPolicyCustomHeader' smart constructor.
data ResponseHeadersPolicyCustomHeader = ResponseHeadersPolicyCustomHeader'
  { -- | The HTTP response header name.
    header :: Prelude.Text,
    -- | The value for the HTTP response header.
    value :: Prelude.Text,
    -- | A Boolean that determines whether CloudFront overrides a response header
    -- with the same name received from the origin with the header specified
    -- here.
    override :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyCustomHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'responseHeadersPolicyCustomHeader_header' - The HTTP response header name.
--
-- 'value', 'responseHeadersPolicyCustomHeader_value' - The value for the HTTP response header.
--
-- 'override', 'responseHeadersPolicyCustomHeader_override' - A Boolean that determines whether CloudFront overrides a response header
-- with the same name received from the origin with the header specified
-- here.
newResponseHeadersPolicyCustomHeader ::
  -- | 'header'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  -- | 'override'
  Prelude.Bool ->
  ResponseHeadersPolicyCustomHeader
newResponseHeadersPolicyCustomHeader
  pHeader_
  pValue_
  pOverride_ =
    ResponseHeadersPolicyCustomHeader'
      { header =
          pHeader_,
        value = pValue_,
        override = pOverride_
      }

-- | The HTTP response header name.
responseHeadersPolicyCustomHeader_header :: Lens.Lens' ResponseHeadersPolicyCustomHeader Prelude.Text
responseHeadersPolicyCustomHeader_header = Lens.lens (\ResponseHeadersPolicyCustomHeader' {header} -> header) (\s@ResponseHeadersPolicyCustomHeader' {} a -> s {header = a} :: ResponseHeadersPolicyCustomHeader)

-- | The value for the HTTP response header.
responseHeadersPolicyCustomHeader_value :: Lens.Lens' ResponseHeadersPolicyCustomHeader Prelude.Text
responseHeadersPolicyCustomHeader_value = Lens.lens (\ResponseHeadersPolicyCustomHeader' {value} -> value) (\s@ResponseHeadersPolicyCustomHeader' {} a -> s {value = a} :: ResponseHeadersPolicyCustomHeader)

-- | A Boolean that determines whether CloudFront overrides a response header
-- with the same name received from the origin with the header specified
-- here.
responseHeadersPolicyCustomHeader_override :: Lens.Lens' ResponseHeadersPolicyCustomHeader Prelude.Bool
responseHeadersPolicyCustomHeader_override = Lens.lens (\ResponseHeadersPolicyCustomHeader' {override} -> override) (\s@ResponseHeadersPolicyCustomHeader' {} a -> s {override = a} :: ResponseHeadersPolicyCustomHeader)

instance
  Data.FromXML
    ResponseHeadersPolicyCustomHeader
  where
  parseXML x =
    ResponseHeadersPolicyCustomHeader'
      Prelude.<$> (x Data..@ "Header")
      Prelude.<*> (x Data..@ "Value")
      Prelude.<*> (x Data..@ "Override")

instance
  Prelude.Hashable
    ResponseHeadersPolicyCustomHeader
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyCustomHeader' {..} =
      _salt
        `Prelude.hashWithSalt` header
        `Prelude.hashWithSalt` value
        `Prelude.hashWithSalt` override

instance
  Prelude.NFData
    ResponseHeadersPolicyCustomHeader
  where
  rnf ResponseHeadersPolicyCustomHeader' {..} =
    Prelude.rnf header `Prelude.seq`
      Prelude.rnf value `Prelude.seq`
        Prelude.rnf override

instance Data.ToXML ResponseHeadersPolicyCustomHeader where
  toXML ResponseHeadersPolicyCustomHeader' {..} =
    Prelude.mconcat
      [ "Header" Data.@= header,
        "Value" Data.@= value,
        "Override" Data.@= override
      ]
