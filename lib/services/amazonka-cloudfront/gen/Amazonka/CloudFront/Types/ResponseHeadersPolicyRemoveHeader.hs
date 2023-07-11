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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of an HTTP header that CloudFront removes from HTTP responses
-- to requests that match the cache behavior that this response headers
-- policy is attached to.
--
-- /See:/ 'newResponseHeadersPolicyRemoveHeader' smart constructor.
data ResponseHeadersPolicyRemoveHeader = ResponseHeadersPolicyRemoveHeader'
  { -- | The HTTP header name.
    header :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyRemoveHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'responseHeadersPolicyRemoveHeader_header' - The HTTP header name.
newResponseHeadersPolicyRemoveHeader ::
  -- | 'header'
  Prelude.Text ->
  ResponseHeadersPolicyRemoveHeader
newResponseHeadersPolicyRemoveHeader pHeader_ =
  ResponseHeadersPolicyRemoveHeader'
    { header =
        pHeader_
    }

-- | The HTTP header name.
responseHeadersPolicyRemoveHeader_header :: Lens.Lens' ResponseHeadersPolicyRemoveHeader Prelude.Text
responseHeadersPolicyRemoveHeader_header = Lens.lens (\ResponseHeadersPolicyRemoveHeader' {header} -> header) (\s@ResponseHeadersPolicyRemoveHeader' {} a -> s {header = a} :: ResponseHeadersPolicyRemoveHeader)

instance
  Data.FromXML
    ResponseHeadersPolicyRemoveHeader
  where
  parseXML x =
    ResponseHeadersPolicyRemoveHeader'
      Prelude.<$> (x Data..@ "Header")

instance
  Prelude.Hashable
    ResponseHeadersPolicyRemoveHeader
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyRemoveHeader' {..} =
      _salt `Prelude.hashWithSalt` header

instance
  Prelude.NFData
    ResponseHeadersPolicyRemoveHeader
  where
  rnf ResponseHeadersPolicyRemoveHeader' {..} =
    Prelude.rnf header

instance Data.ToXML ResponseHeadersPolicyRemoveHeader where
  toXML ResponseHeadersPolicyRemoveHeader' {..} =
    Prelude.mconcat ["Header" Data.@= header]
