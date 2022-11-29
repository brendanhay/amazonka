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
-- Module      : Amazonka.S3.Types.CORSRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CORSRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Specifies a cross-origin access rule for an Amazon S3 bucket.
--
-- /See:/ 'newCORSRule' smart constructor.
data CORSRule = CORSRule'
  { -- | Headers that are specified in the @Access-Control-Request-Headers@
    -- header. These headers are allowed in a preflight OPTIONS request. In
    -- response to any preflight OPTIONS request, Amazon S3 returns any
    -- requested headers that are allowed.
    allowedHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Unique identifier for the rule. The value cannot be longer than 255
    -- characters.
    id :: Prelude.Maybe Prelude.Text,
    -- | One or more headers in the response that you want customers to be able
    -- to access from their applications (for example, from a JavaScript
    -- @XMLHttpRequest@ object).
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The time in seconds that your browser is to cache the preflight response
    -- for the specified resource.
    maxAgeSeconds :: Prelude.Maybe Prelude.Int,
    -- | An HTTP method that you allow the origin to execute. Valid values are
    -- @GET@, @PUT@, @HEAD@, @POST@, and @DELETE@.
    allowedMethods :: [Prelude.Text],
    -- | One or more origins you want customers to be able to access the bucket
    -- from.
    allowedOrigins :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CORSRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedHeaders', 'cORSRule_allowedHeaders' - Headers that are specified in the @Access-Control-Request-Headers@
-- header. These headers are allowed in a preflight OPTIONS request. In
-- response to any preflight OPTIONS request, Amazon S3 returns any
-- requested headers that are allowed.
--
-- 'id', 'cORSRule_id' - Unique identifier for the rule. The value cannot be longer than 255
-- characters.
--
-- 'exposeHeaders', 'cORSRule_exposeHeaders' - One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
--
-- 'maxAgeSeconds', 'cORSRule_maxAgeSeconds' - The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
--
-- 'allowedMethods', 'cORSRule_allowedMethods' - An HTTP method that you allow the origin to execute. Valid values are
-- @GET@, @PUT@, @HEAD@, @POST@, and @DELETE@.
--
-- 'allowedOrigins', 'cORSRule_allowedOrigins' - One or more origins you want customers to be able to access the bucket
-- from.
newCORSRule ::
  CORSRule
newCORSRule =
  CORSRule'
    { allowedHeaders = Prelude.Nothing,
      id = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      maxAgeSeconds = Prelude.Nothing,
      allowedMethods = Prelude.mempty,
      allowedOrigins = Prelude.mempty
    }

-- | Headers that are specified in the @Access-Control-Request-Headers@
-- header. These headers are allowed in a preflight OPTIONS request. In
-- response to any preflight OPTIONS request, Amazon S3 returns any
-- requested headers that are allowed.
cORSRule_allowedHeaders :: Lens.Lens' CORSRule (Prelude.Maybe [Prelude.Text])
cORSRule_allowedHeaders = Lens.lens (\CORSRule' {allowedHeaders} -> allowedHeaders) (\s@CORSRule' {} a -> s {allowedHeaders = a} :: CORSRule) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
cORSRule_id :: Lens.Lens' CORSRule (Prelude.Maybe Prelude.Text)
cORSRule_id = Lens.lens (\CORSRule' {id} -> id) (\s@CORSRule' {} a -> s {id = a} :: CORSRule)

-- | One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
cORSRule_exposeHeaders :: Lens.Lens' CORSRule (Prelude.Maybe [Prelude.Text])
cORSRule_exposeHeaders = Lens.lens (\CORSRule' {exposeHeaders} -> exposeHeaders) (\s@CORSRule' {} a -> s {exposeHeaders = a} :: CORSRule) Prelude.. Lens.mapping Lens.coerced

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
cORSRule_maxAgeSeconds :: Lens.Lens' CORSRule (Prelude.Maybe Prelude.Int)
cORSRule_maxAgeSeconds = Lens.lens (\CORSRule' {maxAgeSeconds} -> maxAgeSeconds) (\s@CORSRule' {} a -> s {maxAgeSeconds = a} :: CORSRule)

-- | An HTTP method that you allow the origin to execute. Valid values are
-- @GET@, @PUT@, @HEAD@, @POST@, and @DELETE@.
cORSRule_allowedMethods :: Lens.Lens' CORSRule [Prelude.Text]
cORSRule_allowedMethods = Lens.lens (\CORSRule' {allowedMethods} -> allowedMethods) (\s@CORSRule' {} a -> s {allowedMethods = a} :: CORSRule) Prelude.. Lens.coerced

-- | One or more origins you want customers to be able to access the bucket
-- from.
cORSRule_allowedOrigins :: Lens.Lens' CORSRule [Prelude.Text]
cORSRule_allowedOrigins = Lens.lens (\CORSRule' {allowedOrigins} -> allowedOrigins) (\s@CORSRule' {} a -> s {allowedOrigins = a} :: CORSRule) Prelude.. Lens.coerced

instance Core.FromXML CORSRule where
  parseXML x =
    CORSRule'
      Prelude.<$> (Core.may (Core.parseXMLList "AllowedHeader") x)
      Prelude.<*> (x Core..@? "ID")
      Prelude.<*> (Core.may (Core.parseXMLList "ExposeHeader") x)
      Prelude.<*> (x Core..@? "MaxAgeSeconds")
      Prelude.<*> (Core.parseXMLList "AllowedMethod" x)
      Prelude.<*> (Core.parseXMLList "AllowedOrigin" x)

instance Prelude.Hashable CORSRule where
  hashWithSalt _salt CORSRule' {..} =
    _salt `Prelude.hashWithSalt` allowedHeaders
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` exposeHeaders
      `Prelude.hashWithSalt` maxAgeSeconds
      `Prelude.hashWithSalt` allowedMethods
      `Prelude.hashWithSalt` allowedOrigins

instance Prelude.NFData CORSRule where
  rnf CORSRule' {..} =
    Prelude.rnf allowedHeaders
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf exposeHeaders
      `Prelude.seq` Prelude.rnf maxAgeSeconds
      `Prelude.seq` Prelude.rnf allowedMethods
      `Prelude.seq` Prelude.rnf allowedOrigins

instance Core.ToXML CORSRule where
  toXML CORSRule' {..} =
    Prelude.mconcat
      [ Core.toXML
          ( Core.toXMLList "AllowedHeader"
              Prelude.<$> allowedHeaders
          ),
        "ID" Core.@= id,
        Core.toXML
          ( Core.toXMLList "ExposeHeader"
              Prelude.<$> exposeHeaders
          ),
        "MaxAgeSeconds" Core.@= maxAgeSeconds,
        Core.toXMLList "AllowedMethod" allowedMethods,
        Core.toXMLList "AllowedOrigin" allowedOrigins
      ]
