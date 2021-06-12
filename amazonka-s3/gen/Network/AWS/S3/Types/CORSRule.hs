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
-- Module      : Network.AWS.S3.Types.CORSRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CORSRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | Specifies a cross-origin access rule for an Amazon S3 bucket.
--
-- /See:/ 'newCORSRule' smart constructor.
data CORSRule = CORSRule'
  { -- | Headers that are specified in the @Access-Control-Request-Headers@
    -- header. These headers are allowed in a preflight OPTIONS request. In
    -- response to any preflight OPTIONS request, Amazon S3 returns any
    -- requested headers that are allowed.
    allowedHeaders :: Core.Maybe [Core.Text],
    -- | The time in seconds that your browser is to cache the preflight response
    -- for the specified resource.
    maxAgeSeconds :: Core.Maybe Core.Int,
    -- | One or more headers in the response that you want customers to be able
    -- to access from their applications (for example, from a JavaScript
    -- @XMLHttpRequest@ object).
    exposeHeaders :: Core.Maybe [Core.Text],
    -- | An HTTP method that you allow the origin to execute. Valid values are
    -- @GET@, @PUT@, @HEAD@, @POST@, and @DELETE@.
    allowedMethods :: [Core.Text],
    -- | One or more origins you want customers to be able to access the bucket
    -- from.
    allowedOrigins :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxAgeSeconds', 'cORSRule_maxAgeSeconds' - The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
--
-- 'exposeHeaders', 'cORSRule_exposeHeaders' - One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
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
    { allowedHeaders = Core.Nothing,
      maxAgeSeconds = Core.Nothing,
      exposeHeaders = Core.Nothing,
      allowedMethods = Core.mempty,
      allowedOrigins = Core.mempty
    }

-- | Headers that are specified in the @Access-Control-Request-Headers@
-- header. These headers are allowed in a preflight OPTIONS request. In
-- response to any preflight OPTIONS request, Amazon S3 returns any
-- requested headers that are allowed.
cORSRule_allowedHeaders :: Lens.Lens' CORSRule (Core.Maybe [Core.Text])
cORSRule_allowedHeaders = Lens.lens (\CORSRule' {allowedHeaders} -> allowedHeaders) (\s@CORSRule' {} a -> s {allowedHeaders = a} :: CORSRule) Core.. Lens.mapping Lens._Coerce

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
cORSRule_maxAgeSeconds :: Lens.Lens' CORSRule (Core.Maybe Core.Int)
cORSRule_maxAgeSeconds = Lens.lens (\CORSRule' {maxAgeSeconds} -> maxAgeSeconds) (\s@CORSRule' {} a -> s {maxAgeSeconds = a} :: CORSRule)

-- | One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
cORSRule_exposeHeaders :: Lens.Lens' CORSRule (Core.Maybe [Core.Text])
cORSRule_exposeHeaders = Lens.lens (\CORSRule' {exposeHeaders} -> exposeHeaders) (\s@CORSRule' {} a -> s {exposeHeaders = a} :: CORSRule) Core.. Lens.mapping Lens._Coerce

-- | An HTTP method that you allow the origin to execute. Valid values are
-- @GET@, @PUT@, @HEAD@, @POST@, and @DELETE@.
cORSRule_allowedMethods :: Lens.Lens' CORSRule [Core.Text]
cORSRule_allowedMethods = Lens.lens (\CORSRule' {allowedMethods} -> allowedMethods) (\s@CORSRule' {} a -> s {allowedMethods = a} :: CORSRule) Core.. Lens._Coerce

-- | One or more origins you want customers to be able to access the bucket
-- from.
cORSRule_allowedOrigins :: Lens.Lens' CORSRule [Core.Text]
cORSRule_allowedOrigins = Lens.lens (\CORSRule' {allowedOrigins} -> allowedOrigins) (\s@CORSRule' {} a -> s {allowedOrigins = a} :: CORSRule) Core.. Lens._Coerce

instance Core.FromXML CORSRule where
  parseXML x =
    CORSRule'
      Core.<$> (Core.may (Core.parseXMLList "AllowedHeader") x)
      Core.<*> (x Core..@? "MaxAgeSeconds")
      Core.<*> (Core.may (Core.parseXMLList "ExposeHeader") x)
      Core.<*> (Core.parseXMLList "AllowedMethod" x)
      Core.<*> (Core.parseXMLList "AllowedOrigin" x)

instance Core.Hashable CORSRule

instance Core.NFData CORSRule

instance Core.ToXML CORSRule where
  toXML CORSRule' {..} =
    Core.mconcat
      [ Core.toXML
          ( Core.toXMLList "AllowedHeader"
              Core.<$> allowedHeaders
          ),
        "MaxAgeSeconds" Core.@= maxAgeSeconds,
        Core.toXML
          ( Core.toXMLList "ExposeHeader"
              Core.<$> exposeHeaders
          ),
        Core.toXMLList "AllowedMethod" allowedMethods,
        Core.toXMLList "AllowedOrigin" allowedOrigins
      ]
