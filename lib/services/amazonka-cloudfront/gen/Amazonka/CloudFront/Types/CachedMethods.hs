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
-- Module      : Amazonka.CloudFront.Types.CachedMethods
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CachedMethods where

import Amazonka.CloudFront.Types.Method
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that controls whether CloudFront caches the response to
-- requests using the specified HTTP methods. There are two choices:
--
-- -   CloudFront caches responses to @GET@ and @HEAD@ requests.
--
-- -   CloudFront caches responses to @GET@, @HEAD@, and @OPTIONS@
--     requests.
--
-- If you pick the second choice for your Amazon S3 Origin, you may need to
-- forward Access-Control-Request-Method, Access-Control-Request-Headers,
-- and Origin headers for the responses to be cached correctly.
--
-- /See:/ 'newCachedMethods' smart constructor.
data CachedMethods = CachedMethods'
  { -- | The number of HTTP methods for which you want CloudFront to cache
    -- responses. Valid values are @2@ (for caching responses to @GET@ and
    -- @HEAD@ requests) and @3@ (for caching responses to @GET@, @HEAD@, and
    -- @OPTIONS@ requests).
    quantity :: Prelude.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront
    -- to cache responses to.
    items :: [Method]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachedMethods' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'cachedMethods_quantity' - The number of HTTP methods for which you want CloudFront to cache
-- responses. Valid values are @2@ (for caching responses to @GET@ and
-- @HEAD@ requests) and @3@ (for caching responses to @GET@, @HEAD@, and
-- @OPTIONS@ requests).
--
-- 'items', 'cachedMethods_items' - A complex type that contains the HTTP methods that you want CloudFront
-- to cache responses to.
newCachedMethods ::
  -- | 'quantity'
  Prelude.Int ->
  CachedMethods
newCachedMethods pQuantity_ =
  CachedMethods'
    { quantity = pQuantity_,
      items = Prelude.mempty
    }

-- | The number of HTTP methods for which you want CloudFront to cache
-- responses. Valid values are @2@ (for caching responses to @GET@ and
-- @HEAD@ requests) and @3@ (for caching responses to @GET@, @HEAD@, and
-- @OPTIONS@ requests).
cachedMethods_quantity :: Lens.Lens' CachedMethods Prelude.Int
cachedMethods_quantity = Lens.lens (\CachedMethods' {quantity} -> quantity) (\s@CachedMethods' {} a -> s {quantity = a} :: CachedMethods)

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to cache responses to.
cachedMethods_items :: Lens.Lens' CachedMethods [Method]
cachedMethods_items = Lens.lens (\CachedMethods' {items} -> items) (\s@CachedMethods' {} a -> s {items = a} :: CachedMethods) Prelude.. Lens.coerced

instance Data.FromXML CachedMethods where
  parseXML x =
    CachedMethods'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "Method"
                  )

instance Prelude.Hashable CachedMethods where
  hashWithSalt _salt CachedMethods' {..} =
    _salt
      `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` items

instance Prelude.NFData CachedMethods where
  rnf CachedMethods' {..} =
    Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf items

instance Data.ToXML CachedMethods where
  toXML CachedMethods' {..} =
    Prelude.mconcat
      [ "Quantity" Data.@= quantity,
        "Items" Data.@= Data.toXMLList "Method" items
      ]
