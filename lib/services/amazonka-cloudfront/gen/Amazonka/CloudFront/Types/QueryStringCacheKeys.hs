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
-- Module      : Amazonka.CloudFront.Types.QueryStringCacheKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.QueryStringCacheKeys where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use
-- @QueryStringsConfig@ in a cache policy. See @CachePolicy@.
--
-- If you want to send query strings to the origin but not include them in
-- the cache key, use @QueryStringsConfig@ in an origin request policy. See
-- @OriginRequestPolicy@.
--
-- A complex type that contains information about the query string
-- parameters that you want CloudFront to use for caching for a cache
-- behavior.
--
-- /See:/ 'newQueryStringCacheKeys' smart constructor.
data QueryStringCacheKeys = QueryStringCacheKeys'
  { -- | A list that contains the query string parameters that you want
    -- CloudFront to use as a basis for caching for a cache behavior. If
    -- @Quantity@ is 0, you can omit @Items@.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of @whitelisted@ query string parameters for a cache
    -- behavior.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStringCacheKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'queryStringCacheKeys_items' - A list that contains the query string parameters that you want
-- CloudFront to use as a basis for caching for a cache behavior. If
-- @Quantity@ is 0, you can omit @Items@.
--
-- 'quantity', 'queryStringCacheKeys_quantity' - The number of @whitelisted@ query string parameters for a cache
-- behavior.
newQueryStringCacheKeys ::
  -- | 'quantity'
  Prelude.Int ->
  QueryStringCacheKeys
newQueryStringCacheKeys pQuantity_ =
  QueryStringCacheKeys'
    { items = Prelude.Nothing,
      quantity = pQuantity_
    }

-- | A list that contains the query string parameters that you want
-- CloudFront to use as a basis for caching for a cache behavior. If
-- @Quantity@ is 0, you can omit @Items@.
queryStringCacheKeys_items :: Lens.Lens' QueryStringCacheKeys (Prelude.Maybe [Prelude.Text])
queryStringCacheKeys_items = Lens.lens (\QueryStringCacheKeys' {items} -> items) (\s@QueryStringCacheKeys' {} a -> s {items = a} :: QueryStringCacheKeys) Prelude.. Lens.mapping Lens.coerced

-- | The number of @whitelisted@ query string parameters for a cache
-- behavior.
queryStringCacheKeys_quantity :: Lens.Lens' QueryStringCacheKeys Prelude.Int
queryStringCacheKeys_quantity = Lens.lens (\QueryStringCacheKeys' {quantity} -> quantity) (\s@QueryStringCacheKeys' {} a -> s {quantity = a} :: QueryStringCacheKeys)

instance Data.FromXML QueryStringCacheKeys where
  parseXML x =
    QueryStringCacheKeys'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Name")
                  )
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable QueryStringCacheKeys where
  hashWithSalt _salt QueryStringCacheKeys' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData QueryStringCacheKeys where
  rnf QueryStringCacheKeys' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML QueryStringCacheKeys where
  toXML QueryStringCacheKeys' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML (Data.toXMLList "Name" Prelude.<$> items),
        "Quantity" Data.@= quantity
      ]
