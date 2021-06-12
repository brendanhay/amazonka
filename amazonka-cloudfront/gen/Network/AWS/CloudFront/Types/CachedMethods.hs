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
-- Module      : Network.AWS.CloudFront.Types.CachedMethods
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachedMethods where

import Network.AWS.CloudFront.Types.Method
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    quantity :: Core.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront
    -- to cache responses to.
    items :: [Method]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CachedMethods
newCachedMethods pQuantity_ =
  CachedMethods'
    { quantity = pQuantity_,
      items = Core.mempty
    }

-- | The number of HTTP methods for which you want CloudFront to cache
-- responses. Valid values are @2@ (for caching responses to @GET@ and
-- @HEAD@ requests) and @3@ (for caching responses to @GET@, @HEAD@, and
-- @OPTIONS@ requests).
cachedMethods_quantity :: Lens.Lens' CachedMethods Core.Int
cachedMethods_quantity = Lens.lens (\CachedMethods' {quantity} -> quantity) (\s@CachedMethods' {} a -> s {quantity = a} :: CachedMethods)

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to cache responses to.
cachedMethods_items :: Lens.Lens' CachedMethods [Method]
cachedMethods_items = Lens.lens (\CachedMethods' {items} -> items) (\s@CachedMethods' {} a -> s {items = a} :: CachedMethods) Core.. Lens._Coerce

instance Core.FromXML CachedMethods where
  parseXML x =
    CachedMethods'
      Core.<$> (x Core..@ "Quantity")
      Core.<*> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList "Method"
               )

instance Core.Hashable CachedMethods

instance Core.NFData CachedMethods

instance Core.ToXML CachedMethods where
  toXML CachedMethods' {..} =
    Core.mconcat
      [ "Quantity" Core.@= quantity,
        "Items" Core.@= Core.toXMLList "Method" items
      ]
