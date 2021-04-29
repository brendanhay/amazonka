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
-- Module      : Network.AWS.CloudFront.Types.AllowedMethods
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.AllowedMethods where

import Network.AWS.CloudFront.Types.CachedMethods
import Network.AWS.CloudFront.Types.Method
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are three
-- choices:
--
-- -   CloudFront forwards only @GET@ and @HEAD@ requests.
--
-- -   CloudFront forwards only @GET@, @HEAD@, and @OPTIONS@ requests.
--
-- -   CloudFront forwards @GET, HEAD, OPTIONS, PUT, PATCH, POST@, and
--     @DELETE@ requests.
--
-- If you pick the third choice, you may need to restrict access to your
-- Amazon S3 bucket or to your custom origin so users can\'t perform
-- operations that you don\'t want them to. For example, you might not want
-- users to have permissions to delete objects from your origin.
--
-- /See:/ 'newAllowedMethods' smart constructor.
data AllowedMethods = AllowedMethods'
  { cachedMethods :: Prelude.Maybe CachedMethods,
    -- | The number of HTTP methods that you want CloudFront to forward to your
    -- origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for
    -- @GET@, @HEAD@, and @OPTIONS@ requests) and 7 (for
    -- @GET, HEAD, OPTIONS, PUT, PATCH, POST@, and @DELETE@ requests).
    quantity :: Prelude.Int,
    -- | A complex type that contains the HTTP methods that you want CloudFront
    -- to process and forward to your origin.
    items :: [Method]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AllowedMethods' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachedMethods', 'allowedMethods_cachedMethods' - Undocumented member.
--
-- 'quantity', 'allowedMethods_quantity' - The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for
-- @GET@, @HEAD@, and @OPTIONS@ requests) and 7 (for
-- @GET, HEAD, OPTIONS, PUT, PATCH, POST@, and @DELETE@ requests).
--
-- 'items', 'allowedMethods_items' - A complex type that contains the HTTP methods that you want CloudFront
-- to process and forward to your origin.
newAllowedMethods ::
  -- | 'quantity'
  Prelude.Int ->
  AllowedMethods
newAllowedMethods pQuantity_ =
  AllowedMethods'
    { cachedMethods = Prelude.Nothing,
      quantity = pQuantity_,
      items = Prelude.mempty
    }

-- | Undocumented member.
allowedMethods_cachedMethods :: Lens.Lens' AllowedMethods (Prelude.Maybe CachedMethods)
allowedMethods_cachedMethods = Lens.lens (\AllowedMethods' {cachedMethods} -> cachedMethods) (\s@AllowedMethods' {} a -> s {cachedMethods = a} :: AllowedMethods)

-- | The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for
-- @GET@, @HEAD@, and @OPTIONS@ requests) and 7 (for
-- @GET, HEAD, OPTIONS, PUT, PATCH, POST@, and @DELETE@ requests).
allowedMethods_quantity :: Lens.Lens' AllowedMethods Prelude.Int
allowedMethods_quantity = Lens.lens (\AllowedMethods' {quantity} -> quantity) (\s@AllowedMethods' {} a -> s {quantity = a} :: AllowedMethods)

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to process and forward to your origin.
allowedMethods_items :: Lens.Lens' AllowedMethods [Method]
allowedMethods_items = Lens.lens (\AllowedMethods' {items} -> items) (\s@AllowedMethods' {} a -> s {items = a} :: AllowedMethods) Prelude.. Prelude._Coerce

instance Prelude.FromXML AllowedMethods where
  parseXML x =
    AllowedMethods'
      Prelude.<$> (x Prelude..@? "CachedMethods")
      Prelude.<*> (x Prelude..@ "Quantity")
      Prelude.<*> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "Method"
                  )

instance Prelude.Hashable AllowedMethods

instance Prelude.NFData AllowedMethods

instance Prelude.ToXML AllowedMethods where
  toXML AllowedMethods' {..} =
    Prelude.mconcat
      [ "CachedMethods" Prelude.@= cachedMethods,
        "Quantity" Prelude.@= quantity,
        "Items" Prelude.@= Prelude.toXMLList "Method" items
      ]
