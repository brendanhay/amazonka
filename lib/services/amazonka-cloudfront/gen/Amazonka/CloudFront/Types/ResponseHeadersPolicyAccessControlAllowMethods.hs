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
-- Module      : Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethods
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethods where

import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethodsValues
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of HTTP methods that CloudFront includes as values for the
-- @Access-Control-Allow-Methods@ HTTP response header.
--
-- For more information about the @Access-Control-Allow-Methods@ HTTP
-- response header, see
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Methods Access-Control-Allow-Methods>
-- in the MDN Web Docs.
--
-- /See:/ 'newResponseHeadersPolicyAccessControlAllowMethods' smart constructor.
data ResponseHeadersPolicyAccessControlAllowMethods = ResponseHeadersPolicyAccessControlAllowMethods'
  { -- | The number of HTTP methods in the list.
    quantity :: Prelude.Int,
    -- | The list of HTTP methods. Valid values are:
    --
    -- -   @GET@
    --
    -- -   @DELETE@
    --
    -- -   @HEAD@
    --
    -- -   @OPTIONS@
    --
    -- -   @PATCH@
    --
    -- -   @POST@
    --
    -- -   @PUT@
    --
    -- -   @ALL@
    --
    -- @ALL@ is a special value that includes all of the listed HTTP methods.
    items :: [ResponseHeadersPolicyAccessControlAllowMethodsValues]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseHeadersPolicyAccessControlAllowMethods' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quantity', 'responseHeadersPolicyAccessControlAllowMethods_quantity' - The number of HTTP methods in the list.
--
-- 'items', 'responseHeadersPolicyAccessControlAllowMethods_items' - The list of HTTP methods. Valid values are:
--
-- -   @GET@
--
-- -   @DELETE@
--
-- -   @HEAD@
--
-- -   @OPTIONS@
--
-- -   @PATCH@
--
-- -   @POST@
--
-- -   @PUT@
--
-- -   @ALL@
--
-- @ALL@ is a special value that includes all of the listed HTTP methods.
newResponseHeadersPolicyAccessControlAllowMethods ::
  -- | 'quantity'
  Prelude.Int ->
  ResponseHeadersPolicyAccessControlAllowMethods
newResponseHeadersPolicyAccessControlAllowMethods
  pQuantity_ =
    ResponseHeadersPolicyAccessControlAllowMethods'
      { quantity =
          pQuantity_,
        items = Prelude.mempty
      }

-- | The number of HTTP methods in the list.
responseHeadersPolicyAccessControlAllowMethods_quantity :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowMethods Prelude.Int
responseHeadersPolicyAccessControlAllowMethods_quantity = Lens.lens (\ResponseHeadersPolicyAccessControlAllowMethods' {quantity} -> quantity) (\s@ResponseHeadersPolicyAccessControlAllowMethods' {} a -> s {quantity = a} :: ResponseHeadersPolicyAccessControlAllowMethods)

-- | The list of HTTP methods. Valid values are:
--
-- -   @GET@
--
-- -   @DELETE@
--
-- -   @HEAD@
--
-- -   @OPTIONS@
--
-- -   @PATCH@
--
-- -   @POST@
--
-- -   @PUT@
--
-- -   @ALL@
--
-- @ALL@ is a special value that includes all of the listed HTTP methods.
responseHeadersPolicyAccessControlAllowMethods_items :: Lens.Lens' ResponseHeadersPolicyAccessControlAllowMethods [ResponseHeadersPolicyAccessControlAllowMethodsValues]
responseHeadersPolicyAccessControlAllowMethods_items = Lens.lens (\ResponseHeadersPolicyAccessControlAllowMethods' {items} -> items) (\s@ResponseHeadersPolicyAccessControlAllowMethods' {} a -> s {items = a} :: ResponseHeadersPolicyAccessControlAllowMethods) Prelude.. Lens.coerced

instance
  Data.FromXML
    ResponseHeadersPolicyAccessControlAllowMethods
  where
  parseXML x =
    ResponseHeadersPolicyAccessControlAllowMethods'
      Prelude.<$> (x Data..@ "Quantity")
      Prelude.<*> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "Method"
                  )

instance
  Prelude.Hashable
    ResponseHeadersPolicyAccessControlAllowMethods
  where
  hashWithSalt
    _salt
    ResponseHeadersPolicyAccessControlAllowMethods' {..} =
      _salt
        `Prelude.hashWithSalt` quantity
        `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    ResponseHeadersPolicyAccessControlAllowMethods
  where
  rnf
    ResponseHeadersPolicyAccessControlAllowMethods' {..} =
      Prelude.rnf quantity
        `Prelude.seq` Prelude.rnf items

instance
  Data.ToXML
    ResponseHeadersPolicyAccessControlAllowMethods
  where
  toXML
    ResponseHeadersPolicyAccessControlAllowMethods' {..} =
      Prelude.mconcat
        [ "Quantity" Data.@= quantity,
          "Items" Data.@= Data.toXMLList "Method" items
        ]
