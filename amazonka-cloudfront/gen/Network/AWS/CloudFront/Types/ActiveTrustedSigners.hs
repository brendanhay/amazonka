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
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedSigners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedSigners where

import Network.AWS.CloudFront.Types.Signer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of AWS accounts and the active CloudFront key pairs in each
-- account that CloudFront can use to verify the signatures of signed URLs
-- and signed cookies.
--
-- /See:/ 'newActiveTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
  { -- | A list of AWS accounts and the identifiers of active CloudFront key
    -- pairs in each account that CloudFront can use to verify the signatures
    -- of signed URLs and signed cookies.
    items :: Prelude.Maybe [Signer],
    -- | This field is @true@ if any of the AWS accounts in the list have active
    -- CloudFront key pairs that CloudFront can use to verify the signatures of
    -- signed URLs and signed cookies. If not, this field is @false@.
    enabled :: Prelude.Bool,
    -- | The number of AWS accounts in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActiveTrustedSigners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'activeTrustedSigners_items' - A list of AWS accounts and the identifiers of active CloudFront key
-- pairs in each account that CloudFront can use to verify the signatures
-- of signed URLs and signed cookies.
--
-- 'enabled', 'activeTrustedSigners_enabled' - This field is @true@ if any of the AWS accounts in the list have active
-- CloudFront key pairs that CloudFront can use to verify the signatures of
-- signed URLs and signed cookies. If not, this field is @false@.
--
-- 'quantity', 'activeTrustedSigners_quantity' - The number of AWS accounts in the list.
newActiveTrustedSigners ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  ActiveTrustedSigners
newActiveTrustedSigners pEnabled_ pQuantity_ =
  ActiveTrustedSigners'
    { items = Prelude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of AWS accounts and the identifiers of active CloudFront key
-- pairs in each account that CloudFront can use to verify the signatures
-- of signed URLs and signed cookies.
activeTrustedSigners_items :: Lens.Lens' ActiveTrustedSigners (Prelude.Maybe [Signer])
activeTrustedSigners_items = Lens.lens (\ActiveTrustedSigners' {items} -> items) (\s@ActiveTrustedSigners' {} a -> s {items = a} :: ActiveTrustedSigners) Prelude.. Lens.mapping Prelude._Coerce

-- | This field is @true@ if any of the AWS accounts in the list have active
-- CloudFront key pairs that CloudFront can use to verify the signatures of
-- signed URLs and signed cookies. If not, this field is @false@.
activeTrustedSigners_enabled :: Lens.Lens' ActiveTrustedSigners Prelude.Bool
activeTrustedSigners_enabled = Lens.lens (\ActiveTrustedSigners' {enabled} -> enabled) (\s@ActiveTrustedSigners' {} a -> s {enabled = a} :: ActiveTrustedSigners)

-- | The number of AWS accounts in the list.
activeTrustedSigners_quantity :: Lens.Lens' ActiveTrustedSigners Prelude.Int
activeTrustedSigners_quantity = Lens.lens (\ActiveTrustedSigners' {quantity} -> quantity) (\s@ActiveTrustedSigners' {} a -> s {quantity = a} :: ActiveTrustedSigners)

instance Prelude.FromXML ActiveTrustedSigners where
  parseXML x =
    ActiveTrustedSigners'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Signer")
                  )
      Prelude.<*> (x Prelude..@ "Enabled")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable ActiveTrustedSigners

instance Prelude.NFData ActiveTrustedSigners
