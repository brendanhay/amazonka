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
-- Module      : Network.AWS.CloudFront.Types.TrustedSigners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedSigners where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of AWS accounts whose public keys CloudFront can use to verify
-- the signatures of signed URLs and signed cookies.
--
-- /See:/ 'newTrustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { -- | A list of AWS account identifiers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | This field is @true@ if any of the AWS accounts have public keys that
    -- CloudFront can use to verify the signatures of signed URLs and signed
    -- cookies. If not, this field is @false@.
    enabled :: Prelude.Bool,
    -- | The number of AWS accounts in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrustedSigners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'trustedSigners_items' - A list of AWS account identifiers.
--
-- 'enabled', 'trustedSigners_enabled' - This field is @true@ if any of the AWS accounts have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
--
-- 'quantity', 'trustedSigners_quantity' - The number of AWS accounts in the list.
newTrustedSigners ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  TrustedSigners
newTrustedSigners pEnabled_ pQuantity_ =
  TrustedSigners'
    { items = Prelude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of AWS account identifiers.
trustedSigners_items :: Lens.Lens' TrustedSigners (Prelude.Maybe [Prelude.Text])
trustedSigners_items = Lens.lens (\TrustedSigners' {items} -> items) (\s@TrustedSigners' {} a -> s {items = a} :: TrustedSigners) Prelude.. Lens.mapping Prelude._Coerce

-- | This field is @true@ if any of the AWS accounts have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
trustedSigners_enabled :: Lens.Lens' TrustedSigners Prelude.Bool
trustedSigners_enabled = Lens.lens (\TrustedSigners' {enabled} -> enabled) (\s@TrustedSigners' {} a -> s {enabled = a} :: TrustedSigners)

-- | The number of AWS accounts in the list.
trustedSigners_quantity :: Lens.Lens' TrustedSigners Prelude.Int
trustedSigners_quantity = Lens.lens (\TrustedSigners' {quantity} -> quantity) (\s@TrustedSigners' {} a -> s {quantity = a} :: TrustedSigners)

instance Prelude.FromXML TrustedSigners where
  parseXML x =
    TrustedSigners'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "AwsAccountNumber")
                  )
      Prelude.<*> (x Prelude..@ "Enabled")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable TrustedSigners

instance Prelude.NFData TrustedSigners

instance Prelude.ToXML TrustedSigners where
  toXML TrustedSigners' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            ( Prelude.toXMLList "AwsAccountNumber"
                Prelude.<$> items
            ),
        "Enabled" Prelude.@= enabled,
        "Quantity" Prelude.@= quantity
      ]
