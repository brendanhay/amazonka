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
-- Module      : Network.AWS.CloudFront.Types.TrustedKeyGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TrustedKeyGroups where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of key groups whose public keys CloudFront can use to verify the
-- signatures of signed URLs and signed cookies.
--
-- /See:/ 'newTrustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { -- | A list of key groups identifiers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | This field is @true@ if any of the key groups in the list have public
    -- keys that CloudFront can use to verify the signatures of signed URLs and
    -- signed cookies. If not, this field is @false@.
    enabled :: Prelude.Bool,
    -- | The number of key groups in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrustedKeyGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'trustedKeyGroups_items' - A list of key groups identifiers.
--
-- 'enabled', 'trustedKeyGroups_enabled' - This field is @true@ if any of the key groups in the list have public
-- keys that CloudFront can use to verify the signatures of signed URLs and
-- signed cookies. If not, this field is @false@.
--
-- 'quantity', 'trustedKeyGroups_quantity' - The number of key groups in the list.
newTrustedKeyGroups ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  TrustedKeyGroups
newTrustedKeyGroups pEnabled_ pQuantity_ =
  TrustedKeyGroups'
    { items = Prelude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of key groups identifiers.
trustedKeyGroups_items :: Lens.Lens' TrustedKeyGroups (Prelude.Maybe [Prelude.Text])
trustedKeyGroups_items = Lens.lens (\TrustedKeyGroups' {items} -> items) (\s@TrustedKeyGroups' {} a -> s {items = a} :: TrustedKeyGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | This field is @true@ if any of the key groups in the list have public
-- keys that CloudFront can use to verify the signatures of signed URLs and
-- signed cookies. If not, this field is @false@.
trustedKeyGroups_enabled :: Lens.Lens' TrustedKeyGroups Prelude.Bool
trustedKeyGroups_enabled = Lens.lens (\TrustedKeyGroups' {enabled} -> enabled) (\s@TrustedKeyGroups' {} a -> s {enabled = a} :: TrustedKeyGroups)

-- | The number of key groups in the list.
trustedKeyGroups_quantity :: Lens.Lens' TrustedKeyGroups Prelude.Int
trustedKeyGroups_quantity = Lens.lens (\TrustedKeyGroups' {quantity} -> quantity) (\s@TrustedKeyGroups' {} a -> s {quantity = a} :: TrustedKeyGroups)

instance Prelude.FromXML TrustedKeyGroups where
  parseXML x =
    TrustedKeyGroups'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "KeyGroup")
                  )
      Prelude.<*> (x Prelude..@ "Enabled")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable TrustedKeyGroups

instance Prelude.NFData TrustedKeyGroups

instance Prelude.ToXML TrustedKeyGroups where
  toXML TrustedKeyGroups' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "KeyGroup" Prelude.<$> items),
        "Enabled" Prelude.@= enabled,
        "Quantity" Prelude.@= quantity
      ]
