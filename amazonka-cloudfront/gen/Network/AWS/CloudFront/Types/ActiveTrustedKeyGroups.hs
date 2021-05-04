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
-- Module      : Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups where

import Network.AWS.CloudFront.Types.KGKeyPairIds
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of key groups, and the public keys in each key group, that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies.
--
-- /See:/ 'newActiveTrustedKeyGroups' smart constructor.
data ActiveTrustedKeyGroups = ActiveTrustedKeyGroups'
  { -- | A list of key groups, including the identifiers of the public keys in
    -- each key group that CloudFront can use to verify the signatures of
    -- signed URLs and signed cookies.
    items :: Prelude.Maybe [KGKeyPairIds],
    -- | This field is @true@ if any of the key groups have public keys that
    -- CloudFront can use to verify the signatures of signed URLs and signed
    -- cookies. If not, this field is @false@.
    enabled :: Prelude.Bool,
    -- | The number of key groups in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActiveTrustedKeyGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'activeTrustedKeyGroups_items' - A list of key groups, including the identifiers of the public keys in
-- each key group that CloudFront can use to verify the signatures of
-- signed URLs and signed cookies.
--
-- 'enabled', 'activeTrustedKeyGroups_enabled' - This field is @true@ if any of the key groups have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
--
-- 'quantity', 'activeTrustedKeyGroups_quantity' - The number of key groups in the list.
newActiveTrustedKeyGroups ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  ActiveTrustedKeyGroups
newActiveTrustedKeyGroups pEnabled_ pQuantity_ =
  ActiveTrustedKeyGroups'
    { items = Prelude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of key groups, including the identifiers of the public keys in
-- each key group that CloudFront can use to verify the signatures of
-- signed URLs and signed cookies.
activeTrustedKeyGroups_items :: Lens.Lens' ActiveTrustedKeyGroups (Prelude.Maybe [KGKeyPairIds])
activeTrustedKeyGroups_items = Lens.lens (\ActiveTrustedKeyGroups' {items} -> items) (\s@ActiveTrustedKeyGroups' {} a -> s {items = a} :: ActiveTrustedKeyGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | This field is @true@ if any of the key groups have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
activeTrustedKeyGroups_enabled :: Lens.Lens' ActiveTrustedKeyGroups Prelude.Bool
activeTrustedKeyGroups_enabled = Lens.lens (\ActiveTrustedKeyGroups' {enabled} -> enabled) (\s@ActiveTrustedKeyGroups' {} a -> s {enabled = a} :: ActiveTrustedKeyGroups)

-- | The number of key groups in the list.
activeTrustedKeyGroups_quantity :: Lens.Lens' ActiveTrustedKeyGroups Prelude.Int
activeTrustedKeyGroups_quantity = Lens.lens (\ActiveTrustedKeyGroups' {quantity} -> quantity) (\s@ActiveTrustedKeyGroups' {} a -> s {quantity = a} :: ActiveTrustedKeyGroups)

instance Prelude.FromXML ActiveTrustedKeyGroups where
  parseXML x =
    ActiveTrustedKeyGroups'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "KeyGroup")
                  )
      Prelude.<*> (x Prelude..@ "Enabled")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable ActiveTrustedKeyGroups

instance Prelude.NFData ActiveTrustedKeyGroups
