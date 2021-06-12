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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of key groups whose public keys CloudFront can use to verify the
-- signatures of signed URLs and signed cookies.
--
-- /See:/ 'newTrustedKeyGroups' smart constructor.
data TrustedKeyGroups = TrustedKeyGroups'
  { -- | A list of key groups identifiers.
    items :: Core.Maybe [Core.Text],
    -- | This field is @true@ if any of the key groups in the list have public
    -- keys that CloudFront can use to verify the signatures of signed URLs and
    -- signed cookies. If not, this field is @false@.
    enabled :: Core.Bool,
    -- | The number of key groups in the list.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Bool ->
  -- | 'quantity'
  Core.Int ->
  TrustedKeyGroups
newTrustedKeyGroups pEnabled_ pQuantity_ =
  TrustedKeyGroups'
    { items = Core.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of key groups identifiers.
trustedKeyGroups_items :: Lens.Lens' TrustedKeyGroups (Core.Maybe [Core.Text])
trustedKeyGroups_items = Lens.lens (\TrustedKeyGroups' {items} -> items) (\s@TrustedKeyGroups' {} a -> s {items = a} :: TrustedKeyGroups) Core.. Lens.mapping Lens._Coerce

-- | This field is @true@ if any of the key groups in the list have public
-- keys that CloudFront can use to verify the signatures of signed URLs and
-- signed cookies. If not, this field is @false@.
trustedKeyGroups_enabled :: Lens.Lens' TrustedKeyGroups Core.Bool
trustedKeyGroups_enabled = Lens.lens (\TrustedKeyGroups' {enabled} -> enabled) (\s@TrustedKeyGroups' {} a -> s {enabled = a} :: TrustedKeyGroups)

-- | The number of key groups in the list.
trustedKeyGroups_quantity :: Lens.Lens' TrustedKeyGroups Core.Int
trustedKeyGroups_quantity = Lens.lens (\TrustedKeyGroups' {quantity} -> quantity) (\s@TrustedKeyGroups' {} a -> s {quantity = a} :: TrustedKeyGroups)

instance Core.FromXML TrustedKeyGroups where
  parseXML x =
    TrustedKeyGroups'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "KeyGroup")
               )
      Core.<*> (x Core..@ "Enabled")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable TrustedKeyGroups

instance Core.NFData TrustedKeyGroups

instance Core.ToXML TrustedKeyGroups where
  toXML TrustedKeyGroups' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "KeyGroup" Core.<$> items),
        "Enabled" Core.@= enabled,
        "Quantity" Core.@= quantity
      ]
