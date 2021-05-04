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
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyList where

import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of origin request policies.
--
-- /See:/ 'newOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { -- | Contains the origin request policies in the list.
    items :: Prelude.Maybe [OriginRequestPolicySummary],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing origin
    -- request policies where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of origin request policies requested.
    maxItems :: Prelude.Int,
    -- | The total number of origin request policies returned in the response.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OriginRequestPolicyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'originRequestPolicyList_items' - Contains the origin request policies in the list.
--
-- 'nextMarker', 'originRequestPolicyList_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing origin
-- request policies where you left off.
--
-- 'maxItems', 'originRequestPolicyList_maxItems' - The maximum number of origin request policies requested.
--
-- 'quantity', 'originRequestPolicyList_quantity' - The total number of origin request policies returned in the response.
newOriginRequestPolicyList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  OriginRequestPolicyList
newOriginRequestPolicyList pMaxItems_ pQuantity_ =
  OriginRequestPolicyList'
    { items = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | Contains the origin request policies in the list.
originRequestPolicyList_items :: Lens.Lens' OriginRequestPolicyList (Prelude.Maybe [OriginRequestPolicySummary])
originRequestPolicyList_items = Lens.lens (\OriginRequestPolicyList' {items} -> items) (\s@OriginRequestPolicyList' {} a -> s {items = a} :: OriginRequestPolicyList) Prelude.. Lens.mapping Prelude._Coerce

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing origin
-- request policies where you left off.
originRequestPolicyList_nextMarker :: Lens.Lens' OriginRequestPolicyList (Prelude.Maybe Prelude.Text)
originRequestPolicyList_nextMarker = Lens.lens (\OriginRequestPolicyList' {nextMarker} -> nextMarker) (\s@OriginRequestPolicyList' {} a -> s {nextMarker = a} :: OriginRequestPolicyList)

-- | The maximum number of origin request policies requested.
originRequestPolicyList_maxItems :: Lens.Lens' OriginRequestPolicyList Prelude.Int
originRequestPolicyList_maxItems = Lens.lens (\OriginRequestPolicyList' {maxItems} -> maxItems) (\s@OriginRequestPolicyList' {} a -> s {maxItems = a} :: OriginRequestPolicyList)

-- | The total number of origin request policies returned in the response.
originRequestPolicyList_quantity :: Lens.Lens' OriginRequestPolicyList Prelude.Int
originRequestPolicyList_quantity = Lens.lens (\OriginRequestPolicyList' {quantity} -> quantity) (\s@OriginRequestPolicyList' {} a -> s {quantity = a} :: OriginRequestPolicyList)

instance Prelude.FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "OriginRequestPolicySummary")
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable OriginRequestPolicyList

instance Prelude.NFData OriginRequestPolicyList
