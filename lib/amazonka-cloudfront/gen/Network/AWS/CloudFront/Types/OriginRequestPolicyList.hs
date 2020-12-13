{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyList
  ( OriginRequestPolicyList (..),

    -- * Smart constructor
    mkOriginRequestPolicyList,

    -- * Lenses
    orplQuantity,
    orplItems,
    orplMaxItems,
    orplNextMarker,
  )
where

import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of origin request policies.
--
-- /See:/ 'mkOriginRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { -- | The total number of origin request policies returned in the response.
    quantity :: Lude.Int,
    -- | Contains the origin request policies in the list.
    items :: Lude.Maybe [OriginRequestPolicySummary],
    -- | The maximum number of origin request policies requested.
    maxItems :: Lude.Int,
    -- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
    nextMarker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicyList' with the minimum fields required to make a request.
--
-- * 'quantity' - The total number of origin request policies returned in the response.
-- * 'items' - Contains the origin request policies in the list.
-- * 'maxItems' - The maximum number of origin request policies requested.
-- * 'nextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
mkOriginRequestPolicyList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'maxItems'
  Lude.Int ->
  OriginRequestPolicyList
mkOriginRequestPolicyList pQuantity_ pMaxItems_ =
  OriginRequestPolicyList'
    { quantity = pQuantity_,
      items = Lude.Nothing,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing
    }

-- | The total number of origin request policies returned in the response.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplQuantity :: Lens.Lens' OriginRequestPolicyList Lude.Int
orplQuantity = Lens.lens (quantity :: OriginRequestPolicyList -> Lude.Int) (\s a -> s {quantity = a} :: OriginRequestPolicyList)
{-# DEPRECATED orplQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Contains the origin request policies in the list.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplItems :: Lens.Lens' OriginRequestPolicyList (Lude.Maybe [OriginRequestPolicySummary])
orplItems = Lens.lens (items :: OriginRequestPolicyList -> Lude.Maybe [OriginRequestPolicySummary]) (\s a -> s {items = a} :: OriginRequestPolicyList)
{-# DEPRECATED orplItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The maximum number of origin request policies requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplMaxItems :: Lens.Lens' OriginRequestPolicyList Lude.Int
orplMaxItems = Lens.lens (maxItems :: OriginRequestPolicyList -> Lude.Int) (\s a -> s {maxItems = a} :: OriginRequestPolicyList)
{-# DEPRECATED orplMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orplNextMarker :: Lens.Lens' OriginRequestPolicyList (Lude.Maybe Lude.Text)
orplNextMarker = Lens.lens (nextMarker :: OriginRequestPolicyList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: OriginRequestPolicyList)
{-# DEPRECATED orplNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Lude.FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OriginRequestPolicySummary")
               )
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
