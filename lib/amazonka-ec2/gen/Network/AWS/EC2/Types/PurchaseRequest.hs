{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PurchaseRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PurchaseRequest
  ( PurchaseRequest (..),

    -- * Smart constructor
    mkPurchaseRequest,

    -- * Lenses
    prInstanceCount,
    prPurchaseToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a request to purchase Scheduled Instances.
--
-- /See:/ 'mkPurchaseRequest' smart constructor.
data PurchaseRequest = PurchaseRequest'
  { -- | The number of instances.
    instanceCount :: Lude.Int,
    -- | The purchase token.
    purchaseToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseRequest' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of instances.
-- * 'purchaseToken' - The purchase token.
mkPurchaseRequest ::
  -- | 'instanceCount'
  Lude.Int ->
  -- | 'purchaseToken'
  Lude.Text ->
  PurchaseRequest
mkPurchaseRequest pInstanceCount_ pPurchaseToken_ =
  PurchaseRequest'
    { instanceCount = pInstanceCount_,
      purchaseToken = pPurchaseToken_
    }

-- | The number of instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prInstanceCount :: Lens.Lens' PurchaseRequest Lude.Int
prInstanceCount = Lens.lens (instanceCount :: PurchaseRequest -> Lude.Int) (\s a -> s {instanceCount = a} :: PurchaseRequest)
{-# DEPRECATED prInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The purchase token.
--
-- /Note:/ Consider using 'purchaseToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPurchaseToken :: Lens.Lens' PurchaseRequest Lude.Text
prPurchaseToken = Lens.lens (purchaseToken :: PurchaseRequest -> Lude.Text) (\s a -> s {purchaseToken = a} :: PurchaseRequest)
{-# DEPRECATED prPurchaseToken "Use generic-lens or generic-optics with 'purchaseToken' instead." #-}

instance Lude.ToQuery PurchaseRequest where
  toQuery PurchaseRequest' {..} =
    Lude.mconcat
      [ "InstanceCount" Lude.=: instanceCount,
        "PurchaseToken" Lude.=: purchaseToken
      ]
