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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a request to purchase Scheduled Instances.
--
-- /See:/ 'mkPurchaseRequest' smart constructor.
data PurchaseRequest = PurchaseRequest'
  { -- | The number of instances.
    instanceCount :: Core.Int,
    -- | The purchase token.
    purchaseToken :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseRequest' value with any optional fields omitted.
mkPurchaseRequest ::
  -- | 'instanceCount'
  Core.Int ->
  -- | 'purchaseToken'
  Types.String ->
  PurchaseRequest
mkPurchaseRequest instanceCount purchaseToken =
  PurchaseRequest' {instanceCount, purchaseToken}

-- | The number of instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prInstanceCount :: Lens.Lens' PurchaseRequest Core.Int
prInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED prInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The purchase token.
--
-- /Note:/ Consider using 'purchaseToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPurchaseToken :: Lens.Lens' PurchaseRequest Types.String
prPurchaseToken = Lens.field @"purchaseToken"
{-# DEPRECATED prPurchaseToken "Use generic-lens or generic-optics with 'purchaseToken' instead." #-}
