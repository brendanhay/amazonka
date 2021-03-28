{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.AggregationAuthorization
  ( AggregationAuthorization (..)
  -- * Smart constructor
  , mkAggregationAuthorization
  -- * Lenses
  , aaAggregationAuthorizationArn
  , aaAuthorizedAccountId
  , aaAuthorizedAwsRegion
  , aaCreationTime
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AuthorizedAwsRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the authorizations granted to aggregator accounts and regions.
--
-- /See:/ 'mkAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { aggregationAuthorizationArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the aggregation object.
  , authorizedAccountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit account ID of the account authorized to aggregate data.
  , authorizedAwsRegion :: Core.Maybe Types.AuthorizedAwsRegion
    -- ^ The region authorized to collect aggregated data.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time stamp when the aggregation authorization was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AggregationAuthorization' value with any optional fields omitted.
mkAggregationAuthorization
    :: AggregationAuthorization
mkAggregationAuthorization
  = AggregationAuthorization'{aggregationAuthorizationArn =
                                Core.Nothing,
                              authorizedAccountId = Core.Nothing,
                              authorizedAwsRegion = Core.Nothing, creationTime = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the aggregation object.
--
-- /Note:/ Consider using 'aggregationAuthorizationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.Text)
aaAggregationAuthorizationArn = Lens.field @"aggregationAuthorizationArn"
{-# INLINEABLE aaAggregationAuthorizationArn #-}
{-# DEPRECATED aggregationAuthorizationArn "Use generic-lens or generic-optics with 'aggregationAuthorizationArn' instead"  #-}

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAccountId :: Lens.Lens' AggregationAuthorization (Core.Maybe Types.AccountId)
aaAuthorizedAccountId = Lens.field @"authorizedAccountId"
{-# INLINEABLE aaAuthorizedAccountId #-}
{-# DEPRECATED authorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead"  #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Core.Maybe Types.AuthorizedAwsRegion)
aaAuthorizedAwsRegion = Lens.field @"authorizedAwsRegion"
{-# INLINEABLE aaAuthorizedAwsRegion #-}
{-# DEPRECATED authorizedAwsRegion "Use generic-lens or generic-optics with 'authorizedAwsRegion' instead"  #-}

-- | The time stamp when the aggregation authorization was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaCreationTime :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.NominalDiffTime)
aaCreationTime = Lens.field @"creationTime"
{-# INLINEABLE aaCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

instance Core.FromJSON AggregationAuthorization where
        parseJSON
          = Core.withObject "AggregationAuthorization" Core.$
              \ x ->
                AggregationAuthorization' Core.<$>
                  (x Core..:? "AggregationAuthorizationArn") Core.<*>
                    x Core..:? "AuthorizedAccountId"
                    Core.<*> x Core..:? "AuthorizedAwsRegion"
                    Core.<*> x Core..:? "CreationTime"
