{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregationAuthorization
  ( AggregationAuthorization (..),

    -- * Smart constructor
    mkAggregationAuthorization,

    -- * Lenses
    aaAggregationAuthorizationArn,
    aaAuthorizedAccountId,
    aaAuthorizedAwsRegion,
    aaCreationTime,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AuthorizedAwsRegion as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the authorizations granted to aggregator accounts and regions.
--
-- /See:/ 'mkAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { -- | The Amazon Resource Name (ARN) of the aggregation object.
    aggregationAuthorizationArn :: Core.Maybe Types.String,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Core.Maybe Types.AccountId,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Core.Maybe Types.AuthorizedAwsRegion,
    -- | The time stamp when the aggregation authorization was created.
    creationTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AggregationAuthorization' value with any optional fields omitted.
mkAggregationAuthorization ::
  AggregationAuthorization
mkAggregationAuthorization =
  AggregationAuthorization'
    { aggregationAuthorizationArn =
        Core.Nothing,
      authorizedAccountId = Core.Nothing,
      authorizedAwsRegion = Core.Nothing,
      creationTime = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the aggregation object.
--
-- /Note:/ Consider using 'aggregationAuthorizationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Core.Maybe Types.String)
aaAggregationAuthorizationArn = Lens.field @"aggregationAuthorizationArn"
{-# DEPRECATED aaAggregationAuthorizationArn "Use generic-lens or generic-optics with 'aggregationAuthorizationArn' instead." #-}

-- | The 12-digit account ID of the account authorized to aggregate data.
--
-- /Note:/ Consider using 'authorizedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAccountId :: Lens.Lens' AggregationAuthorization (Core.Maybe Types.AccountId)
aaAuthorizedAccountId = Lens.field @"authorizedAccountId"
{-# DEPRECATED aaAuthorizedAccountId "Use generic-lens or generic-optics with 'authorizedAccountId' instead." #-}

-- | The region authorized to collect aggregated data.
--
-- /Note:/ Consider using 'authorizedAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAuthorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Core.Maybe Types.AuthorizedAwsRegion)
aaAuthorizedAwsRegion = Lens.field @"authorizedAwsRegion"
{-# DEPRECATED aaAuthorizedAwsRegion "Use generic-lens or generic-optics with 'authorizedAwsRegion' instead." #-}

-- | The time stamp when the aggregation authorization was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaCreationTime :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.NominalDiffTime)
aaCreationTime = Lens.field @"creationTime"
{-# DEPRECATED aaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Core.FromJSON AggregationAuthorization where
  parseJSON =
    Core.withObject "AggregationAuthorization" Core.$
      \x ->
        AggregationAuthorization'
          Core.<$> (x Core..:? "AggregationAuthorizationArn")
          Core.<*> (x Core..:? "AuthorizedAccountId")
          Core.<*> (x Core..:? "AuthorizedAwsRegion")
          Core.<*> (x Core..:? "CreationTime")
