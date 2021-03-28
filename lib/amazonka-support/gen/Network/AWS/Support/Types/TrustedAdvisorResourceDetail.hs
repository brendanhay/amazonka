{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorResourceDetail
  ( TrustedAdvisorResourceDetail (..)
  -- * Smart constructor
  , mkTrustedAdvisorResourceDetail
  -- * Lenses
  , tardStatus
  , tardResourceId
  , tardMetadata
  , tardIsSuppressed
  , tardRegion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a resource identified by a Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorResourceDetail' smart constructor.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
  { status :: Core.Text
    -- ^ The status code for the resource identified in the Trusted Advisor check.
  , resourceId :: Core.Text
    -- ^ The unique identifier for the identified resource.
  , metadata :: [Core.Text]
    -- ^ Additional information about the identified resource. The exact metadata and its order can be obtained by inspecting the 'TrustedAdvisorCheckDescription' object returned by the call to 'DescribeTrustedAdvisorChecks' . __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
  , isSuppressed :: Core.Maybe Core.Bool
    -- ^ Specifies whether the AWS resource was ignored by Trusted Advisor because it was marked as suppressed by the user.
  , region :: Core.Maybe Core.Text
    -- ^ The AWS region in which the identified resource is located.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorResourceDetail' value with any optional fields omitted.
mkTrustedAdvisorResourceDetail
    :: Core.Text -- ^ 'status'
    -> Core.Text -- ^ 'resourceId'
    -> TrustedAdvisorResourceDetail
mkTrustedAdvisorResourceDetail status resourceId
  = TrustedAdvisorResourceDetail'{status, resourceId,
                                  metadata = Core.mempty, isSuppressed = Core.Nothing,
                                  region = Core.Nothing}

-- | The status code for the resource identified in the Trusted Advisor check.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tardStatus :: Lens.Lens' TrustedAdvisorResourceDetail Core.Text
tardStatus = Lens.field @"status"
{-# INLINEABLE tardStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The unique identifier for the identified resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tardResourceId :: Lens.Lens' TrustedAdvisorResourceDetail Core.Text
tardResourceId = Lens.field @"resourceId"
{-# INLINEABLE tardResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Additional information about the identified resource. The exact metadata and its order can be obtained by inspecting the 'TrustedAdvisorCheckDescription' object returned by the call to 'DescribeTrustedAdvisorChecks' . __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tardMetadata :: Lens.Lens' TrustedAdvisorResourceDetail [Core.Text]
tardMetadata = Lens.field @"metadata"
{-# INLINEABLE tardMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | Specifies whether the AWS resource was ignored by Trusted Advisor because it was marked as suppressed by the user.
--
-- /Note:/ Consider using 'isSuppressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tardIsSuppressed :: Lens.Lens' TrustedAdvisorResourceDetail (Core.Maybe Core.Bool)
tardIsSuppressed = Lens.field @"isSuppressed"
{-# INLINEABLE tardIsSuppressed #-}
{-# DEPRECATED isSuppressed "Use generic-lens or generic-optics with 'isSuppressed' instead"  #-}

-- | The AWS region in which the identified resource is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tardRegion :: Lens.Lens' TrustedAdvisorResourceDetail (Core.Maybe Core.Text)
tardRegion = Lens.field @"region"
{-# INLINEABLE tardRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON TrustedAdvisorResourceDetail where
        parseJSON
          = Core.withObject "TrustedAdvisorResourceDetail" Core.$
              \ x ->
                TrustedAdvisorResourceDetail' Core.<$>
                  (x Core..: "status") Core.<*> x Core..: "resourceId" Core.<*>
                    x Core..:? "metadata" Core..!= Core.mempty
                    Core.<*> x Core..:? "isSuppressed"
                    Core.<*> x Core..:? "region"
