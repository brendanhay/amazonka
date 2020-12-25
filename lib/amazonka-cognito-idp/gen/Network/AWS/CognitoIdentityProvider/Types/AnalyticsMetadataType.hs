{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
  ( AnalyticsMetadataType (..),

    -- * Smart constructor
    mkAnalyticsMetadataType,

    -- * Lenses
    amtAnalyticsEndpointId,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An Amazon Pinpoint analytics endpoint.
--
-- An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.
--
-- /See:/ 'mkAnalyticsMetadataType' smart constructor.
newtype AnalyticsMetadataType = AnalyticsMetadataType'
  { -- | The endpoint ID.
    analyticsEndpointId :: Core.Maybe Types.StringType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsMetadataType' value with any optional fields omitted.
mkAnalyticsMetadataType ::
  AnalyticsMetadataType
mkAnalyticsMetadataType =
  AnalyticsMetadataType' {analyticsEndpointId = Core.Nothing}

-- | The endpoint ID.
--
-- /Note:/ Consider using 'analyticsEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtAnalyticsEndpointId :: Lens.Lens' AnalyticsMetadataType (Core.Maybe Types.StringType)
amtAnalyticsEndpointId = Lens.field @"analyticsEndpointId"
{-# DEPRECATED amtAnalyticsEndpointId "Use generic-lens or generic-optics with 'analyticsEndpointId' instead." #-}

instance Core.FromJSON AnalyticsMetadataType where
  toJSON AnalyticsMetadataType {..} =
    Core.object
      ( Core.catMaybes
          [("AnalyticsEndpointId" Core..=) Core.<$> analyticsEndpointId]
      )
