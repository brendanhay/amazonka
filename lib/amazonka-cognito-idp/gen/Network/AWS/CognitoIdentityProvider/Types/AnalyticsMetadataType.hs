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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An Amazon Pinpoint analytics endpoint.
--
-- An endpoint uniquely identifies a mobile device, email address, or phone number that can receive messages from Amazon Pinpoint analytics.
--
-- /See:/ 'mkAnalyticsMetadataType' smart constructor.
newtype AnalyticsMetadataType = AnalyticsMetadataType'
  { analyticsEndpointId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsMetadataType' with the minimum fields required to make a request.
--
-- * 'analyticsEndpointId' - The endpoint ID.
mkAnalyticsMetadataType ::
  AnalyticsMetadataType
mkAnalyticsMetadataType =
  AnalyticsMetadataType' {analyticsEndpointId = Lude.Nothing}

-- | The endpoint ID.
--
-- /Note:/ Consider using 'analyticsEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amtAnalyticsEndpointId :: Lens.Lens' AnalyticsMetadataType (Lude.Maybe Lude.Text)
amtAnalyticsEndpointId = Lens.lens (analyticsEndpointId :: AnalyticsMetadataType -> Lude.Maybe Lude.Text) (\s a -> s {analyticsEndpointId = a} :: AnalyticsMetadataType)
{-# DEPRECATED amtAnalyticsEndpointId "Use generic-lens or generic-optics with 'analyticsEndpointId' instead." #-}

instance Lude.ToJSON AnalyticsMetadataType where
  toJSON AnalyticsMetadataType' {..} =
    Lude.object
      ( Lude.catMaybes
          [("AnalyticsEndpointId" Lude..=) Lude.<$> analyticsEndpointId]
      )
