-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointConfigSummary
  ( EndpointConfigSummary (..),

    -- * Smart constructor
    mkEndpointConfigSummary,

    -- * Lenses
    ecsEndpointConfigName,
    ecsEndpointConfigARN,
    ecsCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides summary information for an endpoint configuration.
--
-- /See:/ 'mkEndpointConfigSummary' smart constructor.
data EndpointConfigSummary = EndpointConfigSummary'
  { endpointConfigName ::
      Lude.Text,
    endpointConfigARN :: Lude.Text,
    creationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointConfigSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the endpoint configuration was created.
-- * 'endpointConfigARN' - The Amazon Resource Name (ARN) of the endpoint configuration.
-- * 'endpointConfigName' - The name of the endpoint configuration.
mkEndpointConfigSummary ::
  -- | 'endpointConfigName'
  Lude.Text ->
  -- | 'endpointConfigARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  EndpointConfigSummary
mkEndpointConfigSummary
  pEndpointConfigName_
  pEndpointConfigARN_
  pCreationTime_ =
    EndpointConfigSummary'
      { endpointConfigName = pEndpointConfigName_,
        endpointConfigARN = pEndpointConfigARN_,
        creationTime = pCreationTime_
      }

-- | The name of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsEndpointConfigName :: Lens.Lens' EndpointConfigSummary Lude.Text
ecsEndpointConfigName = Lens.lens (endpointConfigName :: EndpointConfigSummary -> Lude.Text) (\s a -> s {endpointConfigName = a} :: EndpointConfigSummary)
{-# DEPRECATED ecsEndpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead." #-}

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- /Note:/ Consider using 'endpointConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsEndpointConfigARN :: Lens.Lens' EndpointConfigSummary Lude.Text
ecsEndpointConfigARN = Lens.lens (endpointConfigARN :: EndpointConfigSummary -> Lude.Text) (\s a -> s {endpointConfigARN = a} :: EndpointConfigSummary)
{-# DEPRECATED ecsEndpointConfigARN "Use generic-lens or generic-optics with 'endpointConfigARN' instead." #-}

-- | A timestamp that shows when the endpoint configuration was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecsCreationTime :: Lens.Lens' EndpointConfigSummary Lude.Timestamp
ecsCreationTime = Lens.lens (creationTime :: EndpointConfigSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: EndpointConfigSummary)
{-# DEPRECATED ecsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON EndpointConfigSummary where
  parseJSON =
    Lude.withObject
      "EndpointConfigSummary"
      ( \x ->
          EndpointConfigSummary'
            Lude.<$> (x Lude..: "EndpointConfigName")
            Lude.<*> (x Lude..: "EndpointConfigArn")
            Lude.<*> (x Lude..: "CreationTime")
      )
