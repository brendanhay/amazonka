{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointProperties
  ( EndpointProperties (..),

    -- * Smart constructor
    mkEndpointProperties,

    -- * Lenses
    epCreationTime,
    epStatus,
    epModelARN,
    epLastModifiedTime,
    epDesiredInferenceUnits,
    epCurrentInferenceUnits,
    epMessage,
    epEndpointARN,
  )
where

import Network.AWS.Comprehend.Types.EndpointStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies information about the specified endpoint.
--
-- /See:/ 'mkEndpointProperties' smart constructor.
data EndpointProperties = EndpointProperties'
  { -- | The creation date and time of the endpoint.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | Specifies the status of the endpoint. Because the endpoint updates and creation are asynchronous, so customers will need to wait for the endpoint to be @Ready@ status before making inference requests.
    status :: Lude.Maybe EndpointStatus,
    -- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
    modelARN :: Lude.Maybe Lude.Text,
    -- | The date and time that the endpoint was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
    desiredInferenceUnits :: Lude.Maybe Lude.Natural,
    -- | The number of inference units currently used by the model using this endpoint.
    currentInferenceUnits :: Lude.Maybe Lude.Natural,
    -- | Specifies a reason for failure in cases of @Failed@ status.
    message :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointProperties' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation date and time of the endpoint.
-- * 'status' - Specifies the status of the endpoint. Because the endpoint updates and creation are asynchronous, so customers will need to wait for the endpoint to be @Ready@ status before making inference requests.
-- * 'modelARN' - The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
-- * 'lastModifiedTime' - The date and time that the endpoint was last modified.
-- * 'desiredInferenceUnits' - The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
-- * 'currentInferenceUnits' - The number of inference units currently used by the model using this endpoint.
-- * 'message' - Specifies a reason for failure in cases of @Failed@ status.
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint.
mkEndpointProperties ::
  EndpointProperties
mkEndpointProperties =
  EndpointProperties'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      modelARN = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      desiredInferenceUnits = Lude.Nothing,
      currentInferenceUnits = Lude.Nothing,
      message = Lude.Nothing,
      endpointARN = Lude.Nothing
    }

-- | The creation date and time of the endpoint.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epCreationTime :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Timestamp)
epCreationTime = Lens.lens (creationTime :: EndpointProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: EndpointProperties)
{-# DEPRECATED epCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Specifies the status of the endpoint. Because the endpoint updates and creation are asynchronous, so customers will need to wait for the endpoint to be @Ready@ status before making inference requests.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epStatus :: Lens.Lens' EndpointProperties (Lude.Maybe EndpointStatus)
epStatus = Lens.lens (status :: EndpointProperties -> Lude.Maybe EndpointStatus) (\s a -> s {status = a} :: EndpointProperties)
{-# DEPRECATED epStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epModelARN :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Text)
epModelARN = Lens.lens (modelARN :: EndpointProperties -> Lude.Maybe Lude.Text) (\s a -> s {modelARN = a} :: EndpointProperties)
{-# DEPRECATED epModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | The date and time that the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLastModifiedTime :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Timestamp)
epLastModifiedTime = Lens.lens (lastModifiedTime :: EndpointProperties -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: EndpointProperties)
{-# DEPRECATED epLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDesiredInferenceUnits :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Natural)
epDesiredInferenceUnits = Lens.lens (desiredInferenceUnits :: EndpointProperties -> Lude.Maybe Lude.Natural) (\s a -> s {desiredInferenceUnits = a} :: EndpointProperties)
{-# DEPRECATED epDesiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead." #-}

-- | The number of inference units currently used by the model using this endpoint.
--
-- /Note:/ Consider using 'currentInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epCurrentInferenceUnits :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Natural)
epCurrentInferenceUnits = Lens.lens (currentInferenceUnits :: EndpointProperties -> Lude.Maybe Lude.Natural) (\s a -> s {currentInferenceUnits = a} :: EndpointProperties)
{-# DEPRECATED epCurrentInferenceUnits "Use generic-lens or generic-optics with 'currentInferenceUnits' instead." #-}

-- | Specifies a reason for failure in cases of @Failed@ status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMessage :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Text)
epMessage = Lens.lens (message :: EndpointProperties -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EndpointProperties)
{-# DEPRECATED epMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The Amazon Resource Number (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epEndpointARN :: Lens.Lens' EndpointProperties (Lude.Maybe Lude.Text)
epEndpointARN = Lens.lens (endpointARN :: EndpointProperties -> Lude.Maybe Lude.Text) (\s a -> s {endpointARN = a} :: EndpointProperties)
{-# DEPRECATED epEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.FromJSON EndpointProperties where
  parseJSON =
    Lude.withObject
      "EndpointProperties"
      ( \x ->
          EndpointProperties'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ModelArn")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "DesiredInferenceUnits")
            Lude.<*> (x Lude..:? "CurrentInferenceUnits")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "EndpointArn")
      )
