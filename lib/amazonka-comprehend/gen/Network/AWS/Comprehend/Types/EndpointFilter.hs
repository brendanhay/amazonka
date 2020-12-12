{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EndpointFilter
  ( EndpointFilter (..),

    -- * Smart constructor
    mkEndpointFilter,

    -- * Lenses
    efStatus,
    efModelARN,
    efCreationTimeAfter,
    efCreationTimeBefore,
  )
where

import Network.AWS.Comprehend.Types.EndpointStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The filter used to determine which endpoints are returned. You can filter jobs on their name, model, status, or the date and time that they were created. You can only set one filter at a time.
--
-- /See:/ 'mkEndpointFilter' smart constructor.
data EndpointFilter = EndpointFilter'
  { status ::
      Lude.Maybe EndpointStatus,
    modelARN :: Lude.Maybe Lude.Text,
    creationTimeAfter :: Lude.Maybe Lude.Timestamp,
    creationTimeBefore :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointFilter' with the minimum fields required to make a request.
--
-- * 'creationTimeAfter' - Specifies a date after which the returned endpoint or endpoints were created.
-- * 'creationTimeBefore' - Specifies a date before which the returned endpoint or endpoints were created.
-- * 'modelARN' - The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
-- * 'status' - Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
mkEndpointFilter ::
  EndpointFilter
mkEndpointFilter =
  EndpointFilter'
    { status = Lude.Nothing,
      modelARN = Lude.Nothing,
      creationTimeAfter = Lude.Nothing,
      creationTimeBefore = Lude.Nothing
    }

-- | Specifies the status of the endpoint being returned. Possible values are: Creating, Ready, Updating, Deleting, Failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efStatus :: Lens.Lens' EndpointFilter (Lude.Maybe EndpointStatus)
efStatus = Lens.lens (status :: EndpointFilter -> Lude.Maybe EndpointStatus) (\s a -> s {status = a} :: EndpointFilter)
{-# DEPRECATED efStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efModelARN :: Lens.Lens' EndpointFilter (Lude.Maybe Lude.Text)
efModelARN = Lens.lens (modelARN :: EndpointFilter -> Lude.Maybe Lude.Text) (\s a -> s {modelARN = a} :: EndpointFilter)
{-# DEPRECATED efModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | Specifies a date after which the returned endpoint or endpoints were created.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCreationTimeAfter :: Lens.Lens' EndpointFilter (Lude.Maybe Lude.Timestamp)
efCreationTimeAfter = Lens.lens (creationTimeAfter :: EndpointFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeAfter = a} :: EndpointFilter)
{-# DEPRECATED efCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | Specifies a date before which the returned endpoint or endpoints were created.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efCreationTimeBefore :: Lens.Lens' EndpointFilter (Lude.Maybe Lude.Timestamp)
efCreationTimeBefore = Lens.lens (creationTimeBefore :: EndpointFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimeBefore = a} :: EndpointFilter)
{-# DEPRECATED efCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

instance Lude.ToJSON EndpointFilter where
  toJSON EndpointFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("ModelArn" Lude..=) Lude.<$> modelARN,
            ("CreationTimeAfter" Lude..=) Lude.<$> creationTimeAfter,
            ("CreationTimeBefore" Lude..=) Lude.<$> creationTimeBefore
          ]
      )
