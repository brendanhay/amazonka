-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppSpecification
  ( AppSpecification (..),

    -- * Smart constructor
    mkAppSpecification,

    -- * Lenses
    asContainerArguments,
    asContainerEntrypoint,
    asImageURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration to run a processing job in a specified container image.
--
-- /See:/ 'mkAppSpecification' smart constructor.
data AppSpecification = AppSpecification'
  { containerArguments ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    containerEntrypoint ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    imageURI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppSpecification' with the minimum fields required to make a request.
--
-- * 'containerArguments' - The arguments for a container used to run a processing job.
-- * 'containerEntrypoint' - The entrypoint for a container used to run a processing job.
-- * 'imageURI' - The container image to be run by the processing job.
mkAppSpecification ::
  -- | 'imageURI'
  Lude.Text ->
  AppSpecification
mkAppSpecification pImageURI_ =
  AppSpecification'
    { containerArguments = Lude.Nothing,
      containerEntrypoint = Lude.Nothing,
      imageURI = pImageURI_
    }

-- | The arguments for a container used to run a processing job.
--
-- /Note:/ Consider using 'containerArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asContainerArguments :: Lens.Lens' AppSpecification (Lude.Maybe (Lude.NonEmpty Lude.Text))
asContainerArguments = Lens.lens (containerArguments :: AppSpecification -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {containerArguments = a} :: AppSpecification)
{-# DEPRECATED asContainerArguments "Use generic-lens or generic-optics with 'containerArguments' instead." #-}

-- | The entrypoint for a container used to run a processing job.
--
-- /Note:/ Consider using 'containerEntrypoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asContainerEntrypoint :: Lens.Lens' AppSpecification (Lude.Maybe (Lude.NonEmpty Lude.Text))
asContainerEntrypoint = Lens.lens (containerEntrypoint :: AppSpecification -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {containerEntrypoint = a} :: AppSpecification)
{-# DEPRECATED asContainerEntrypoint "Use generic-lens or generic-optics with 'containerEntrypoint' instead." #-}

-- | The container image to be run by the processing job.
--
-- /Note:/ Consider using 'imageURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asImageURI :: Lens.Lens' AppSpecification Lude.Text
asImageURI = Lens.lens (imageURI :: AppSpecification -> Lude.Text) (\s a -> s {imageURI = a} :: AppSpecification)
{-# DEPRECATED asImageURI "Use generic-lens or generic-optics with 'imageURI' instead." #-}

instance Lude.FromJSON AppSpecification where
  parseJSON =
    Lude.withObject
      "AppSpecification"
      ( \x ->
          AppSpecification'
            Lude.<$> (x Lude..:? "ContainerArguments")
            Lude.<*> (x Lude..:? "ContainerEntrypoint")
            Lude.<*> (x Lude..: "ImageUri")
      )

instance Lude.ToJSON AppSpecification where
  toJSON AppSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContainerArguments" Lude..=) Lude.<$> containerArguments,
            ("ContainerEntrypoint" Lude..=) Lude.<$> containerEntrypoint,
            Lude.Just ("ImageUri" Lude..= imageURI)
          ]
      )
