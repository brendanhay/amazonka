{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StartSigningJobParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StartSigningJobParameter
  ( StartSigningJobParameter (..),

    -- * Smart constructor
    mkStartSigningJobParameter,

    -- * Lenses
    ssjpDestination,
    ssjpSigningProfileName,
    ssjpSigningProfileParameter,
  )
where

import Network.AWS.IoT.Types.Destination
import Network.AWS.IoT.Types.SigningProfileParameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information required to start a signing job.
--
-- /See:/ 'mkStartSigningJobParameter' smart constructor.
data StartSigningJobParameter = StartSigningJobParameter'
  { destination ::
      Lude.Maybe Destination,
    signingProfileName ::
      Lude.Maybe Lude.Text,
    signingProfileParameter ::
      Lude.Maybe SigningProfileParameter
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSigningJobParameter' with the minimum fields required to make a request.
--
-- * 'destination' - The location to write the code-signed file.
-- * 'signingProfileName' - The code-signing profile name.
-- * 'signingProfileParameter' - Describes the code-signing profile.
mkStartSigningJobParameter ::
  StartSigningJobParameter
mkStartSigningJobParameter =
  StartSigningJobParameter'
    { destination = Lude.Nothing,
      signingProfileName = Lude.Nothing,
      signingProfileParameter = Lude.Nothing
    }

-- | The location to write the code-signed file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpDestination :: Lens.Lens' StartSigningJobParameter (Lude.Maybe Destination)
ssjpDestination = Lens.lens (destination :: StartSigningJobParameter -> Lude.Maybe Destination) (\s a -> s {destination = a} :: StartSigningJobParameter)
{-# DEPRECATED ssjpDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The code-signing profile name.
--
-- /Note:/ Consider using 'signingProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpSigningProfileName :: Lens.Lens' StartSigningJobParameter (Lude.Maybe Lude.Text)
ssjpSigningProfileName = Lens.lens (signingProfileName :: StartSigningJobParameter -> Lude.Maybe Lude.Text) (\s a -> s {signingProfileName = a} :: StartSigningJobParameter)
{-# DEPRECATED ssjpSigningProfileName "Use generic-lens or generic-optics with 'signingProfileName' instead." #-}

-- | Describes the code-signing profile.
--
-- /Note:/ Consider using 'signingProfileParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpSigningProfileParameter :: Lens.Lens' StartSigningJobParameter (Lude.Maybe SigningProfileParameter)
ssjpSigningProfileParameter = Lens.lens (signingProfileParameter :: StartSigningJobParameter -> Lude.Maybe SigningProfileParameter) (\s a -> s {signingProfileParameter = a} :: StartSigningJobParameter)
{-# DEPRECATED ssjpSigningProfileParameter "Use generic-lens or generic-optics with 'signingProfileParameter' instead." #-}

instance Lude.FromJSON StartSigningJobParameter where
  parseJSON =
    Lude.withObject
      "StartSigningJobParameter"
      ( \x ->
          StartSigningJobParameter'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "signingProfileName")
            Lude.<*> (x Lude..:? "signingProfileParameter")
      )

instance Lude.ToJSON StartSigningJobParameter where
  toJSON StartSigningJobParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destination" Lude..=) Lude.<$> destination,
            ("signingProfileName" Lude..=) Lude.<$> signingProfileName,
            ("signingProfileParameter" Lude..=)
              Lude.<$> signingProfileParameter
          ]
      )
