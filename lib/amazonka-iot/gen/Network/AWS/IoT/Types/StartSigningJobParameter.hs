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

import qualified Network.AWS.IoT.Types.Destination as Types
import qualified Network.AWS.IoT.Types.SigningProfileName as Types
import qualified Network.AWS.IoT.Types.SigningProfileParameter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information required to start a signing job.
--
-- /See:/ 'mkStartSigningJobParameter' smart constructor.
data StartSigningJobParameter = StartSigningJobParameter'
  { -- | The location to write the code-signed file.
    destination :: Core.Maybe Types.Destination,
    -- | The code-signing profile name.
    signingProfileName :: Core.Maybe Types.SigningProfileName,
    -- | Describes the code-signing profile.
    signingProfileParameter :: Core.Maybe Types.SigningProfileParameter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSigningJobParameter' value with any optional fields omitted.
mkStartSigningJobParameter ::
  StartSigningJobParameter
mkStartSigningJobParameter =
  StartSigningJobParameter'
    { destination = Core.Nothing,
      signingProfileName = Core.Nothing,
      signingProfileParameter = Core.Nothing
    }

-- | The location to write the code-signed file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpDestination :: Lens.Lens' StartSigningJobParameter (Core.Maybe Types.Destination)
ssjpDestination = Lens.field @"destination"
{-# DEPRECATED ssjpDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The code-signing profile name.
--
-- /Note:/ Consider using 'signingProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpSigningProfileName :: Lens.Lens' StartSigningJobParameter (Core.Maybe Types.SigningProfileName)
ssjpSigningProfileName = Lens.field @"signingProfileName"
{-# DEPRECATED ssjpSigningProfileName "Use generic-lens or generic-optics with 'signingProfileName' instead." #-}

-- | Describes the code-signing profile.
--
-- /Note:/ Consider using 'signingProfileParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssjpSigningProfileParameter :: Lens.Lens' StartSigningJobParameter (Core.Maybe Types.SigningProfileParameter)
ssjpSigningProfileParameter = Lens.field @"signingProfileParameter"
{-# DEPRECATED ssjpSigningProfileParameter "Use generic-lens or generic-optics with 'signingProfileParameter' instead." #-}

instance Core.FromJSON StartSigningJobParameter where
  toJSON StartSigningJobParameter {..} =
    Core.object
      ( Core.catMaybes
          [ ("destination" Core..=) Core.<$> destination,
            ("signingProfileName" Core..=) Core.<$> signingProfileName,
            ("signingProfileParameter" Core..=)
              Core.<$> signingProfileParameter
          ]
      )

instance Core.FromJSON StartSigningJobParameter where
  parseJSON =
    Core.withObject "StartSigningJobParameter" Core.$
      \x ->
        StartSigningJobParameter'
          Core.<$> (x Core..:? "destination")
          Core.<*> (x Core..:? "signingProfileName")
          Core.<*> (x Core..:? "signingProfileParameter")
