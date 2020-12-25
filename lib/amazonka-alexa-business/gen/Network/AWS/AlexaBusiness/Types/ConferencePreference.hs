{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferencePreference
  ( ConferencePreference (..),

    -- * Smart constructor
    mkConferencePreference,

    -- * Lenses
    cpDefaultConferenceProviderArn,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The default conference provider that is used if no other scheduled meetings are detected.
--
-- /See:/ 'mkConferencePreference' smart constructor.
newtype ConferencePreference = ConferencePreference'
  { -- | The ARN of the default conference provider.
    defaultConferenceProviderArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ConferencePreference' value with any optional fields omitted.
mkConferencePreference ::
  ConferencePreference
mkConferencePreference =
  ConferencePreference'
    { defaultConferenceProviderArn =
        Core.Nothing
    }

-- | The ARN of the default conference provider.
--
-- /Note:/ Consider using 'defaultConferenceProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDefaultConferenceProviderArn :: Lens.Lens' ConferencePreference (Core.Maybe Types.Arn)
cpDefaultConferenceProviderArn = Lens.field @"defaultConferenceProviderArn"
{-# DEPRECATED cpDefaultConferenceProviderArn "Use generic-lens or generic-optics with 'defaultConferenceProviderArn' instead." #-}

instance Core.FromJSON ConferencePreference where
  toJSON ConferencePreference {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultConferenceProviderArn" Core..=)
              Core.<$> defaultConferenceProviderArn
          ]
      )

instance Core.FromJSON ConferencePreference where
  parseJSON =
    Core.withObject "ConferencePreference" Core.$
      \x ->
        ConferencePreference'
          Core.<$> (x Core..:? "DefaultConferenceProviderArn")
