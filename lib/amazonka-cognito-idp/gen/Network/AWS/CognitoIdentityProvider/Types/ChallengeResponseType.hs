{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
  ( ChallengeResponseType (..),

    -- * Smart constructor
    mkChallengeResponseType,

    -- * Lenses
    crtChallengeName,
    crtChallengeResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.ChallengeName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The challenge response type.
--
-- /See:/ 'mkChallengeResponseType' smart constructor.
data ChallengeResponseType = ChallengeResponseType'
  { -- | The challenge name
    challengeName :: Core.Maybe Types.ChallengeName,
    -- | The challenge response.
    challengeResponse :: Core.Maybe Types.ChallengeResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChallengeResponseType' value with any optional fields omitted.
mkChallengeResponseType ::
  ChallengeResponseType
mkChallengeResponseType =
  ChallengeResponseType'
    { challengeName = Core.Nothing,
      challengeResponse = Core.Nothing
    }

-- | The challenge name
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtChallengeName :: Lens.Lens' ChallengeResponseType (Core.Maybe Types.ChallengeName)
crtChallengeName = Lens.field @"challengeName"
{-# DEPRECATED crtChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge response.
--
-- /Note:/ Consider using 'challengeResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtChallengeResponse :: Lens.Lens' ChallengeResponseType (Core.Maybe Types.ChallengeResponse)
crtChallengeResponse = Lens.field @"challengeResponse"
{-# DEPRECATED crtChallengeResponse "Use generic-lens or generic-optics with 'challengeResponse' instead." #-}

instance Core.FromJSON ChallengeResponseType where
  parseJSON =
    Core.withObject "ChallengeResponseType" Core.$
      \x ->
        ChallengeResponseType'
          Core.<$> (x Core..:? "ChallengeName")
          Core.<*> (x Core..:? "ChallengeResponse")
