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

import Network.AWS.CognitoIdentityProvider.Types.ChallengeName
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The challenge response type.
--
-- /See:/ 'mkChallengeResponseType' smart constructor.
data ChallengeResponseType = ChallengeResponseType'
  { -- | The challenge name
    challengeName :: Lude.Maybe ChallengeName,
    -- | The challenge response.
    challengeResponse :: Lude.Maybe ChallengeResponse
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChallengeResponseType' with the minimum fields required to make a request.
--
-- * 'challengeName' - The challenge name
-- * 'challengeResponse' - The challenge response.
mkChallengeResponseType ::
  ChallengeResponseType
mkChallengeResponseType =
  ChallengeResponseType'
    { challengeName = Lude.Nothing,
      challengeResponse = Lude.Nothing
    }

-- | The challenge name
--
-- /Note:/ Consider using 'challengeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtChallengeName :: Lens.Lens' ChallengeResponseType (Lude.Maybe ChallengeName)
crtChallengeName = Lens.lens (challengeName :: ChallengeResponseType -> Lude.Maybe ChallengeName) (\s a -> s {challengeName = a} :: ChallengeResponseType)
{-# DEPRECATED crtChallengeName "Use generic-lens or generic-optics with 'challengeName' instead." #-}

-- | The challenge response.
--
-- /Note:/ Consider using 'challengeResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtChallengeResponse :: Lens.Lens' ChallengeResponseType (Lude.Maybe ChallengeResponse)
crtChallengeResponse = Lens.lens (challengeResponse :: ChallengeResponseType -> Lude.Maybe ChallengeResponse) (\s a -> s {challengeResponse = a} :: ChallengeResponseType)
{-# DEPRECATED crtChallengeResponse "Use generic-lens or generic-optics with 'challengeResponse' instead." #-}

instance Lude.FromJSON ChallengeResponseType where
  parseJSON =
    Lude.withObject
      "ChallengeResponseType"
      ( \x ->
          ChallengeResponseType'
            Lude.<$> (x Lude..:? "ChallengeName")
            Lude.<*> (x Lude..:? "ChallengeResponse")
      )
