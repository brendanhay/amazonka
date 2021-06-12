{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType where

import Network.AWS.CognitoIdentityProvider.Types.ChallengeName
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The challenge response type.
--
-- /See:/ 'newChallengeResponseType' smart constructor.
data ChallengeResponseType = ChallengeResponseType'
  { -- | The challenge response.
    challengeResponse :: Core.Maybe ChallengeResponse,
    -- | The challenge name
    challengeName :: Core.Maybe ChallengeName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChallengeResponseType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'challengeResponse', 'challengeResponseType_challengeResponse' - The challenge response.
--
-- 'challengeName', 'challengeResponseType_challengeName' - The challenge name
newChallengeResponseType ::
  ChallengeResponseType
newChallengeResponseType =
  ChallengeResponseType'
    { challengeResponse =
        Core.Nothing,
      challengeName = Core.Nothing
    }

-- | The challenge response.
challengeResponseType_challengeResponse :: Lens.Lens' ChallengeResponseType (Core.Maybe ChallengeResponse)
challengeResponseType_challengeResponse = Lens.lens (\ChallengeResponseType' {challengeResponse} -> challengeResponse) (\s@ChallengeResponseType' {} a -> s {challengeResponse = a} :: ChallengeResponseType)

-- | The challenge name
challengeResponseType_challengeName :: Lens.Lens' ChallengeResponseType (Core.Maybe ChallengeName)
challengeResponseType_challengeName = Lens.lens (\ChallengeResponseType' {challengeName} -> challengeName) (\s@ChallengeResponseType' {} a -> s {challengeName = a} :: ChallengeResponseType)

instance Core.FromJSON ChallengeResponseType where
  parseJSON =
    Core.withObject
      "ChallengeResponseType"
      ( \x ->
          ChallengeResponseType'
            Core.<$> (x Core..:? "ChallengeResponse")
            Core.<*> (x Core..:? "ChallengeName")
      )

instance Core.Hashable ChallengeResponseType

instance Core.NFData ChallengeResponseType
