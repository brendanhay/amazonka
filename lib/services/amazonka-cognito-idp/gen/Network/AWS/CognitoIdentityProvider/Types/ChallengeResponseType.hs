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
import qualified Network.AWS.Prelude as Prelude

-- | The challenge response type.
--
-- /See:/ 'newChallengeResponseType' smart constructor.
data ChallengeResponseType = ChallengeResponseType'
  { -- | The challenge name
    challengeName :: Prelude.Maybe ChallengeName,
    -- | The challenge response.
    challengeResponse :: Prelude.Maybe ChallengeResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChallengeResponseType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'challengeName', 'challengeResponseType_challengeName' - The challenge name
--
-- 'challengeResponse', 'challengeResponseType_challengeResponse' - The challenge response.
newChallengeResponseType ::
  ChallengeResponseType
newChallengeResponseType =
  ChallengeResponseType'
    { challengeName =
        Prelude.Nothing,
      challengeResponse = Prelude.Nothing
    }

-- | The challenge name
challengeResponseType_challengeName :: Lens.Lens' ChallengeResponseType (Prelude.Maybe ChallengeName)
challengeResponseType_challengeName = Lens.lens (\ChallengeResponseType' {challengeName} -> challengeName) (\s@ChallengeResponseType' {} a -> s {challengeName = a} :: ChallengeResponseType)

-- | The challenge response.
challengeResponseType_challengeResponse :: Lens.Lens' ChallengeResponseType (Prelude.Maybe ChallengeResponse)
challengeResponseType_challengeResponse = Lens.lens (\ChallengeResponseType' {challengeResponse} -> challengeResponse) (\s@ChallengeResponseType' {} a -> s {challengeResponse = a} :: ChallengeResponseType)

instance Core.FromJSON ChallengeResponseType where
  parseJSON =
    Core.withObject
      "ChallengeResponseType"
      ( \x ->
          ChallengeResponseType'
            Prelude.<$> (x Core..:? "ChallengeName")
            Prelude.<*> (x Core..:? "ChallengeResponse")
      )

instance Prelude.Hashable ChallengeResponseType

instance Prelude.NFData ChallengeResponseType
