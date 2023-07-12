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
-- Module      : Amazonka.CognitoIdentityProvider.Types.ChallengeResponseType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ChallengeResponseType where

import Amazonka.CognitoIdentityProvider.Types.ChallengeName
import Amazonka.CognitoIdentityProvider.Types.ChallengeResponse
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The challenge response type.
--
-- /See:/ 'newChallengeResponseType' smart constructor.
data ChallengeResponseType = ChallengeResponseType'
  { -- | The challenge name.
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
-- 'challengeName', 'challengeResponseType_challengeName' - The challenge name.
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

-- | The challenge name.
challengeResponseType_challengeName :: Lens.Lens' ChallengeResponseType (Prelude.Maybe ChallengeName)
challengeResponseType_challengeName = Lens.lens (\ChallengeResponseType' {challengeName} -> challengeName) (\s@ChallengeResponseType' {} a -> s {challengeName = a} :: ChallengeResponseType)

-- | The challenge response.
challengeResponseType_challengeResponse :: Lens.Lens' ChallengeResponseType (Prelude.Maybe ChallengeResponse)
challengeResponseType_challengeResponse = Lens.lens (\ChallengeResponseType' {challengeResponse} -> challengeResponse) (\s@ChallengeResponseType' {} a -> s {challengeResponse = a} :: ChallengeResponseType)

instance Data.FromJSON ChallengeResponseType where
  parseJSON =
    Data.withObject
      "ChallengeResponseType"
      ( \x ->
          ChallengeResponseType'
            Prelude.<$> (x Data..:? "ChallengeName")
            Prelude.<*> (x Data..:? "ChallengeResponse")
      )

instance Prelude.Hashable ChallengeResponseType where
  hashWithSalt _salt ChallengeResponseType' {..} =
    _salt
      `Prelude.hashWithSalt` challengeName
      `Prelude.hashWithSalt` challengeResponse

instance Prelude.NFData ChallengeResponseType where
  rnf ChallengeResponseType' {..} =
    Prelude.rnf challengeName
      `Prelude.seq` Prelude.rnf challengeResponse
