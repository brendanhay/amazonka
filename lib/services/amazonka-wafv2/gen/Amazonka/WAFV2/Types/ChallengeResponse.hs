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
-- Module      : Amazonka.WAFV2.Types.ChallengeResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ChallengeResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FailureReason

-- | The result from the inspection of the web request for a valid challenge
-- token.
--
-- /See:/ 'newChallengeResponse' smart constructor.
data ChallengeResponse = ChallengeResponse'
  { -- | The reason for failure, populated when the evaluation of the token
    -- fails.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The HTTP response code indicating the status of the challenge token in
    -- the web request. If the token is missing, invalid, or expired, this code
    -- is @202 Request Accepted@.
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | The time that the challenge was last solved for the supplied token.
    solveTimestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChallengeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'challengeResponse_failureReason' - The reason for failure, populated when the evaluation of the token
-- fails.
--
-- 'responseCode', 'challengeResponse_responseCode' - The HTTP response code indicating the status of the challenge token in
-- the web request. If the token is missing, invalid, or expired, this code
-- is @202 Request Accepted@.
--
-- 'solveTimestamp', 'challengeResponse_solveTimestamp' - The time that the challenge was last solved for the supplied token.
newChallengeResponse ::
  ChallengeResponse
newChallengeResponse =
  ChallengeResponse'
    { failureReason = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      solveTimestamp = Prelude.Nothing
    }

-- | The reason for failure, populated when the evaluation of the token
-- fails.
challengeResponse_failureReason :: Lens.Lens' ChallengeResponse (Prelude.Maybe FailureReason)
challengeResponse_failureReason = Lens.lens (\ChallengeResponse' {failureReason} -> failureReason) (\s@ChallengeResponse' {} a -> s {failureReason = a} :: ChallengeResponse)

-- | The HTTP response code indicating the status of the challenge token in
-- the web request. If the token is missing, invalid, or expired, this code
-- is @202 Request Accepted@.
challengeResponse_responseCode :: Lens.Lens' ChallengeResponse (Prelude.Maybe Prelude.Int)
challengeResponse_responseCode = Lens.lens (\ChallengeResponse' {responseCode} -> responseCode) (\s@ChallengeResponse' {} a -> s {responseCode = a} :: ChallengeResponse)

-- | The time that the challenge was last solved for the supplied token.
challengeResponse_solveTimestamp :: Lens.Lens' ChallengeResponse (Prelude.Maybe Prelude.Integer)
challengeResponse_solveTimestamp = Lens.lens (\ChallengeResponse' {solveTimestamp} -> solveTimestamp) (\s@ChallengeResponse' {} a -> s {solveTimestamp = a} :: ChallengeResponse)

instance Data.FromJSON ChallengeResponse where
  parseJSON =
    Data.withObject
      "ChallengeResponse"
      ( \x ->
          ChallengeResponse'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "ResponseCode")
            Prelude.<*> (x Data..:? "SolveTimestamp")
      )

instance Prelude.Hashable ChallengeResponse where
  hashWithSalt _salt ChallengeResponse' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` responseCode
      `Prelude.hashWithSalt` solveTimestamp

instance Prelude.NFData ChallengeResponse where
  rnf ChallengeResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf responseCode
      `Prelude.seq` Prelude.rnf solveTimestamp
