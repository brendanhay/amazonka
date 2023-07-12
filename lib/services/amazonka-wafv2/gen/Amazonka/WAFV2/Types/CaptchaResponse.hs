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
-- Module      : Amazonka.WAFV2.Types.CaptchaResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CaptchaResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.FailureReason

-- | The result from the inspection of the web request for a valid @CAPTCHA@
-- token.
--
-- /See:/ 'newCaptchaResponse' smart constructor.
data CaptchaResponse = CaptchaResponse'
  { -- | The reason for failure, populated when the evaluation of the token
    -- fails.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The HTTP response code indicating the status of the @CAPTCHA@ token in
    -- the web request. If the token is missing, invalid, or expired, this code
    -- is @405 Method Not Allowed@.
    responseCode :: Prelude.Maybe Prelude.Int,
    -- | The time that the @CAPTCHA@ was last solved for the supplied token.
    solveTimestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptchaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'captchaResponse_failureReason' - The reason for failure, populated when the evaluation of the token
-- fails.
--
-- 'responseCode', 'captchaResponse_responseCode' - The HTTP response code indicating the status of the @CAPTCHA@ token in
-- the web request. If the token is missing, invalid, or expired, this code
-- is @405 Method Not Allowed@.
--
-- 'solveTimestamp', 'captchaResponse_solveTimestamp' - The time that the @CAPTCHA@ was last solved for the supplied token.
newCaptchaResponse ::
  CaptchaResponse
newCaptchaResponse =
  CaptchaResponse'
    { failureReason = Prelude.Nothing,
      responseCode = Prelude.Nothing,
      solveTimestamp = Prelude.Nothing
    }

-- | The reason for failure, populated when the evaluation of the token
-- fails.
captchaResponse_failureReason :: Lens.Lens' CaptchaResponse (Prelude.Maybe FailureReason)
captchaResponse_failureReason = Lens.lens (\CaptchaResponse' {failureReason} -> failureReason) (\s@CaptchaResponse' {} a -> s {failureReason = a} :: CaptchaResponse)

-- | The HTTP response code indicating the status of the @CAPTCHA@ token in
-- the web request. If the token is missing, invalid, or expired, this code
-- is @405 Method Not Allowed@.
captchaResponse_responseCode :: Lens.Lens' CaptchaResponse (Prelude.Maybe Prelude.Int)
captchaResponse_responseCode = Lens.lens (\CaptchaResponse' {responseCode} -> responseCode) (\s@CaptchaResponse' {} a -> s {responseCode = a} :: CaptchaResponse)

-- | The time that the @CAPTCHA@ was last solved for the supplied token.
captchaResponse_solveTimestamp :: Lens.Lens' CaptchaResponse (Prelude.Maybe Prelude.Integer)
captchaResponse_solveTimestamp = Lens.lens (\CaptchaResponse' {solveTimestamp} -> solveTimestamp) (\s@CaptchaResponse' {} a -> s {solveTimestamp = a} :: CaptchaResponse)

instance Data.FromJSON CaptchaResponse where
  parseJSON =
    Data.withObject
      "CaptchaResponse"
      ( \x ->
          CaptchaResponse'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "ResponseCode")
            Prelude.<*> (x Data..:? "SolveTimestamp")
      )

instance Prelude.Hashable CaptchaResponse where
  hashWithSalt _salt CaptchaResponse' {..} =
    _salt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` responseCode
      `Prelude.hashWithSalt` solveTimestamp

instance Prelude.NFData CaptchaResponse where
  rnf CaptchaResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf responseCode
      `Prelude.seq` Prelude.rnf solveTimestamp
