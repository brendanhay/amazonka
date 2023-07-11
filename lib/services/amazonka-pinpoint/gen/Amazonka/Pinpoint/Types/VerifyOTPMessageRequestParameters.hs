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
-- Module      : Amazonka.Pinpoint.Types.VerifyOTPMessageRequestParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.VerifyOTPMessageRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Verify OTP message request.
--
-- /See:/ 'newVerifyOTPMessageRequestParameters' smart constructor.
data VerifyOTPMessageRequestParameters = VerifyOTPMessageRequestParameters'
  { -- | The reference identifier provided when the OTP was previously sent.
    referenceId :: Prelude.Text,
    -- | The OTP the end user provided for verification.
    otp :: Prelude.Text,
    -- | The destination identity to send OTP to.
    destinationIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyOTPMessageRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceId', 'verifyOTPMessageRequestParameters_referenceId' - The reference identifier provided when the OTP was previously sent.
--
-- 'otp', 'verifyOTPMessageRequestParameters_otp' - The OTP the end user provided for verification.
--
-- 'destinationIdentity', 'verifyOTPMessageRequestParameters_destinationIdentity' - The destination identity to send OTP to.
newVerifyOTPMessageRequestParameters ::
  -- | 'referenceId'
  Prelude.Text ->
  -- | 'otp'
  Prelude.Text ->
  -- | 'destinationIdentity'
  Prelude.Text ->
  VerifyOTPMessageRequestParameters
newVerifyOTPMessageRequestParameters
  pReferenceId_
  pOtp_
  pDestinationIdentity_ =
    VerifyOTPMessageRequestParameters'
      { referenceId =
          pReferenceId_,
        otp = pOtp_,
        destinationIdentity =
          pDestinationIdentity_
      }

-- | The reference identifier provided when the OTP was previously sent.
verifyOTPMessageRequestParameters_referenceId :: Lens.Lens' VerifyOTPMessageRequestParameters Prelude.Text
verifyOTPMessageRequestParameters_referenceId = Lens.lens (\VerifyOTPMessageRequestParameters' {referenceId} -> referenceId) (\s@VerifyOTPMessageRequestParameters' {} a -> s {referenceId = a} :: VerifyOTPMessageRequestParameters)

-- | The OTP the end user provided for verification.
verifyOTPMessageRequestParameters_otp :: Lens.Lens' VerifyOTPMessageRequestParameters Prelude.Text
verifyOTPMessageRequestParameters_otp = Lens.lens (\VerifyOTPMessageRequestParameters' {otp} -> otp) (\s@VerifyOTPMessageRequestParameters' {} a -> s {otp = a} :: VerifyOTPMessageRequestParameters)

-- | The destination identity to send OTP to.
verifyOTPMessageRequestParameters_destinationIdentity :: Lens.Lens' VerifyOTPMessageRequestParameters Prelude.Text
verifyOTPMessageRequestParameters_destinationIdentity = Lens.lens (\VerifyOTPMessageRequestParameters' {destinationIdentity} -> destinationIdentity) (\s@VerifyOTPMessageRequestParameters' {} a -> s {destinationIdentity = a} :: VerifyOTPMessageRequestParameters)

instance
  Prelude.Hashable
    VerifyOTPMessageRequestParameters
  where
  hashWithSalt
    _salt
    VerifyOTPMessageRequestParameters' {..} =
      _salt
        `Prelude.hashWithSalt` referenceId
        `Prelude.hashWithSalt` otp
        `Prelude.hashWithSalt` destinationIdentity

instance
  Prelude.NFData
    VerifyOTPMessageRequestParameters
  where
  rnf VerifyOTPMessageRequestParameters' {..} =
    Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf otp
      `Prelude.seq` Prelude.rnf destinationIdentity

instance
  Data.ToJSON
    VerifyOTPMessageRequestParameters
  where
  toJSON VerifyOTPMessageRequestParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ReferenceId" Data..= referenceId),
            Prelude.Just ("Otp" Data..= otp),
            Prelude.Just
              ("DestinationIdentity" Data..= destinationIdentity)
          ]
      )
