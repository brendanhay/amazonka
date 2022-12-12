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
-- Module      : Amazonka.Pinpoint.Types.SendOTPMessageRequestParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SendOTPMessageRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Send OTP message request parameters.
--
-- /See:/ 'newSendOTPMessageRequestParameters' smart constructor.
data SendOTPMessageRequestParameters = SendOTPMessageRequestParameters'
  { -- | The attempts allowed to validate an OTP.
    allowedAttempts :: Prelude.Maybe Prelude.Int,
    -- | The number of characters in the generated OTP.
    codeLength :: Prelude.Maybe Prelude.Int,
    -- | A unique Entity ID received from DLT after entity registration is
    -- approved.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The language to be used for the outgoing message body containing the
    -- OTP.
    language :: Prelude.Maybe Prelude.Text,
    -- | A unique Template ID received from DLT after entity registration is
    -- approved.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The time in minutes before the OTP is no longer valid.
    validityPeriod :: Prelude.Maybe Prelude.Int,
    -- | The brand name that will be substituted into the OTP message body.
    -- Should be owned by calling AWS account.
    brandName :: Prelude.Text,
    -- | Developer-specified reference identifier. Required to match during OTP
    -- verification.
    referenceId :: Prelude.Text,
    -- | Channel type for the OTP message. Supported values: [SMS].
    channel :: Prelude.Text,
    -- | The destination identity to send OTP to.
    destinationIdentity :: Prelude.Text,
    -- | The origination identity used to send OTP from.
    originationIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendOTPMessageRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedAttempts', 'sendOTPMessageRequestParameters_allowedAttempts' - The attempts allowed to validate an OTP.
--
-- 'codeLength', 'sendOTPMessageRequestParameters_codeLength' - The number of characters in the generated OTP.
--
-- 'entityId', 'sendOTPMessageRequestParameters_entityId' - A unique Entity ID received from DLT after entity registration is
-- approved.
--
-- 'language', 'sendOTPMessageRequestParameters_language' - The language to be used for the outgoing message body containing the
-- OTP.
--
-- 'templateId', 'sendOTPMessageRequestParameters_templateId' - A unique Template ID received from DLT after entity registration is
-- approved.
--
-- 'validityPeriod', 'sendOTPMessageRequestParameters_validityPeriod' - The time in minutes before the OTP is no longer valid.
--
-- 'brandName', 'sendOTPMessageRequestParameters_brandName' - The brand name that will be substituted into the OTP message body.
-- Should be owned by calling AWS account.
--
-- 'referenceId', 'sendOTPMessageRequestParameters_referenceId' - Developer-specified reference identifier. Required to match during OTP
-- verification.
--
-- 'channel', 'sendOTPMessageRequestParameters_channel' - Channel type for the OTP message. Supported values: [SMS].
--
-- 'destinationIdentity', 'sendOTPMessageRequestParameters_destinationIdentity' - The destination identity to send OTP to.
--
-- 'originationIdentity', 'sendOTPMessageRequestParameters_originationIdentity' - The origination identity used to send OTP from.
newSendOTPMessageRequestParameters ::
  -- | 'brandName'
  Prelude.Text ->
  -- | 'referenceId'
  Prelude.Text ->
  -- | 'channel'
  Prelude.Text ->
  -- | 'destinationIdentity'
  Prelude.Text ->
  -- | 'originationIdentity'
  Prelude.Text ->
  SendOTPMessageRequestParameters
newSendOTPMessageRequestParameters
  pBrandName_
  pReferenceId_
  pChannel_
  pDestinationIdentity_
  pOriginationIdentity_ =
    SendOTPMessageRequestParameters'
      { allowedAttempts =
          Prelude.Nothing,
        codeLength = Prelude.Nothing,
        entityId = Prelude.Nothing,
        language = Prelude.Nothing,
        templateId = Prelude.Nothing,
        validityPeriod = Prelude.Nothing,
        brandName = pBrandName_,
        referenceId = pReferenceId_,
        channel = pChannel_,
        destinationIdentity =
          pDestinationIdentity_,
        originationIdentity =
          pOriginationIdentity_
      }

-- | The attempts allowed to validate an OTP.
sendOTPMessageRequestParameters_allowedAttempts :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Int)
sendOTPMessageRequestParameters_allowedAttempts = Lens.lens (\SendOTPMessageRequestParameters' {allowedAttempts} -> allowedAttempts) (\s@SendOTPMessageRequestParameters' {} a -> s {allowedAttempts = a} :: SendOTPMessageRequestParameters)

-- | The number of characters in the generated OTP.
sendOTPMessageRequestParameters_codeLength :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Int)
sendOTPMessageRequestParameters_codeLength = Lens.lens (\SendOTPMessageRequestParameters' {codeLength} -> codeLength) (\s@SendOTPMessageRequestParameters' {} a -> s {codeLength = a} :: SendOTPMessageRequestParameters)

-- | A unique Entity ID received from DLT after entity registration is
-- approved.
sendOTPMessageRequestParameters_entityId :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Text)
sendOTPMessageRequestParameters_entityId = Lens.lens (\SendOTPMessageRequestParameters' {entityId} -> entityId) (\s@SendOTPMessageRequestParameters' {} a -> s {entityId = a} :: SendOTPMessageRequestParameters)

-- | The language to be used for the outgoing message body containing the
-- OTP.
sendOTPMessageRequestParameters_language :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Text)
sendOTPMessageRequestParameters_language = Lens.lens (\SendOTPMessageRequestParameters' {language} -> language) (\s@SendOTPMessageRequestParameters' {} a -> s {language = a} :: SendOTPMessageRequestParameters)

-- | A unique Template ID received from DLT after entity registration is
-- approved.
sendOTPMessageRequestParameters_templateId :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Text)
sendOTPMessageRequestParameters_templateId = Lens.lens (\SendOTPMessageRequestParameters' {templateId} -> templateId) (\s@SendOTPMessageRequestParameters' {} a -> s {templateId = a} :: SendOTPMessageRequestParameters)

-- | The time in minutes before the OTP is no longer valid.
sendOTPMessageRequestParameters_validityPeriod :: Lens.Lens' SendOTPMessageRequestParameters (Prelude.Maybe Prelude.Int)
sendOTPMessageRequestParameters_validityPeriod = Lens.lens (\SendOTPMessageRequestParameters' {validityPeriod} -> validityPeriod) (\s@SendOTPMessageRequestParameters' {} a -> s {validityPeriod = a} :: SendOTPMessageRequestParameters)

-- | The brand name that will be substituted into the OTP message body.
-- Should be owned by calling AWS account.
sendOTPMessageRequestParameters_brandName :: Lens.Lens' SendOTPMessageRequestParameters Prelude.Text
sendOTPMessageRequestParameters_brandName = Lens.lens (\SendOTPMessageRequestParameters' {brandName} -> brandName) (\s@SendOTPMessageRequestParameters' {} a -> s {brandName = a} :: SendOTPMessageRequestParameters)

-- | Developer-specified reference identifier. Required to match during OTP
-- verification.
sendOTPMessageRequestParameters_referenceId :: Lens.Lens' SendOTPMessageRequestParameters Prelude.Text
sendOTPMessageRequestParameters_referenceId = Lens.lens (\SendOTPMessageRequestParameters' {referenceId} -> referenceId) (\s@SendOTPMessageRequestParameters' {} a -> s {referenceId = a} :: SendOTPMessageRequestParameters)

-- | Channel type for the OTP message. Supported values: [SMS].
sendOTPMessageRequestParameters_channel :: Lens.Lens' SendOTPMessageRequestParameters Prelude.Text
sendOTPMessageRequestParameters_channel = Lens.lens (\SendOTPMessageRequestParameters' {channel} -> channel) (\s@SendOTPMessageRequestParameters' {} a -> s {channel = a} :: SendOTPMessageRequestParameters)

-- | The destination identity to send OTP to.
sendOTPMessageRequestParameters_destinationIdentity :: Lens.Lens' SendOTPMessageRequestParameters Prelude.Text
sendOTPMessageRequestParameters_destinationIdentity = Lens.lens (\SendOTPMessageRequestParameters' {destinationIdentity} -> destinationIdentity) (\s@SendOTPMessageRequestParameters' {} a -> s {destinationIdentity = a} :: SendOTPMessageRequestParameters)

-- | The origination identity used to send OTP from.
sendOTPMessageRequestParameters_originationIdentity :: Lens.Lens' SendOTPMessageRequestParameters Prelude.Text
sendOTPMessageRequestParameters_originationIdentity = Lens.lens (\SendOTPMessageRequestParameters' {originationIdentity} -> originationIdentity) (\s@SendOTPMessageRequestParameters' {} a -> s {originationIdentity = a} :: SendOTPMessageRequestParameters)

instance
  Prelude.Hashable
    SendOTPMessageRequestParameters
  where
  hashWithSalt
    _salt
    SendOTPMessageRequestParameters' {..} =
      _salt `Prelude.hashWithSalt` allowedAttempts
        `Prelude.hashWithSalt` codeLength
        `Prelude.hashWithSalt` entityId
        `Prelude.hashWithSalt` language
        `Prelude.hashWithSalt` templateId
        `Prelude.hashWithSalt` validityPeriod
        `Prelude.hashWithSalt` brandName
        `Prelude.hashWithSalt` referenceId
        `Prelude.hashWithSalt` channel
        `Prelude.hashWithSalt` destinationIdentity
        `Prelude.hashWithSalt` originationIdentity

instance
  Prelude.NFData
    SendOTPMessageRequestParameters
  where
  rnf SendOTPMessageRequestParameters' {..} =
    Prelude.rnf allowedAttempts
      `Prelude.seq` Prelude.rnf codeLength
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf validityPeriod
      `Prelude.seq` Prelude.rnf brandName
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf channel
      `Prelude.seq` Prelude.rnf destinationIdentity
      `Prelude.seq` Prelude.rnf originationIdentity

instance Data.ToJSON SendOTPMessageRequestParameters where
  toJSON SendOTPMessageRequestParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowedAttempts" Data..=)
              Prelude.<$> allowedAttempts,
            ("CodeLength" Data..=) Prelude.<$> codeLength,
            ("EntityId" Data..=) Prelude.<$> entityId,
            ("Language" Data..=) Prelude.<$> language,
            ("TemplateId" Data..=) Prelude.<$> templateId,
            ("ValidityPeriod" Data..=)
              Prelude.<$> validityPeriod,
            Prelude.Just ("BrandName" Data..= brandName),
            Prelude.Just ("ReferenceId" Data..= referenceId),
            Prelude.Just ("Channel" Data..= channel),
            Prelude.Just
              ("DestinationIdentity" Data..= destinationIdentity),
            Prelude.Just
              ("OriginationIdentity" Data..= originationIdentity)
          ]
      )
