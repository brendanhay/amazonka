{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.StartBotRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this to provide your transcript data, and to start the bot
-- recommendation process.
module Amazonka.LexV2Models.StartBotRecommendation
  ( -- * Creating a Request
    StartBotRecommendation (..),
    newStartBotRecommendation,

    -- * Request Lenses
    startBotRecommendation_encryptionSetting,
    startBotRecommendation_botId,
    startBotRecommendation_botVersion,
    startBotRecommendation_localeId,
    startBotRecommendation_transcriptSourceSetting,

    -- * Destructuring the Response
    StartBotRecommendationResponse (..),
    newStartBotRecommendationResponse,

    -- * Response Lenses
    startBotRecommendationResponse_botId,
    startBotRecommendationResponse_botRecommendationId,
    startBotRecommendationResponse_botRecommendationStatus,
    startBotRecommendationResponse_botVersion,
    startBotRecommendationResponse_creationDateTime,
    startBotRecommendationResponse_encryptionSetting,
    startBotRecommendationResponse_localeId,
    startBotRecommendationResponse_transcriptSourceSetting,
    startBotRecommendationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartBotRecommendation' smart constructor.
data StartBotRecommendation = StartBotRecommendation'
  { -- | The object representing the passwords that will be used to encrypt the
    -- data related to the bot recommendation results, as well as the KMS key
    -- ARN used to encrypt the associated metadata.
    encryptionSetting :: Prelude.Maybe EncryptionSetting,
    -- | The unique identifier of the bot containing the bot recommendation.
    botId :: Prelude.Text,
    -- | The version of the bot containing the bot recommendation.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the bot recommendation to
    -- start. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Text,
    -- | The object representing the Amazon S3 bucket containing the transcript,
    -- as well as the associated metadata.
    transcriptSourceSetting :: TranscriptSourceSetting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBotRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionSetting', 'startBotRecommendation_encryptionSetting' - The object representing the passwords that will be used to encrypt the
-- data related to the bot recommendation results, as well as the KMS key
-- ARN used to encrypt the associated metadata.
--
-- 'botId', 'startBotRecommendation_botId' - The unique identifier of the bot containing the bot recommendation.
--
-- 'botVersion', 'startBotRecommendation_botVersion' - The version of the bot containing the bot recommendation.
--
-- 'localeId', 'startBotRecommendation_localeId' - The identifier of the language and locale of the bot recommendation to
-- start. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'transcriptSourceSetting', 'startBotRecommendation_transcriptSourceSetting' - The object representing the Amazon S3 bucket containing the transcript,
-- as well as the associated metadata.
newStartBotRecommendation ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'transcriptSourceSetting'
  TranscriptSourceSetting ->
  StartBotRecommendation
newStartBotRecommendation
  pBotId_
  pBotVersion_
  pLocaleId_
  pTranscriptSourceSetting_ =
    StartBotRecommendation'
      { encryptionSetting =
          Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        transcriptSourceSetting = pTranscriptSourceSetting_
      }

-- | The object representing the passwords that will be used to encrypt the
-- data related to the bot recommendation results, as well as the KMS key
-- ARN used to encrypt the associated metadata.
startBotRecommendation_encryptionSetting :: Lens.Lens' StartBotRecommendation (Prelude.Maybe EncryptionSetting)
startBotRecommendation_encryptionSetting = Lens.lens (\StartBotRecommendation' {encryptionSetting} -> encryptionSetting) (\s@StartBotRecommendation' {} a -> s {encryptionSetting = a} :: StartBotRecommendation)

-- | The unique identifier of the bot containing the bot recommendation.
startBotRecommendation_botId :: Lens.Lens' StartBotRecommendation Prelude.Text
startBotRecommendation_botId = Lens.lens (\StartBotRecommendation' {botId} -> botId) (\s@StartBotRecommendation' {} a -> s {botId = a} :: StartBotRecommendation)

-- | The version of the bot containing the bot recommendation.
startBotRecommendation_botVersion :: Lens.Lens' StartBotRecommendation Prelude.Text
startBotRecommendation_botVersion = Lens.lens (\StartBotRecommendation' {botVersion} -> botVersion) (\s@StartBotRecommendation' {} a -> s {botVersion = a} :: StartBotRecommendation)

-- | The identifier of the language and locale of the bot recommendation to
-- start. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
startBotRecommendation_localeId :: Lens.Lens' StartBotRecommendation Prelude.Text
startBotRecommendation_localeId = Lens.lens (\StartBotRecommendation' {localeId} -> localeId) (\s@StartBotRecommendation' {} a -> s {localeId = a} :: StartBotRecommendation)

-- | The object representing the Amazon S3 bucket containing the transcript,
-- as well as the associated metadata.
startBotRecommendation_transcriptSourceSetting :: Lens.Lens' StartBotRecommendation TranscriptSourceSetting
startBotRecommendation_transcriptSourceSetting = Lens.lens (\StartBotRecommendation' {transcriptSourceSetting} -> transcriptSourceSetting) (\s@StartBotRecommendation' {} a -> s {transcriptSourceSetting = a} :: StartBotRecommendation)

instance Core.AWSRequest StartBotRecommendation where
  type
    AWSResponse StartBotRecommendation =
      StartBotRecommendationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBotRecommendationResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botRecommendationId")
            Prelude.<*> (x Data..?> "botRecommendationStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "encryptionSetting")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "transcriptSourceSetting")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBotRecommendation where
  hashWithSalt _salt StartBotRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionSetting
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` transcriptSourceSetting

instance Prelude.NFData StartBotRecommendation where
  rnf StartBotRecommendation' {..} =
    Prelude.rnf encryptionSetting
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf transcriptSourceSetting

instance Data.ToHeaders StartBotRecommendation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartBotRecommendation where
  toJSON StartBotRecommendation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionSetting" Data..=)
              Prelude.<$> encryptionSetting,
            Prelude.Just
              ( "transcriptSourceSetting"
                  Data..= transcriptSourceSetting
              )
          ]
      )

instance Data.ToPath StartBotRecommendation where
  toPath StartBotRecommendation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/botrecommendations/"
      ]

instance Data.ToQuery StartBotRecommendation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBotRecommendationResponse' smart constructor.
data StartBotRecommendationResponse = StartBotRecommendationResponse'
  { -- | The unique identifier of the bot containing the bot recommendation.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot recommendation that you have created.
    botRecommendationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the bot recommendation.
    --
    -- If the status is Failed, then the reasons for the failure are listed in
    -- the failureReasons field.
    botRecommendationStatus :: Prelude.Maybe BotRecommendationStatus,
    -- | The version of the bot containing the bot recommendation.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot recommendation was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The object representing the passwords that were used to encrypt the data
    -- related to the bot recommendation results, as well as the KMS key ARN
    -- used to encrypt the associated metadata.
    encryptionSetting :: Prelude.Maybe EncryptionSetting,
    -- | The identifier of the language and locale of the bot recommendation to
    -- start. The string must match one of the supported locales. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The object representing the Amazon S3 bucket containing the transcript,
    -- as well as the associated metadata.
    transcriptSourceSetting :: Prelude.Maybe TranscriptSourceSetting,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBotRecommendationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'startBotRecommendationResponse_botId' - The unique identifier of the bot containing the bot recommendation.
--
-- 'botRecommendationId', 'startBotRecommendationResponse_botRecommendationId' - The identifier of the bot recommendation that you have created.
--
-- 'botRecommendationStatus', 'startBotRecommendationResponse_botRecommendationStatus' - The status of the bot recommendation.
--
-- If the status is Failed, then the reasons for the failure are listed in
-- the failureReasons field.
--
-- 'botVersion', 'startBotRecommendationResponse_botVersion' - The version of the bot containing the bot recommendation.
--
-- 'creationDateTime', 'startBotRecommendationResponse_creationDateTime' - A timestamp of the date and time that the bot recommendation was
-- created.
--
-- 'encryptionSetting', 'startBotRecommendationResponse_encryptionSetting' - The object representing the passwords that were used to encrypt the data
-- related to the bot recommendation results, as well as the KMS key ARN
-- used to encrypt the associated metadata.
--
-- 'localeId', 'startBotRecommendationResponse_localeId' - The identifier of the language and locale of the bot recommendation to
-- start. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
--
-- 'transcriptSourceSetting', 'startBotRecommendationResponse_transcriptSourceSetting' - The object representing the Amazon S3 bucket containing the transcript,
-- as well as the associated metadata.
--
-- 'httpStatus', 'startBotRecommendationResponse_httpStatus' - The response's http status code.
newStartBotRecommendationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartBotRecommendationResponse
newStartBotRecommendationResponse pHttpStatus_ =
  StartBotRecommendationResponse'
    { botId =
        Prelude.Nothing,
      botRecommendationId = Prelude.Nothing,
      botRecommendationStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      encryptionSetting = Prelude.Nothing,
      localeId = Prelude.Nothing,
      transcriptSourceSetting = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot containing the bot recommendation.
startBotRecommendationResponse_botId :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe Prelude.Text)
startBotRecommendationResponse_botId = Lens.lens (\StartBotRecommendationResponse' {botId} -> botId) (\s@StartBotRecommendationResponse' {} a -> s {botId = a} :: StartBotRecommendationResponse)

-- | The identifier of the bot recommendation that you have created.
startBotRecommendationResponse_botRecommendationId :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe Prelude.Text)
startBotRecommendationResponse_botRecommendationId = Lens.lens (\StartBotRecommendationResponse' {botRecommendationId} -> botRecommendationId) (\s@StartBotRecommendationResponse' {} a -> s {botRecommendationId = a} :: StartBotRecommendationResponse)

-- | The status of the bot recommendation.
--
-- If the status is Failed, then the reasons for the failure are listed in
-- the failureReasons field.
startBotRecommendationResponse_botRecommendationStatus :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe BotRecommendationStatus)
startBotRecommendationResponse_botRecommendationStatus = Lens.lens (\StartBotRecommendationResponse' {botRecommendationStatus} -> botRecommendationStatus) (\s@StartBotRecommendationResponse' {} a -> s {botRecommendationStatus = a} :: StartBotRecommendationResponse)

-- | The version of the bot containing the bot recommendation.
startBotRecommendationResponse_botVersion :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe Prelude.Text)
startBotRecommendationResponse_botVersion = Lens.lens (\StartBotRecommendationResponse' {botVersion} -> botVersion) (\s@StartBotRecommendationResponse' {} a -> s {botVersion = a} :: StartBotRecommendationResponse)

-- | A timestamp of the date and time that the bot recommendation was
-- created.
startBotRecommendationResponse_creationDateTime :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe Prelude.UTCTime)
startBotRecommendationResponse_creationDateTime = Lens.lens (\StartBotRecommendationResponse' {creationDateTime} -> creationDateTime) (\s@StartBotRecommendationResponse' {} a -> s {creationDateTime = a} :: StartBotRecommendationResponse) Prelude.. Lens.mapping Data._Time

-- | The object representing the passwords that were used to encrypt the data
-- related to the bot recommendation results, as well as the KMS key ARN
-- used to encrypt the associated metadata.
startBotRecommendationResponse_encryptionSetting :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe EncryptionSetting)
startBotRecommendationResponse_encryptionSetting = Lens.lens (\StartBotRecommendationResponse' {encryptionSetting} -> encryptionSetting) (\s@StartBotRecommendationResponse' {} a -> s {encryptionSetting = a} :: StartBotRecommendationResponse)

-- | The identifier of the language and locale of the bot recommendation to
-- start. The string must match one of the supported locales. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
startBotRecommendationResponse_localeId :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe Prelude.Text)
startBotRecommendationResponse_localeId = Lens.lens (\StartBotRecommendationResponse' {localeId} -> localeId) (\s@StartBotRecommendationResponse' {} a -> s {localeId = a} :: StartBotRecommendationResponse)

-- | The object representing the Amazon S3 bucket containing the transcript,
-- as well as the associated metadata.
startBotRecommendationResponse_transcriptSourceSetting :: Lens.Lens' StartBotRecommendationResponse (Prelude.Maybe TranscriptSourceSetting)
startBotRecommendationResponse_transcriptSourceSetting = Lens.lens (\StartBotRecommendationResponse' {transcriptSourceSetting} -> transcriptSourceSetting) (\s@StartBotRecommendationResponse' {} a -> s {transcriptSourceSetting = a} :: StartBotRecommendationResponse)

-- | The response's http status code.
startBotRecommendationResponse_httpStatus :: Lens.Lens' StartBotRecommendationResponse Prelude.Int
startBotRecommendationResponse_httpStatus = Lens.lens (\StartBotRecommendationResponse' {httpStatus} -> httpStatus) (\s@StartBotRecommendationResponse' {} a -> s {httpStatus = a} :: StartBotRecommendationResponse)

instance
  Prelude.NFData
    StartBotRecommendationResponse
  where
  rnf StartBotRecommendationResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botRecommendationId
      `Prelude.seq` Prelude.rnf botRecommendationStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf encryptionSetting
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf transcriptSourceSetting
      `Prelude.seq` Prelude.rnf httpStatus
