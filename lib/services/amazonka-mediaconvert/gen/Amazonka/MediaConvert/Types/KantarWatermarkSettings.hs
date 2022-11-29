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
-- Module      : Amazonka.MediaConvert.Types.KantarWatermarkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.KantarWatermarkSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
--
-- /See:/ 'newKantarWatermarkSettings' smart constructor.
data KantarWatermarkSettings = KantarWatermarkSettings'
  { -- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
    -- 50 characters.
    metadata4 :: Prelude.Maybe Prelude.Text,
    -- | Specify a unique identifier for Kantar to use for this piece of content.
    contentReference :: Prelude.Maybe Prelude.Text,
    -- | Provide an audio channel name from your Kantar audio license.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
    -- 50 characters.
    metadata7 :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
    -- 50 characters.
    metadata5 :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify the Amazon S3 bucket where you want MediaConvert to
    -- store your Kantar watermark XML logs. When you don\'t specify a bucket,
    -- MediaConvert doesn\'t save these logs. Note that your MediaConvert
    -- service role must provide access to this location. For more information,
    -- see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
    logDestination :: Prelude.Maybe Prelude.Text,
    -- | You can optionally use this field to specify the first timestamp that
    -- Kantar embeds during watermarking. Kantar suggests that you be very
    -- cautious when using this Kantar feature, and that you use it only on
    -- channels that are managed specifically for use with this feature by your
    -- Audience Measurement Operator. For more information about this feature,
    -- contact Kantar technical support.
    metadata3 :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify an offset, in whole seconds, from the start of your
    -- output and the beginning of the watermarking. When you don\'t specify an
    -- offset, Kantar defaults to zero.
    fileOffset :: Prelude.Maybe Prelude.Double,
    -- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
    -- 50 characters.
    metadata8 :: Prelude.Maybe Prelude.Text,
    -- | Provide your Kantar license ID number. You should get this number from
    -- Kantar.
    kantarLicenseId :: Prelude.Maybe Prelude.Natural,
    -- | Provide the HTTPS endpoint to the Kantar server. You should get this
    -- endpoint from Kantar.
    kantarServerUrl :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
    -- 50 characters.
    metadata6 :: Prelude.Maybe Prelude.Text,
    -- | Provide the name of the AWS Secrets Manager secret where your Kantar
    -- credentials are stored. Note that your MediaConvert service role must
    -- provide access to this secret. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/granting-permissions-for-mediaconvert-to-access-secrets-manager-secret.html.
    -- For instructions on creating a secret, see
    -- https:\/\/docs.aws.amazon.com\/secretsmanager\/latest\/userguide\/tutorials_basic.html,
    -- in the AWS Secrets Manager User Guide.
    credentialsSecretName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KantarWatermarkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata4', 'kantarWatermarkSettings_metadata4' - Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
--
-- 'contentReference', 'kantarWatermarkSettings_contentReference' - Specify a unique identifier for Kantar to use for this piece of content.
--
-- 'channelName', 'kantarWatermarkSettings_channelName' - Provide an audio channel name from your Kantar audio license.
--
-- 'metadata7', 'kantarWatermarkSettings_metadata7' - Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
--
-- 'metadata5', 'kantarWatermarkSettings_metadata5' - Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
--
-- 'logDestination', 'kantarWatermarkSettings_logDestination' - Optional. Specify the Amazon S3 bucket where you want MediaConvert to
-- store your Kantar watermark XML logs. When you don\'t specify a bucket,
-- MediaConvert doesn\'t save these logs. Note that your MediaConvert
-- service role must provide access to this location. For more information,
-- see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
--
-- 'metadata3', 'kantarWatermarkSettings_metadata3' - You can optionally use this field to specify the first timestamp that
-- Kantar embeds during watermarking. Kantar suggests that you be very
-- cautious when using this Kantar feature, and that you use it only on
-- channels that are managed specifically for use with this feature by your
-- Audience Measurement Operator. For more information about this feature,
-- contact Kantar technical support.
--
-- 'fileOffset', 'kantarWatermarkSettings_fileOffset' - Optional. Specify an offset, in whole seconds, from the start of your
-- output and the beginning of the watermarking. When you don\'t specify an
-- offset, Kantar defaults to zero.
--
-- 'metadata8', 'kantarWatermarkSettings_metadata8' - Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
--
-- 'kantarLicenseId', 'kantarWatermarkSettings_kantarLicenseId' - Provide your Kantar license ID number. You should get this number from
-- Kantar.
--
-- 'kantarServerUrl', 'kantarWatermarkSettings_kantarServerUrl' - Provide the HTTPS endpoint to the Kantar server. You should get this
-- endpoint from Kantar.
--
-- 'metadata6', 'kantarWatermarkSettings_metadata6' - Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
--
-- 'credentialsSecretName', 'kantarWatermarkSettings_credentialsSecretName' - Provide the name of the AWS Secrets Manager secret where your Kantar
-- credentials are stored. Note that your MediaConvert service role must
-- provide access to this secret. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/granting-permissions-for-mediaconvert-to-access-secrets-manager-secret.html.
-- For instructions on creating a secret, see
-- https:\/\/docs.aws.amazon.com\/secretsmanager\/latest\/userguide\/tutorials_basic.html,
-- in the AWS Secrets Manager User Guide.
newKantarWatermarkSettings ::
  KantarWatermarkSettings
newKantarWatermarkSettings =
  KantarWatermarkSettings'
    { metadata4 =
        Prelude.Nothing,
      contentReference = Prelude.Nothing,
      channelName = Prelude.Nothing,
      metadata7 = Prelude.Nothing,
      metadata5 = Prelude.Nothing,
      logDestination = Prelude.Nothing,
      metadata3 = Prelude.Nothing,
      fileOffset = Prelude.Nothing,
      metadata8 = Prelude.Nothing,
      kantarLicenseId = Prelude.Nothing,
      kantarServerUrl = Prelude.Nothing,
      metadata6 = Prelude.Nothing,
      credentialsSecretName = Prelude.Nothing
    }

-- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
kantarWatermarkSettings_metadata4 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata4 = Lens.lens (\KantarWatermarkSettings' {metadata4} -> metadata4) (\s@KantarWatermarkSettings' {} a -> s {metadata4 = a} :: KantarWatermarkSettings)

-- | Specify a unique identifier for Kantar to use for this piece of content.
kantarWatermarkSettings_contentReference :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_contentReference = Lens.lens (\KantarWatermarkSettings' {contentReference} -> contentReference) (\s@KantarWatermarkSettings' {} a -> s {contentReference = a} :: KantarWatermarkSettings)

-- | Provide an audio channel name from your Kantar audio license.
kantarWatermarkSettings_channelName :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_channelName = Lens.lens (\KantarWatermarkSettings' {channelName} -> channelName) (\s@KantarWatermarkSettings' {} a -> s {channelName = a} :: KantarWatermarkSettings)

-- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
kantarWatermarkSettings_metadata7 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata7 = Lens.lens (\KantarWatermarkSettings' {metadata7} -> metadata7) (\s@KantarWatermarkSettings' {} a -> s {metadata7 = a} :: KantarWatermarkSettings)

-- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
kantarWatermarkSettings_metadata5 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata5 = Lens.lens (\KantarWatermarkSettings' {metadata5} -> metadata5) (\s@KantarWatermarkSettings' {} a -> s {metadata5 = a} :: KantarWatermarkSettings)

-- | Optional. Specify the Amazon S3 bucket where you want MediaConvert to
-- store your Kantar watermark XML logs. When you don\'t specify a bucket,
-- MediaConvert doesn\'t save these logs. Note that your MediaConvert
-- service role must provide access to this location. For more information,
-- see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/iam-role.html
kantarWatermarkSettings_logDestination :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_logDestination = Lens.lens (\KantarWatermarkSettings' {logDestination} -> logDestination) (\s@KantarWatermarkSettings' {} a -> s {logDestination = a} :: KantarWatermarkSettings)

-- | You can optionally use this field to specify the first timestamp that
-- Kantar embeds during watermarking. Kantar suggests that you be very
-- cautious when using this Kantar feature, and that you use it only on
-- channels that are managed specifically for use with this feature by your
-- Audience Measurement Operator. For more information about this feature,
-- contact Kantar technical support.
kantarWatermarkSettings_metadata3 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata3 = Lens.lens (\KantarWatermarkSettings' {metadata3} -> metadata3) (\s@KantarWatermarkSettings' {} a -> s {metadata3 = a} :: KantarWatermarkSettings)

-- | Optional. Specify an offset, in whole seconds, from the start of your
-- output and the beginning of the watermarking. When you don\'t specify an
-- offset, Kantar defaults to zero.
kantarWatermarkSettings_fileOffset :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Double)
kantarWatermarkSettings_fileOffset = Lens.lens (\KantarWatermarkSettings' {fileOffset} -> fileOffset) (\s@KantarWatermarkSettings' {} a -> s {fileOffset = a} :: KantarWatermarkSettings)

-- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
kantarWatermarkSettings_metadata8 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata8 = Lens.lens (\KantarWatermarkSettings' {metadata8} -> metadata8) (\s@KantarWatermarkSettings' {} a -> s {metadata8 = a} :: KantarWatermarkSettings)

-- | Provide your Kantar license ID number. You should get this number from
-- Kantar.
kantarWatermarkSettings_kantarLicenseId :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Natural)
kantarWatermarkSettings_kantarLicenseId = Lens.lens (\KantarWatermarkSettings' {kantarLicenseId} -> kantarLicenseId) (\s@KantarWatermarkSettings' {} a -> s {kantarLicenseId = a} :: KantarWatermarkSettings)

-- | Provide the HTTPS endpoint to the Kantar server. You should get this
-- endpoint from Kantar.
kantarWatermarkSettings_kantarServerUrl :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_kantarServerUrl = Lens.lens (\KantarWatermarkSettings' {kantarServerUrl} -> kantarServerUrl) (\s@KantarWatermarkSettings' {} a -> s {kantarServerUrl = a} :: KantarWatermarkSettings)

-- | Additional metadata that MediaConvert sends to Kantar. Maximum length is
-- 50 characters.
kantarWatermarkSettings_metadata6 :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_metadata6 = Lens.lens (\KantarWatermarkSettings' {metadata6} -> metadata6) (\s@KantarWatermarkSettings' {} a -> s {metadata6 = a} :: KantarWatermarkSettings)

-- | Provide the name of the AWS Secrets Manager secret where your Kantar
-- credentials are stored. Note that your MediaConvert service role must
-- provide access to this secret. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/granting-permissions-for-mediaconvert-to-access-secrets-manager-secret.html.
-- For instructions on creating a secret, see
-- https:\/\/docs.aws.amazon.com\/secretsmanager\/latest\/userguide\/tutorials_basic.html,
-- in the AWS Secrets Manager User Guide.
kantarWatermarkSettings_credentialsSecretName :: Lens.Lens' KantarWatermarkSettings (Prelude.Maybe Prelude.Text)
kantarWatermarkSettings_credentialsSecretName = Lens.lens (\KantarWatermarkSettings' {credentialsSecretName} -> credentialsSecretName) (\s@KantarWatermarkSettings' {} a -> s {credentialsSecretName = a} :: KantarWatermarkSettings)

instance Core.FromJSON KantarWatermarkSettings where
  parseJSON =
    Core.withObject
      "KantarWatermarkSettings"
      ( \x ->
          KantarWatermarkSettings'
            Prelude.<$> (x Core..:? "metadata4")
            Prelude.<*> (x Core..:? "contentReference")
            Prelude.<*> (x Core..:? "channelName")
            Prelude.<*> (x Core..:? "metadata7")
            Prelude.<*> (x Core..:? "metadata5")
            Prelude.<*> (x Core..:? "logDestination")
            Prelude.<*> (x Core..:? "metadata3")
            Prelude.<*> (x Core..:? "fileOffset")
            Prelude.<*> (x Core..:? "metadata8")
            Prelude.<*> (x Core..:? "kantarLicenseId")
            Prelude.<*> (x Core..:? "kantarServerUrl")
            Prelude.<*> (x Core..:? "metadata6")
            Prelude.<*> (x Core..:? "credentialsSecretName")
      )

instance Prelude.Hashable KantarWatermarkSettings where
  hashWithSalt _salt KantarWatermarkSettings' {..} =
    _salt `Prelude.hashWithSalt` metadata4
      `Prelude.hashWithSalt` contentReference
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` metadata7
      `Prelude.hashWithSalt` metadata5
      `Prelude.hashWithSalt` logDestination
      `Prelude.hashWithSalt` metadata3
      `Prelude.hashWithSalt` fileOffset
      `Prelude.hashWithSalt` metadata8
      `Prelude.hashWithSalt` kantarLicenseId
      `Prelude.hashWithSalt` kantarServerUrl
      `Prelude.hashWithSalt` metadata6
      `Prelude.hashWithSalt` credentialsSecretName

instance Prelude.NFData KantarWatermarkSettings where
  rnf KantarWatermarkSettings' {..} =
    Prelude.rnf metadata4
      `Prelude.seq` Prelude.rnf contentReference
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf metadata7
      `Prelude.seq` Prelude.rnf metadata5
      `Prelude.seq` Prelude.rnf logDestination
      `Prelude.seq` Prelude.rnf metadata3
      `Prelude.seq` Prelude.rnf fileOffset
      `Prelude.seq` Prelude.rnf metadata8
      `Prelude.seq` Prelude.rnf kantarLicenseId
      `Prelude.seq` Prelude.rnf kantarServerUrl
      `Prelude.seq` Prelude.rnf metadata6
      `Prelude.seq` Prelude.rnf credentialsSecretName

instance Core.ToJSON KantarWatermarkSettings where
  toJSON KantarWatermarkSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("metadata4" Core..=) Prelude.<$> metadata4,
            ("contentReference" Core..=)
              Prelude.<$> contentReference,
            ("channelName" Core..=) Prelude.<$> channelName,
            ("metadata7" Core..=) Prelude.<$> metadata7,
            ("metadata5" Core..=) Prelude.<$> metadata5,
            ("logDestination" Core..=)
              Prelude.<$> logDestination,
            ("metadata3" Core..=) Prelude.<$> metadata3,
            ("fileOffset" Core..=) Prelude.<$> fileOffset,
            ("metadata8" Core..=) Prelude.<$> metadata8,
            ("kantarLicenseId" Core..=)
              Prelude.<$> kantarLicenseId,
            ("kantarServerUrl" Core..=)
              Prelude.<$> kantarServerUrl,
            ("metadata6" Core..=) Prelude.<$> metadata6,
            ("credentialsSecretName" Core..=)
              Prelude.<$> credentialsSecretName
          ]
      )
