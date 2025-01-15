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
-- Module      : Amazonka.MediaTailor.PutPlaybackConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a playback configuration. For information about MediaTailor
-- configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
module Amazonka.MediaTailor.PutPlaybackConfiguration
  ( -- * Creating a Request
    PutPlaybackConfiguration (..),
    newPutPlaybackConfiguration,

    -- * Request Lenses
    putPlaybackConfiguration_adDecisionServerUrl,
    putPlaybackConfiguration_availSuppression,
    putPlaybackConfiguration_bumper,
    putPlaybackConfiguration_cdnConfiguration,
    putPlaybackConfiguration_configurationAliases,
    putPlaybackConfiguration_dashConfiguration,
    putPlaybackConfiguration_livePreRollConfiguration,
    putPlaybackConfiguration_manifestProcessingRules,
    putPlaybackConfiguration_personalizationThresholdSeconds,
    putPlaybackConfiguration_slateAdUrl,
    putPlaybackConfiguration_tags,
    putPlaybackConfiguration_transcodeProfileName,
    putPlaybackConfiguration_videoContentSourceUrl,
    putPlaybackConfiguration_name,

    -- * Destructuring the Response
    PutPlaybackConfigurationResponse (..),
    newPutPlaybackConfigurationResponse,

    -- * Response Lenses
    putPlaybackConfigurationResponse_adDecisionServerUrl,
    putPlaybackConfigurationResponse_availSuppression,
    putPlaybackConfigurationResponse_bumper,
    putPlaybackConfigurationResponse_cdnConfiguration,
    putPlaybackConfigurationResponse_configurationAliases,
    putPlaybackConfigurationResponse_dashConfiguration,
    putPlaybackConfigurationResponse_hlsConfiguration,
    putPlaybackConfigurationResponse_livePreRollConfiguration,
    putPlaybackConfigurationResponse_logConfiguration,
    putPlaybackConfigurationResponse_manifestProcessingRules,
    putPlaybackConfigurationResponse_name,
    putPlaybackConfigurationResponse_personalizationThresholdSeconds,
    putPlaybackConfigurationResponse_playbackConfigurationArn,
    putPlaybackConfigurationResponse_playbackEndpointPrefix,
    putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    putPlaybackConfigurationResponse_slateAdUrl,
    putPlaybackConfigurationResponse_tags,
    putPlaybackConfigurationResponse_transcodeProfileName,
    putPlaybackConfigurationResponse_videoContentSourceUrl,
    putPlaybackConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPlaybackConfiguration' smart constructor.
data PutPlaybackConfiguration = PutPlaybackConfiguration'
  { -- | The URL for the ad decision server (ADS). This includes the
    -- specification of static parameters and placeholders for dynamic
    -- parameters. AWS Elemental MediaTailor substitutes player-specific and
    -- session-specific parameters as needed when calling the ADS. Alternately,
    -- for testing you can provide a static VAST URL. The maximum length is
    -- 25,000 characters.
    adDecisionServerUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for avail suppression, also known as ad suppression.
    -- For more information about ad suppression, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
    availSuppression :: Prelude.Maybe AvailSuppression,
    -- | The configuration for bumpers. Bumpers are short audio or video clips
    -- that play at the start or before the end of an ad break. To learn more
    -- about bumpers, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
    bumper :: Prelude.Maybe Bumper,
    -- | The configuration for using a content delivery network (CDN), like
    -- Amazon CloudFront, for content and ad segment management.
    cdnConfiguration :: Prelude.Maybe CdnConfiguration,
    -- | The player parameters and aliases used as dynamic variables during
    -- session initialization. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
    configurationAliases :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The configuration for DASH content.
    dashConfiguration :: Prelude.Maybe DashConfigurationForPut,
    -- | The configuration for pre-roll ad insertion.
    livePreRollConfiguration :: Prelude.Maybe LivePreRollConfiguration,
    -- | The configuration for manifest processing rules. Manifest processing
    -- rules enable customization of the personalized manifests created by
    -- MediaTailor.
    manifestProcessingRules :: Prelude.Maybe ManifestProcessingRules,
    -- | Defines the maximum duration of underfilled ad time (in seconds) allowed
    -- in an ad break. If the duration of underfilled ad time exceeds the
    -- personalization threshold, then the personalization of the ad break is
    -- abandoned and the underlying content is shown. This feature applies to
    -- /ad replacement/ in live and VOD streams, rather than ad insertion,
    -- because it relies on an underlying content stream. For more information
    -- about ad break behavior, including ad replacement and insertion, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
    personalizationThresholdSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The URL for a high-quality video asset to transcode and use to fill in
    -- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
    -- to fill in gaps in media content. Configuring the slate is optional for
    -- non-VPAID configurations. For VPAID, the slate is required because
    -- MediaTailor provides it in the slots that are designated for dynamic ad
    -- content. The slate must be a high-quality asset that contains both audio
    -- and video.
    slateAdUrl :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the playback configuration. Tags are key-value
    -- pairs that you can associate with Amazon resources to help with
    -- organization, access control, and cost tracking. For more information,
    -- see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name that is used to associate this playback configuration with a
    -- custom transcode profile. This overrides the dynamic transcoding
    -- defaults of MediaTailor. Use this only if you have already set up custom
    -- profiles with the help of AWS Support.
    transcodeProfileName :: Prelude.Maybe Prelude.Text,
    -- | The URL prefix for the parent manifest for the stream, minus the asset
    -- ID. The maximum length is 512 characters.
    videoContentSourceUrl :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the playback configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPlaybackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adDecisionServerUrl', 'putPlaybackConfiguration_adDecisionServerUrl' - The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
--
-- 'availSuppression', 'putPlaybackConfiguration_availSuppression' - The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
--
-- 'bumper', 'putPlaybackConfiguration_bumper' - The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
--
-- 'cdnConfiguration', 'putPlaybackConfiguration_cdnConfiguration' - The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
--
-- 'configurationAliases', 'putPlaybackConfiguration_configurationAliases' - The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
--
-- 'dashConfiguration', 'putPlaybackConfiguration_dashConfiguration' - The configuration for DASH content.
--
-- 'livePreRollConfiguration', 'putPlaybackConfiguration_livePreRollConfiguration' - The configuration for pre-roll ad insertion.
--
-- 'manifestProcessingRules', 'putPlaybackConfiguration_manifestProcessingRules' - The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- 'personalizationThresholdSeconds', 'putPlaybackConfiguration_personalizationThresholdSeconds' - Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
--
-- 'slateAdUrl', 'putPlaybackConfiguration_slateAdUrl' - The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
--
-- 'tags', 'putPlaybackConfiguration_tags' - The tags to assign to the playback configuration. Tags are key-value
-- pairs that you can associate with Amazon resources to help with
-- organization, access control, and cost tracking. For more information,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'transcodeProfileName', 'putPlaybackConfiguration_transcodeProfileName' - The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
--
-- 'videoContentSourceUrl', 'putPlaybackConfiguration_videoContentSourceUrl' - The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
--
-- 'name', 'putPlaybackConfiguration_name' - The identifier for the playback configuration.
newPutPlaybackConfiguration ::
  -- | 'name'
  Prelude.Text ->
  PutPlaybackConfiguration
newPutPlaybackConfiguration pName_ =
  PutPlaybackConfiguration'
    { adDecisionServerUrl =
        Prelude.Nothing,
      availSuppression = Prelude.Nothing,
      bumper = Prelude.Nothing,
      cdnConfiguration = Prelude.Nothing,
      configurationAliases = Prelude.Nothing,
      dashConfiguration = Prelude.Nothing,
      livePreRollConfiguration = Prelude.Nothing,
      manifestProcessingRules = Prelude.Nothing,
      personalizationThresholdSeconds = Prelude.Nothing,
      slateAdUrl = Prelude.Nothing,
      tags = Prelude.Nothing,
      transcodeProfileName = Prelude.Nothing,
      videoContentSourceUrl = Prelude.Nothing,
      name = pName_
    }

-- | The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
putPlaybackConfiguration_adDecisionServerUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_adDecisionServerUrl = Lens.lens (\PutPlaybackConfiguration' {adDecisionServerUrl} -> adDecisionServerUrl) (\s@PutPlaybackConfiguration' {} a -> s {adDecisionServerUrl = a} :: PutPlaybackConfiguration)

-- | The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
putPlaybackConfiguration_availSuppression :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe AvailSuppression)
putPlaybackConfiguration_availSuppression = Lens.lens (\PutPlaybackConfiguration' {availSuppression} -> availSuppression) (\s@PutPlaybackConfiguration' {} a -> s {availSuppression = a} :: PutPlaybackConfiguration)

-- | The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
putPlaybackConfiguration_bumper :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Bumper)
putPlaybackConfiguration_bumper = Lens.lens (\PutPlaybackConfiguration' {bumper} -> bumper) (\s@PutPlaybackConfiguration' {} a -> s {bumper = a} :: PutPlaybackConfiguration)

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
putPlaybackConfiguration_cdnConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe CdnConfiguration)
putPlaybackConfiguration_cdnConfiguration = Lens.lens (\PutPlaybackConfiguration' {cdnConfiguration} -> cdnConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {cdnConfiguration = a} :: PutPlaybackConfiguration)

-- | The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
putPlaybackConfiguration_configurationAliases :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
putPlaybackConfiguration_configurationAliases = Lens.lens (\PutPlaybackConfiguration' {configurationAliases} -> configurationAliases) (\s@PutPlaybackConfiguration' {} a -> s {configurationAliases = a} :: PutPlaybackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for DASH content.
putPlaybackConfiguration_dashConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe DashConfigurationForPut)
putPlaybackConfiguration_dashConfiguration = Lens.lens (\PutPlaybackConfiguration' {dashConfiguration} -> dashConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {dashConfiguration = a} :: PutPlaybackConfiguration)

-- | The configuration for pre-roll ad insertion.
putPlaybackConfiguration_livePreRollConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe LivePreRollConfiguration)
putPlaybackConfiguration_livePreRollConfiguration = Lens.lens (\PutPlaybackConfiguration' {livePreRollConfiguration} -> livePreRollConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {livePreRollConfiguration = a} :: PutPlaybackConfiguration)

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
putPlaybackConfiguration_manifestProcessingRules :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe ManifestProcessingRules)
putPlaybackConfiguration_manifestProcessingRules = Lens.lens (\PutPlaybackConfiguration' {manifestProcessingRules} -> manifestProcessingRules) (\s@PutPlaybackConfiguration' {} a -> s {manifestProcessingRules = a} :: PutPlaybackConfiguration)

-- | Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
putPlaybackConfiguration_personalizationThresholdSeconds :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Natural)
putPlaybackConfiguration_personalizationThresholdSeconds = Lens.lens (\PutPlaybackConfiguration' {personalizationThresholdSeconds} -> personalizationThresholdSeconds) (\s@PutPlaybackConfiguration' {} a -> s {personalizationThresholdSeconds = a} :: PutPlaybackConfiguration)

-- | The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
putPlaybackConfiguration_slateAdUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_slateAdUrl = Lens.lens (\PutPlaybackConfiguration' {slateAdUrl} -> slateAdUrl) (\s@PutPlaybackConfiguration' {} a -> s {slateAdUrl = a} :: PutPlaybackConfiguration)

-- | The tags to assign to the playback configuration. Tags are key-value
-- pairs that you can associate with Amazon resources to help with
-- organization, access control, and cost tracking. For more information,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
putPlaybackConfiguration_tags :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putPlaybackConfiguration_tags = Lens.lens (\PutPlaybackConfiguration' {tags} -> tags) (\s@PutPlaybackConfiguration' {} a -> s {tags = a} :: PutPlaybackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
putPlaybackConfiguration_transcodeProfileName :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_transcodeProfileName = Lens.lens (\PutPlaybackConfiguration' {transcodeProfileName} -> transcodeProfileName) (\s@PutPlaybackConfiguration' {} a -> s {transcodeProfileName = a} :: PutPlaybackConfiguration)

-- | The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
putPlaybackConfiguration_videoContentSourceUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_videoContentSourceUrl = Lens.lens (\PutPlaybackConfiguration' {videoContentSourceUrl} -> videoContentSourceUrl) (\s@PutPlaybackConfiguration' {} a -> s {videoContentSourceUrl = a} :: PutPlaybackConfiguration)

-- | The identifier for the playback configuration.
putPlaybackConfiguration_name :: Lens.Lens' PutPlaybackConfiguration Prelude.Text
putPlaybackConfiguration_name = Lens.lens (\PutPlaybackConfiguration' {name} -> name) (\s@PutPlaybackConfiguration' {} a -> s {name = a} :: PutPlaybackConfiguration)

instance Core.AWSRequest PutPlaybackConfiguration where
  type
    AWSResponse PutPlaybackConfiguration =
      PutPlaybackConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPlaybackConfigurationResponse'
            Prelude.<$> (x Data..?> "AdDecisionServerUrl")
            Prelude.<*> (x Data..?> "AvailSuppression")
            Prelude.<*> (x Data..?> "Bumper")
            Prelude.<*> (x Data..?> "CdnConfiguration")
            Prelude.<*> ( x
                            Data..?> "ConfigurationAliases"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "DashConfiguration")
            Prelude.<*> (x Data..?> "HlsConfiguration")
            Prelude.<*> (x Data..?> "LivePreRollConfiguration")
            Prelude.<*> (x Data..?> "LogConfiguration")
            Prelude.<*> (x Data..?> "ManifestProcessingRules")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PersonalizationThresholdSeconds")
            Prelude.<*> (x Data..?> "PlaybackConfigurationArn")
            Prelude.<*> (x Data..?> "PlaybackEndpointPrefix")
            Prelude.<*> (x Data..?> "SessionInitializationEndpointPrefix")
            Prelude.<*> (x Data..?> "SlateAdUrl")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TranscodeProfileName")
            Prelude.<*> (x Data..?> "VideoContentSourceUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPlaybackConfiguration where
  hashWithSalt _salt PutPlaybackConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` adDecisionServerUrl
      `Prelude.hashWithSalt` availSuppression
      `Prelude.hashWithSalt` bumper
      `Prelude.hashWithSalt` cdnConfiguration
      `Prelude.hashWithSalt` configurationAliases
      `Prelude.hashWithSalt` dashConfiguration
      `Prelude.hashWithSalt` livePreRollConfiguration
      `Prelude.hashWithSalt` manifestProcessingRules
      `Prelude.hashWithSalt` personalizationThresholdSeconds
      `Prelude.hashWithSalt` slateAdUrl
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` transcodeProfileName
      `Prelude.hashWithSalt` videoContentSourceUrl
      `Prelude.hashWithSalt` name

instance Prelude.NFData PutPlaybackConfiguration where
  rnf PutPlaybackConfiguration' {..} =
    Prelude.rnf adDecisionServerUrl `Prelude.seq`
      Prelude.rnf availSuppression `Prelude.seq`
        Prelude.rnf bumper `Prelude.seq`
          Prelude.rnf cdnConfiguration `Prelude.seq`
            Prelude.rnf configurationAliases `Prelude.seq`
              Prelude.rnf dashConfiguration `Prelude.seq`
                Prelude.rnf livePreRollConfiguration `Prelude.seq`
                  Prelude.rnf manifestProcessingRules `Prelude.seq`
                    Prelude.rnf personalizationThresholdSeconds `Prelude.seq`
                      Prelude.rnf slateAdUrl `Prelude.seq`
                        Prelude.rnf tags `Prelude.seq`
                          Prelude.rnf transcodeProfileName `Prelude.seq`
                            Prelude.rnf videoContentSourceUrl `Prelude.seq`
                              Prelude.rnf name

instance Data.ToHeaders PutPlaybackConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPlaybackConfiguration where
  toJSON PutPlaybackConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdDecisionServerUrl" Data..=)
              Prelude.<$> adDecisionServerUrl,
            ("AvailSuppression" Data..=)
              Prelude.<$> availSuppression,
            ("Bumper" Data..=) Prelude.<$> bumper,
            ("CdnConfiguration" Data..=)
              Prelude.<$> cdnConfiguration,
            ("ConfigurationAliases" Data..=)
              Prelude.<$> configurationAliases,
            ("DashConfiguration" Data..=)
              Prelude.<$> dashConfiguration,
            ("LivePreRollConfiguration" Data..=)
              Prelude.<$> livePreRollConfiguration,
            ("ManifestProcessingRules" Data..=)
              Prelude.<$> manifestProcessingRules,
            ("PersonalizationThresholdSeconds" Data..=)
              Prelude.<$> personalizationThresholdSeconds,
            ("SlateAdUrl" Data..=) Prelude.<$> slateAdUrl,
            ("tags" Data..=) Prelude.<$> tags,
            ("TranscodeProfileName" Data..=)
              Prelude.<$> transcodeProfileName,
            ("VideoContentSourceUrl" Data..=)
              Prelude.<$> videoContentSourceUrl,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath PutPlaybackConfiguration where
  toPath = Prelude.const "/playbackConfiguration"

instance Data.ToQuery PutPlaybackConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPlaybackConfigurationResponse' smart constructor.
data PutPlaybackConfigurationResponse = PutPlaybackConfigurationResponse'
  { -- | The URL for the ad decision server (ADS). This includes the
    -- specification of static parameters and placeholders for dynamic
    -- parameters. AWS Elemental MediaTailor substitutes player-specific and
    -- session-specific parameters as needed when calling the ADS. Alternately,
    -- for testing you can provide a static VAST URL. The maximum length is
    -- 25,000 characters.
    adDecisionServerUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for avail suppression, also known as ad suppression.
    -- For more information about ad suppression, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
    availSuppression :: Prelude.Maybe AvailSuppression,
    -- | The configuration for bumpers. Bumpers are short audio or video clips
    -- that play at the start or before the end of an ad break. To learn more
    -- about bumpers, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
    bumper :: Prelude.Maybe Bumper,
    -- | The configuration for using a content delivery network (CDN), like
    -- Amazon CloudFront, for content and ad segment management.
    cdnConfiguration :: Prelude.Maybe CdnConfiguration,
    -- | The player parameters and aliases used as dynamic variables during
    -- session initialization. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
    configurationAliases :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The configuration for DASH content.
    dashConfiguration :: Prelude.Maybe DashConfiguration,
    -- | The configuration for HLS content.
    hlsConfiguration :: Prelude.Maybe HlsConfiguration,
    -- | The configuration for pre-roll ad insertion.
    livePreRollConfiguration :: Prelude.Maybe LivePreRollConfiguration,
    -- | The Amazon CloudWatch log settings for a playback configuration.
    logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | The configuration for manifest processing rules. Manifest processing
    -- rules enable customization of the personalized manifests created by
    -- MediaTailor.
    manifestProcessingRules :: Prelude.Maybe ManifestProcessingRules,
    -- | The identifier for the playback configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Defines the maximum duration of underfilled ad time (in seconds) allowed
    -- in an ad break. If the duration of underfilled ad time exceeds the
    -- personalization threshold, then the personalization of the ad break is
    -- abandoned and the underlying content is shown. This feature applies to
    -- /ad replacement/ in live and VOD streams, rather than ad insertion,
    -- because it relies on an underlying content stream. For more information
    -- about ad break behavior, including ad replacement and insertion, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
    personalizationThresholdSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) associated with the playback
    -- configuration.
    playbackConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The playback endpoint prefix associated with the playback configuration.
    playbackEndpointPrefix :: Prelude.Maybe Prelude.Text,
    -- | The session initialization endpoint prefix associated with the playback
    -- configuration.
    sessionInitializationEndpointPrefix :: Prelude.Maybe Prelude.Text,
    -- | The URL for a high-quality video asset to transcode and use to fill in
    -- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
    -- to fill in gaps in media content. Configuring the slate is optional for
    -- non-VPAID configurations. For VPAID, the slate is required because
    -- MediaTailor provides it in the slots that are designated for dynamic ad
    -- content. The slate must be a high-quality asset that contains both audio
    -- and video.
    slateAdUrl :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the playback configuration. Tags are key-value
    -- pairs that you can associate with Amazon resources to help with
    -- organization, access control, and cost tracking. For more information,
    -- see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name that is used to associate this playback configuration with a
    -- custom transcode profile. This overrides the dynamic transcoding
    -- defaults of MediaTailor. Use this only if you have already set up custom
    -- profiles with the help of AWS Support.
    transcodeProfileName :: Prelude.Maybe Prelude.Text,
    -- | The URL prefix for the parent manifest for the stream, minus the asset
    -- ID. The maximum length is 512 characters.
    videoContentSourceUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPlaybackConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adDecisionServerUrl', 'putPlaybackConfigurationResponse_adDecisionServerUrl' - The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
--
-- 'availSuppression', 'putPlaybackConfigurationResponse_availSuppression' - The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
--
-- 'bumper', 'putPlaybackConfigurationResponse_bumper' - The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
--
-- 'cdnConfiguration', 'putPlaybackConfigurationResponse_cdnConfiguration' - The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
--
-- 'configurationAliases', 'putPlaybackConfigurationResponse_configurationAliases' - The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
--
-- 'dashConfiguration', 'putPlaybackConfigurationResponse_dashConfiguration' - The configuration for DASH content.
--
-- 'hlsConfiguration', 'putPlaybackConfigurationResponse_hlsConfiguration' - The configuration for HLS content.
--
-- 'livePreRollConfiguration', 'putPlaybackConfigurationResponse_livePreRollConfiguration' - The configuration for pre-roll ad insertion.
--
-- 'logConfiguration', 'putPlaybackConfigurationResponse_logConfiguration' - The Amazon CloudWatch log settings for a playback configuration.
--
-- 'manifestProcessingRules', 'putPlaybackConfigurationResponse_manifestProcessingRules' - The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- 'name', 'putPlaybackConfigurationResponse_name' - The identifier for the playback configuration.
--
-- 'personalizationThresholdSeconds', 'putPlaybackConfigurationResponse_personalizationThresholdSeconds' - Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
--
-- 'playbackConfigurationArn', 'putPlaybackConfigurationResponse_playbackConfigurationArn' - The Amazon Resource Name (ARN) associated with the playback
-- configuration.
--
-- 'playbackEndpointPrefix', 'putPlaybackConfigurationResponse_playbackEndpointPrefix' - The playback endpoint prefix associated with the playback configuration.
--
-- 'sessionInitializationEndpointPrefix', 'putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix' - The session initialization endpoint prefix associated with the playback
-- configuration.
--
-- 'slateAdUrl', 'putPlaybackConfigurationResponse_slateAdUrl' - The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
--
-- 'tags', 'putPlaybackConfigurationResponse_tags' - The tags to assign to the playback configuration. Tags are key-value
-- pairs that you can associate with Amazon resources to help with
-- organization, access control, and cost tracking. For more information,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'transcodeProfileName', 'putPlaybackConfigurationResponse_transcodeProfileName' - The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
--
-- 'videoContentSourceUrl', 'putPlaybackConfigurationResponse_videoContentSourceUrl' - The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
--
-- 'httpStatus', 'putPlaybackConfigurationResponse_httpStatus' - The response's http status code.
newPutPlaybackConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPlaybackConfigurationResponse
newPutPlaybackConfigurationResponse pHttpStatus_ =
  PutPlaybackConfigurationResponse'
    { adDecisionServerUrl =
        Prelude.Nothing,
      availSuppression = Prelude.Nothing,
      bumper = Prelude.Nothing,
      cdnConfiguration = Prelude.Nothing,
      configurationAliases = Prelude.Nothing,
      dashConfiguration = Prelude.Nothing,
      hlsConfiguration = Prelude.Nothing,
      livePreRollConfiguration =
        Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      manifestProcessingRules = Prelude.Nothing,
      name = Prelude.Nothing,
      personalizationThresholdSeconds =
        Prelude.Nothing,
      playbackConfigurationArn =
        Prelude.Nothing,
      playbackEndpointPrefix = Prelude.Nothing,
      sessionInitializationEndpointPrefix =
        Prelude.Nothing,
      slateAdUrl = Prelude.Nothing,
      tags = Prelude.Nothing,
      transcodeProfileName = Prelude.Nothing,
      videoContentSourceUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
putPlaybackConfigurationResponse_adDecisionServerUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_adDecisionServerUrl = Lens.lens (\PutPlaybackConfigurationResponse' {adDecisionServerUrl} -> adDecisionServerUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {adDecisionServerUrl = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
putPlaybackConfigurationResponse_availSuppression :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe AvailSuppression)
putPlaybackConfigurationResponse_availSuppression = Lens.lens (\PutPlaybackConfigurationResponse' {availSuppression} -> availSuppression) (\s@PutPlaybackConfigurationResponse' {} a -> s {availSuppression = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
putPlaybackConfigurationResponse_bumper :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Bumper)
putPlaybackConfigurationResponse_bumper = Lens.lens (\PutPlaybackConfigurationResponse' {bumper} -> bumper) (\s@PutPlaybackConfigurationResponse' {} a -> s {bumper = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
putPlaybackConfigurationResponse_cdnConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe CdnConfiguration)
putPlaybackConfigurationResponse_cdnConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {cdnConfiguration} -> cdnConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {cdnConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
putPlaybackConfigurationResponse_configurationAliases :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
putPlaybackConfigurationResponse_configurationAliases = Lens.lens (\PutPlaybackConfigurationResponse' {configurationAliases} -> configurationAliases) (\s@PutPlaybackConfigurationResponse' {} a -> s {configurationAliases = a} :: PutPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for DASH content.
putPlaybackConfigurationResponse_dashConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe DashConfiguration)
putPlaybackConfigurationResponse_dashConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {dashConfiguration} -> dashConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {dashConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for HLS content.
putPlaybackConfigurationResponse_hlsConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe HlsConfiguration)
putPlaybackConfigurationResponse_hlsConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {hlsConfiguration} -> hlsConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {hlsConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for pre-roll ad insertion.
putPlaybackConfigurationResponse_livePreRollConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe LivePreRollConfiguration)
putPlaybackConfigurationResponse_livePreRollConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {livePreRollConfiguration} -> livePreRollConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {livePreRollConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The Amazon CloudWatch log settings for a playback configuration.
putPlaybackConfigurationResponse_logConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe LogConfiguration)
putPlaybackConfigurationResponse_logConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {logConfiguration} -> logConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {logConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
putPlaybackConfigurationResponse_manifestProcessingRules :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe ManifestProcessingRules)
putPlaybackConfigurationResponse_manifestProcessingRules = Lens.lens (\PutPlaybackConfigurationResponse' {manifestProcessingRules} -> manifestProcessingRules) (\s@PutPlaybackConfigurationResponse' {} a -> s {manifestProcessingRules = a} :: PutPlaybackConfigurationResponse)

-- | The identifier for the playback configuration.
putPlaybackConfigurationResponse_name :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_name = Lens.lens (\PutPlaybackConfigurationResponse' {name} -> name) (\s@PutPlaybackConfigurationResponse' {} a -> s {name = a} :: PutPlaybackConfigurationResponse)

-- | Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
putPlaybackConfigurationResponse_personalizationThresholdSeconds :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Natural)
putPlaybackConfigurationResponse_personalizationThresholdSeconds = Lens.lens (\PutPlaybackConfigurationResponse' {personalizationThresholdSeconds} -> personalizationThresholdSeconds) (\s@PutPlaybackConfigurationResponse' {} a -> s {personalizationThresholdSeconds = a} :: PutPlaybackConfigurationResponse)

-- | The Amazon Resource Name (ARN) associated with the playback
-- configuration.
putPlaybackConfigurationResponse_playbackConfigurationArn :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_playbackConfigurationArn = Lens.lens (\PutPlaybackConfigurationResponse' {playbackConfigurationArn} -> playbackConfigurationArn) (\s@PutPlaybackConfigurationResponse' {} a -> s {playbackConfigurationArn = a} :: PutPlaybackConfigurationResponse)

-- | The playback endpoint prefix associated with the playback configuration.
putPlaybackConfigurationResponse_playbackEndpointPrefix :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_playbackEndpointPrefix = Lens.lens (\PutPlaybackConfigurationResponse' {playbackEndpointPrefix} -> playbackEndpointPrefix) (\s@PutPlaybackConfigurationResponse' {} a -> s {playbackEndpointPrefix = a} :: PutPlaybackConfigurationResponse)

-- | The session initialization endpoint prefix associated with the playback
-- configuration.
putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix = Lens.lens (\PutPlaybackConfigurationResponse' {sessionInitializationEndpointPrefix} -> sessionInitializationEndpointPrefix) (\s@PutPlaybackConfigurationResponse' {} a -> s {sessionInitializationEndpointPrefix = a} :: PutPlaybackConfigurationResponse)

-- | The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
putPlaybackConfigurationResponse_slateAdUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_slateAdUrl = Lens.lens (\PutPlaybackConfigurationResponse' {slateAdUrl} -> slateAdUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {slateAdUrl = a} :: PutPlaybackConfigurationResponse)

-- | The tags to assign to the playback configuration. Tags are key-value
-- pairs that you can associate with Amazon resources to help with
-- organization, access control, and cost tracking. For more information,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
putPlaybackConfigurationResponse_tags :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putPlaybackConfigurationResponse_tags = Lens.lens (\PutPlaybackConfigurationResponse' {tags} -> tags) (\s@PutPlaybackConfigurationResponse' {} a -> s {tags = a} :: PutPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
putPlaybackConfigurationResponse_transcodeProfileName :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_transcodeProfileName = Lens.lens (\PutPlaybackConfigurationResponse' {transcodeProfileName} -> transcodeProfileName) (\s@PutPlaybackConfigurationResponse' {} a -> s {transcodeProfileName = a} :: PutPlaybackConfigurationResponse)

-- | The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
putPlaybackConfigurationResponse_videoContentSourceUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_videoContentSourceUrl = Lens.lens (\PutPlaybackConfigurationResponse' {videoContentSourceUrl} -> videoContentSourceUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {videoContentSourceUrl = a} :: PutPlaybackConfigurationResponse)

-- | The response's http status code.
putPlaybackConfigurationResponse_httpStatus :: Lens.Lens' PutPlaybackConfigurationResponse Prelude.Int
putPlaybackConfigurationResponse_httpStatus = Lens.lens (\PutPlaybackConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutPlaybackConfigurationResponse' {} a -> s {httpStatus = a} :: PutPlaybackConfigurationResponse)

instance
  Prelude.NFData
    PutPlaybackConfigurationResponse
  where
  rnf PutPlaybackConfigurationResponse' {..} =
    Prelude.rnf adDecisionServerUrl
      `Prelude.seq` Prelude.rnf availSuppression
      `Prelude.seq` Prelude.rnf bumper
      `Prelude.seq` Prelude.rnf cdnConfiguration
      `Prelude.seq` Prelude.rnf configurationAliases
      `Prelude.seq` Prelude.rnf dashConfiguration
      `Prelude.seq` Prelude.rnf hlsConfiguration
      `Prelude.seq` Prelude.rnf livePreRollConfiguration
      `Prelude.seq` Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf manifestProcessingRules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf personalizationThresholdSeconds
      `Prelude.seq` Prelude.rnf playbackConfigurationArn
      `Prelude.seq` Prelude.rnf playbackEndpointPrefix
      `Prelude.seq` Prelude.rnf
        sessionInitializationEndpointPrefix
      `Prelude.seq` Prelude.rnf slateAdUrl
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transcodeProfileName
      `Prelude.seq` Prelude.rnf
        videoContentSourceUrl
      `Prelude.seq` Prelude.rnf httpStatus
