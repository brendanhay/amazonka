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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new playback configuration to AWS Elemental MediaTailor.
module Amazonka.MediaTailor.PutPlaybackConfiguration
  ( -- * Creating a Request
    PutPlaybackConfiguration (..),
    newPutPlaybackConfiguration,

    -- * Request Lenses
    putPlaybackConfiguration_personalizationThresholdSeconds,
    putPlaybackConfiguration_availSuppression,
    putPlaybackConfiguration_bumper,
    putPlaybackConfiguration_adDecisionServerUrl,
    putPlaybackConfiguration_videoContentSourceUrl,
    putPlaybackConfiguration_dashConfiguration,
    putPlaybackConfiguration_manifestProcessingRules,
    putPlaybackConfiguration_livePreRollConfiguration,
    putPlaybackConfiguration_name,
    putPlaybackConfiguration_configurationAliases,
    putPlaybackConfiguration_transcodeProfileName,
    putPlaybackConfiguration_tags,
    putPlaybackConfiguration_slateAdUrl,
    putPlaybackConfiguration_cdnConfiguration,

    -- * Destructuring the Response
    PutPlaybackConfigurationResponse (..),
    newPutPlaybackConfigurationResponse,

    -- * Response Lenses
    putPlaybackConfigurationResponse_playbackEndpointPrefix,
    putPlaybackConfigurationResponse_hlsConfiguration,
    putPlaybackConfigurationResponse_personalizationThresholdSeconds,
    putPlaybackConfigurationResponse_playbackConfigurationArn,
    putPlaybackConfigurationResponse_availSuppression,
    putPlaybackConfigurationResponse_bumper,
    putPlaybackConfigurationResponse_adDecisionServerUrl,
    putPlaybackConfigurationResponse_videoContentSourceUrl,
    putPlaybackConfigurationResponse_dashConfiguration,
    putPlaybackConfigurationResponse_manifestProcessingRules,
    putPlaybackConfigurationResponse_logConfiguration,
    putPlaybackConfigurationResponse_livePreRollConfiguration,
    putPlaybackConfigurationResponse_name,
    putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    putPlaybackConfigurationResponse_configurationAliases,
    putPlaybackConfigurationResponse_transcodeProfileName,
    putPlaybackConfigurationResponse_tags,
    putPlaybackConfigurationResponse_slateAdUrl,
    putPlaybackConfigurationResponse_cdnConfiguration,
    putPlaybackConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPlaybackConfiguration' smart constructor.
data PutPlaybackConfiguration = PutPlaybackConfiguration'
  { -- | Defines the maximum duration of underfilled ad time (in seconds) allowed
    -- in an ad break. If the duration of underfilled ad time exceeds the
    -- personalization threshold, then the personalization of the ad break is
    -- abandoned and the underlying content is shown. This feature applies to
    -- /ad replacement/ in live and VOD streams, rather than ad insertion,
    -- because it relies on an underlying content stream. For more information
    -- about ad break behavior, including ad replacement and insertion, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
    personalizationThresholdSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The configuration for avail suppression, also known as ad suppression.
    -- For more information about ad suppression, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
    availSuppression :: Prelude.Maybe AvailSuppression,
    -- | The configuration for bumpers. Bumpers are short audio or video clips
    -- that play at the start or before the end of an ad break. To learn more
    -- about bumpers, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
    bumper :: Prelude.Maybe Bumper,
    -- | The URL for the ad decision server (ADS). This includes the
    -- specification of static parameters and placeholders for dynamic
    -- parameters. AWS Elemental MediaTailor substitutes player-specific and
    -- session-specific parameters as needed when calling the ADS. Alternately,
    -- for testing you can provide a static VAST URL. The maximum length is
    -- 25,000 characters.
    adDecisionServerUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL prefix for the parent manifest for the stream, minus the asset
    -- ID. The maximum length is 512 characters.
    videoContentSourceUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for DASH content.
    dashConfiguration :: Prelude.Maybe DashConfigurationForPut,
    -- | The configuration for manifest processing rules. Manifest processing
    -- rules enable customization of the personalized manifests created by
    -- MediaTailor.
    manifestProcessingRules :: Prelude.Maybe ManifestProcessingRules,
    -- | The configuration for pre-roll ad insertion.
    livePreRollConfiguration :: Prelude.Maybe LivePreRollConfiguration,
    -- | The identifier for the playback configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The player parameters and aliases used as dynamic variables during
    -- session initialization. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
    configurationAliases :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name that is used to associate this playback configuration with a
    -- custom transcode profile. This overrides the dynamic transcoding
    -- defaults of MediaTailor. Use this only if you have already set up custom
    -- profiles with the help of AWS Support.
    transcodeProfileName :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the playback configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URL for a high-quality video asset to transcode and use to fill in
    -- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
    -- to fill in gaps in media content. Configuring the slate is optional for
    -- non-VPAID configurations. For VPAID, the slate is required because
    -- MediaTailor provides it in the slots that are designated for dynamic ad
    -- content. The slate must be a high-quality asset that contains both audio
    -- and video.
    slateAdUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for using a content delivery network (CDN), like
    -- Amazon CloudFront, for content and ad segment management.
    cdnConfiguration :: Prelude.Maybe CdnConfiguration
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
-- 'personalizationThresholdSeconds', 'putPlaybackConfiguration_personalizationThresholdSeconds' - Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
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
-- 'adDecisionServerUrl', 'putPlaybackConfiguration_adDecisionServerUrl' - The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
--
-- 'videoContentSourceUrl', 'putPlaybackConfiguration_videoContentSourceUrl' - The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
--
-- 'dashConfiguration', 'putPlaybackConfiguration_dashConfiguration' - The configuration for DASH content.
--
-- 'manifestProcessingRules', 'putPlaybackConfiguration_manifestProcessingRules' - The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- 'livePreRollConfiguration', 'putPlaybackConfiguration_livePreRollConfiguration' - The configuration for pre-roll ad insertion.
--
-- 'name', 'putPlaybackConfiguration_name' - The identifier for the playback configuration.
--
-- 'configurationAliases', 'putPlaybackConfiguration_configurationAliases' - The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
--
-- 'transcodeProfileName', 'putPlaybackConfiguration_transcodeProfileName' - The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
--
-- 'tags', 'putPlaybackConfiguration_tags' - The tags to assign to the playback configuration.
--
-- 'slateAdUrl', 'putPlaybackConfiguration_slateAdUrl' - The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
--
-- 'cdnConfiguration', 'putPlaybackConfiguration_cdnConfiguration' - The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
newPutPlaybackConfiguration ::
  PutPlaybackConfiguration
newPutPlaybackConfiguration =
  PutPlaybackConfiguration'
    { personalizationThresholdSeconds =
        Prelude.Nothing,
      availSuppression = Prelude.Nothing,
      bumper = Prelude.Nothing,
      adDecisionServerUrl = Prelude.Nothing,
      videoContentSourceUrl = Prelude.Nothing,
      dashConfiguration = Prelude.Nothing,
      manifestProcessingRules = Prelude.Nothing,
      livePreRollConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      configurationAliases = Prelude.Nothing,
      transcodeProfileName = Prelude.Nothing,
      tags = Prelude.Nothing,
      slateAdUrl = Prelude.Nothing,
      cdnConfiguration = Prelude.Nothing
    }

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

-- | The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
putPlaybackConfiguration_adDecisionServerUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_adDecisionServerUrl = Lens.lens (\PutPlaybackConfiguration' {adDecisionServerUrl} -> adDecisionServerUrl) (\s@PutPlaybackConfiguration' {} a -> s {adDecisionServerUrl = a} :: PutPlaybackConfiguration)

-- | The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
putPlaybackConfiguration_videoContentSourceUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_videoContentSourceUrl = Lens.lens (\PutPlaybackConfiguration' {videoContentSourceUrl} -> videoContentSourceUrl) (\s@PutPlaybackConfiguration' {} a -> s {videoContentSourceUrl = a} :: PutPlaybackConfiguration)

-- | The configuration for DASH content.
putPlaybackConfiguration_dashConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe DashConfigurationForPut)
putPlaybackConfiguration_dashConfiguration = Lens.lens (\PutPlaybackConfiguration' {dashConfiguration} -> dashConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {dashConfiguration = a} :: PutPlaybackConfiguration)

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
putPlaybackConfiguration_manifestProcessingRules :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe ManifestProcessingRules)
putPlaybackConfiguration_manifestProcessingRules = Lens.lens (\PutPlaybackConfiguration' {manifestProcessingRules} -> manifestProcessingRules) (\s@PutPlaybackConfiguration' {} a -> s {manifestProcessingRules = a} :: PutPlaybackConfiguration)

-- | The configuration for pre-roll ad insertion.
putPlaybackConfiguration_livePreRollConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe LivePreRollConfiguration)
putPlaybackConfiguration_livePreRollConfiguration = Lens.lens (\PutPlaybackConfiguration' {livePreRollConfiguration} -> livePreRollConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {livePreRollConfiguration = a} :: PutPlaybackConfiguration)

-- | The identifier for the playback configuration.
putPlaybackConfiguration_name :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_name = Lens.lens (\PutPlaybackConfiguration' {name} -> name) (\s@PutPlaybackConfiguration' {} a -> s {name = a} :: PutPlaybackConfiguration)

-- | The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
putPlaybackConfiguration_configurationAliases :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
putPlaybackConfiguration_configurationAliases = Lens.lens (\PutPlaybackConfiguration' {configurationAliases} -> configurationAliases) (\s@PutPlaybackConfiguration' {} a -> s {configurationAliases = a} :: PutPlaybackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
putPlaybackConfiguration_transcodeProfileName :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_transcodeProfileName = Lens.lens (\PutPlaybackConfiguration' {transcodeProfileName} -> transcodeProfileName) (\s@PutPlaybackConfiguration' {} a -> s {transcodeProfileName = a} :: PutPlaybackConfiguration)

-- | The tags to assign to the playback configuration.
putPlaybackConfiguration_tags :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putPlaybackConfiguration_tags = Lens.lens (\PutPlaybackConfiguration' {tags} -> tags) (\s@PutPlaybackConfiguration' {} a -> s {tags = a} :: PutPlaybackConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID configurations. For VPAID, the slate is required because
-- MediaTailor provides it in the slots that are designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
putPlaybackConfiguration_slateAdUrl :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe Prelude.Text)
putPlaybackConfiguration_slateAdUrl = Lens.lens (\PutPlaybackConfiguration' {slateAdUrl} -> slateAdUrl) (\s@PutPlaybackConfiguration' {} a -> s {slateAdUrl = a} :: PutPlaybackConfiguration)

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
putPlaybackConfiguration_cdnConfiguration :: Lens.Lens' PutPlaybackConfiguration (Prelude.Maybe CdnConfiguration)
putPlaybackConfiguration_cdnConfiguration = Lens.lens (\PutPlaybackConfiguration' {cdnConfiguration} -> cdnConfiguration) (\s@PutPlaybackConfiguration' {} a -> s {cdnConfiguration = a} :: PutPlaybackConfiguration)

instance Core.AWSRequest PutPlaybackConfiguration where
  type
    AWSResponse PutPlaybackConfiguration =
      PutPlaybackConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPlaybackConfigurationResponse'
            Prelude.<$> (x Core..?> "PlaybackEndpointPrefix")
            Prelude.<*> (x Core..?> "HlsConfiguration")
            Prelude.<*> (x Core..?> "PersonalizationThresholdSeconds")
            Prelude.<*> (x Core..?> "PlaybackConfigurationArn")
            Prelude.<*> (x Core..?> "AvailSuppression")
            Prelude.<*> (x Core..?> "Bumper")
            Prelude.<*> (x Core..?> "AdDecisionServerUrl")
            Prelude.<*> (x Core..?> "VideoContentSourceUrl")
            Prelude.<*> (x Core..?> "DashConfiguration")
            Prelude.<*> (x Core..?> "ManifestProcessingRules")
            Prelude.<*> (x Core..?> "LogConfiguration")
            Prelude.<*> (x Core..?> "LivePreRollConfiguration")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "SessionInitializationEndpointPrefix")
            Prelude.<*> ( x Core..?> "ConfigurationAliases"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "TranscodeProfileName")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "SlateAdUrl")
            Prelude.<*> (x Core..?> "CdnConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPlaybackConfiguration

instance Prelude.NFData PutPlaybackConfiguration

instance Core.ToHeaders PutPlaybackConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutPlaybackConfiguration where
  toJSON PutPlaybackConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PersonalizationThresholdSeconds" Core..=)
              Prelude.<$> personalizationThresholdSeconds,
            ("AvailSuppression" Core..=)
              Prelude.<$> availSuppression,
            ("Bumper" Core..=) Prelude.<$> bumper,
            ("AdDecisionServerUrl" Core..=)
              Prelude.<$> adDecisionServerUrl,
            ("VideoContentSourceUrl" Core..=)
              Prelude.<$> videoContentSourceUrl,
            ("DashConfiguration" Core..=)
              Prelude.<$> dashConfiguration,
            ("ManifestProcessingRules" Core..=)
              Prelude.<$> manifestProcessingRules,
            ("LivePreRollConfiguration" Core..=)
              Prelude.<$> livePreRollConfiguration,
            ("Name" Core..=) Prelude.<$> name,
            ("ConfigurationAliases" Core..=)
              Prelude.<$> configurationAliases,
            ("TranscodeProfileName" Core..=)
              Prelude.<$> transcodeProfileName,
            ("tags" Core..=) Prelude.<$> tags,
            ("SlateAdUrl" Core..=) Prelude.<$> slateAdUrl,
            ("CdnConfiguration" Core..=)
              Prelude.<$> cdnConfiguration
          ]
      )

instance Core.ToPath PutPlaybackConfiguration where
  toPath = Prelude.const "/playbackConfiguration"

instance Core.ToQuery PutPlaybackConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPlaybackConfigurationResponse' smart constructor.
data PutPlaybackConfigurationResponse = PutPlaybackConfigurationResponse'
  { -- | The URL that the player accesses to get a manifest from AWS Elemental
    -- MediaTailor. This session will use server-side reporting.
    playbackEndpointPrefix :: Prelude.Maybe Prelude.Text,
    -- | The configuration for HLS content.
    hlsConfiguration :: Prelude.Maybe HlsConfiguration,
    -- | Defines the maximum duration of underfilled ad time (in seconds) allowed
    -- in an ad break. If the duration of underfilled ad time exceeds the
    -- personalization threshold, then the personalization of the ad break is
    -- abandoned and the underlying content is shown. This feature applies to
    -- /ad replacement/ in live and VOD streams, rather than ad insertion,
    -- because it relies on an underlying content stream. For more information
    -- about ad break behavior, including ad replacement and insertion, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
    personalizationThresholdSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the playback configuration.
    playbackConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration for avail suppression, also known as ad suppression.
    -- For more information about ad suppression, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
    availSuppression :: Prelude.Maybe AvailSuppression,
    -- | The configuration for bumpers. Bumpers are short audio or video clips
    -- that play at the start or before the end of an ad break. To learn more
    -- about bumpers, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
    bumper :: Prelude.Maybe Bumper,
    -- | The URL for the ad decision server (ADS). This includes the
    -- specification of static parameters and placeholders for dynamic
    -- parameters. AWS Elemental MediaTailor substitutes player-specific and
    -- session-specific parameters as needed when calling the ADS. Alternately,
    -- for testing, you can provide a static VAST URL. The maximum length is
    -- 25,000 characters.
    adDecisionServerUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL prefix for the parent manifest for the stream, minus the asset
    -- ID. The maximum length is 512 characters.
    videoContentSourceUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for DASH content.
    dashConfiguration :: Prelude.Maybe DashConfiguration,
    -- | The configuration for manifest processing rules. Manifest processing
    -- rules enable customization of the personalized manifests created by
    -- MediaTailor.
    manifestProcessingRules :: Prelude.Maybe ManifestProcessingRules,
    -- | The Amazon CloudWatch log settings for a playback configuration.
    logConfiguration :: Prelude.Maybe LogConfiguration,
    -- | The configuration for pre-roll ad insertion.
    livePreRollConfiguration :: Prelude.Maybe LivePreRollConfiguration,
    -- | The identifier for the playback configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL that the player uses to initialize a session that uses
    -- client-side reporting.
    sessionInitializationEndpointPrefix :: Prelude.Maybe Prelude.Text,
    -- | The player parameters and aliases used as dynamic variables during
    -- session initialization. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
    configurationAliases :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name that is used to associate this playback configuration with a
    -- custom transcode profile. This overrides the dynamic transcoding
    -- defaults of MediaTailor. Use this only if you have already set up custom
    -- profiles with the help of AWS Support.
    transcodeProfileName :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the playback configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URL for a high-quality video asset to transcode and use to fill in
    -- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
    -- to fill in gaps in media content. Configuring the slate is optional for
    -- non-VPAID playback configurations. For VPAID, the slate is required
    -- because MediaTailor provides it in the slots designated for dynamic ad
    -- content. The slate must be a high-quality asset that contains both audio
    -- and video.
    slateAdUrl :: Prelude.Maybe Prelude.Text,
    -- | The configuration for using a content delivery network (CDN), like
    -- Amazon CloudFront, for content and ad segment management.
    cdnConfiguration :: Prelude.Maybe CdnConfiguration,
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
-- 'playbackEndpointPrefix', 'putPlaybackConfigurationResponse_playbackEndpointPrefix' - The URL that the player accesses to get a manifest from AWS Elemental
-- MediaTailor. This session will use server-side reporting.
--
-- 'hlsConfiguration', 'putPlaybackConfigurationResponse_hlsConfiguration' - The configuration for HLS content.
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
-- 'playbackConfigurationArn', 'putPlaybackConfigurationResponse_playbackConfigurationArn' - The Amazon Resource Name (ARN) for the playback configuration.
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
-- 'adDecisionServerUrl', 'putPlaybackConfigurationResponse_adDecisionServerUrl' - The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing, you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
--
-- 'videoContentSourceUrl', 'putPlaybackConfigurationResponse_videoContentSourceUrl' - The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
--
-- 'dashConfiguration', 'putPlaybackConfigurationResponse_dashConfiguration' - The configuration for DASH content.
--
-- 'manifestProcessingRules', 'putPlaybackConfigurationResponse_manifestProcessingRules' - The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- 'logConfiguration', 'putPlaybackConfigurationResponse_logConfiguration' - The Amazon CloudWatch log settings for a playback configuration.
--
-- 'livePreRollConfiguration', 'putPlaybackConfigurationResponse_livePreRollConfiguration' - The configuration for pre-roll ad insertion.
--
-- 'name', 'putPlaybackConfigurationResponse_name' - The identifier for the playback configuration.
--
-- 'sessionInitializationEndpointPrefix', 'putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix' - The URL that the player uses to initialize a session that uses
-- client-side reporting.
--
-- 'configurationAliases', 'putPlaybackConfigurationResponse_configurationAliases' - The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
--
-- 'transcodeProfileName', 'putPlaybackConfigurationResponse_transcodeProfileName' - The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
--
-- 'tags', 'putPlaybackConfigurationResponse_tags' - The tags assigned to the playback configuration.
--
-- 'slateAdUrl', 'putPlaybackConfigurationResponse_slateAdUrl' - The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID playback configurations. For VPAID, the slate is required
-- because MediaTailor provides it in the slots designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
--
-- 'cdnConfiguration', 'putPlaybackConfigurationResponse_cdnConfiguration' - The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
--
-- 'httpStatus', 'putPlaybackConfigurationResponse_httpStatus' - The response's http status code.
newPutPlaybackConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPlaybackConfigurationResponse
newPutPlaybackConfigurationResponse pHttpStatus_ =
  PutPlaybackConfigurationResponse'
    { playbackEndpointPrefix =
        Prelude.Nothing,
      hlsConfiguration = Prelude.Nothing,
      personalizationThresholdSeconds =
        Prelude.Nothing,
      playbackConfigurationArn =
        Prelude.Nothing,
      availSuppression = Prelude.Nothing,
      bumper = Prelude.Nothing,
      adDecisionServerUrl = Prelude.Nothing,
      videoContentSourceUrl = Prelude.Nothing,
      dashConfiguration = Prelude.Nothing,
      manifestProcessingRules = Prelude.Nothing,
      logConfiguration = Prelude.Nothing,
      livePreRollConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      sessionInitializationEndpointPrefix =
        Prelude.Nothing,
      configurationAliases = Prelude.Nothing,
      transcodeProfileName = Prelude.Nothing,
      tags = Prelude.Nothing,
      slateAdUrl = Prelude.Nothing,
      cdnConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL that the player accesses to get a manifest from AWS Elemental
-- MediaTailor. This session will use server-side reporting.
putPlaybackConfigurationResponse_playbackEndpointPrefix :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_playbackEndpointPrefix = Lens.lens (\PutPlaybackConfigurationResponse' {playbackEndpointPrefix} -> playbackEndpointPrefix) (\s@PutPlaybackConfigurationResponse' {} a -> s {playbackEndpointPrefix = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for HLS content.
putPlaybackConfigurationResponse_hlsConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe HlsConfiguration)
putPlaybackConfigurationResponse_hlsConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {hlsConfiguration} -> hlsConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {hlsConfiguration = a} :: PutPlaybackConfigurationResponse)

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

-- | The Amazon Resource Name (ARN) for the playback configuration.
putPlaybackConfigurationResponse_playbackConfigurationArn :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_playbackConfigurationArn = Lens.lens (\PutPlaybackConfigurationResponse' {playbackConfigurationArn} -> playbackConfigurationArn) (\s@PutPlaybackConfigurationResponse' {} a -> s {playbackConfigurationArn = a} :: PutPlaybackConfigurationResponse)

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

-- | The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing, you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
putPlaybackConfigurationResponse_adDecisionServerUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_adDecisionServerUrl = Lens.lens (\PutPlaybackConfigurationResponse' {adDecisionServerUrl} -> adDecisionServerUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {adDecisionServerUrl = a} :: PutPlaybackConfigurationResponse)

-- | The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
putPlaybackConfigurationResponse_videoContentSourceUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_videoContentSourceUrl = Lens.lens (\PutPlaybackConfigurationResponse' {videoContentSourceUrl} -> videoContentSourceUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {videoContentSourceUrl = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for DASH content.
putPlaybackConfigurationResponse_dashConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe DashConfiguration)
putPlaybackConfigurationResponse_dashConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {dashConfiguration} -> dashConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {dashConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
putPlaybackConfigurationResponse_manifestProcessingRules :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe ManifestProcessingRules)
putPlaybackConfigurationResponse_manifestProcessingRules = Lens.lens (\PutPlaybackConfigurationResponse' {manifestProcessingRules} -> manifestProcessingRules) (\s@PutPlaybackConfigurationResponse' {} a -> s {manifestProcessingRules = a} :: PutPlaybackConfigurationResponse)

-- | The Amazon CloudWatch log settings for a playback configuration.
putPlaybackConfigurationResponse_logConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe LogConfiguration)
putPlaybackConfigurationResponse_logConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {logConfiguration} -> logConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {logConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for pre-roll ad insertion.
putPlaybackConfigurationResponse_livePreRollConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe LivePreRollConfiguration)
putPlaybackConfigurationResponse_livePreRollConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {livePreRollConfiguration} -> livePreRollConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {livePreRollConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The identifier for the playback configuration.
putPlaybackConfigurationResponse_name :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_name = Lens.lens (\PutPlaybackConfigurationResponse' {name} -> name) (\s@PutPlaybackConfigurationResponse' {} a -> s {name = a} :: PutPlaybackConfigurationResponse)

-- | The URL that the player uses to initialize a session that uses
-- client-side reporting.
putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_sessionInitializationEndpointPrefix = Lens.lens (\PutPlaybackConfigurationResponse' {sessionInitializationEndpointPrefix} -> sessionInitializationEndpointPrefix) (\s@PutPlaybackConfigurationResponse' {} a -> s {sessionInitializationEndpointPrefix = a} :: PutPlaybackConfigurationResponse)

-- | The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
putPlaybackConfigurationResponse_configurationAliases :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
putPlaybackConfigurationResponse_configurationAliases = Lens.lens (\PutPlaybackConfigurationResponse' {configurationAliases} -> configurationAliases) (\s@PutPlaybackConfigurationResponse' {} a -> s {configurationAliases = a} :: PutPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
putPlaybackConfigurationResponse_transcodeProfileName :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_transcodeProfileName = Lens.lens (\PutPlaybackConfigurationResponse' {transcodeProfileName} -> transcodeProfileName) (\s@PutPlaybackConfigurationResponse' {} a -> s {transcodeProfileName = a} :: PutPlaybackConfigurationResponse)

-- | The tags assigned to the playback configuration.
putPlaybackConfigurationResponse_tags :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putPlaybackConfigurationResponse_tags = Lens.lens (\PutPlaybackConfigurationResponse' {tags} -> tags) (\s@PutPlaybackConfigurationResponse' {} a -> s {tags = a} :: PutPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID playback configurations. For VPAID, the slate is required
-- because MediaTailor provides it in the slots designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
putPlaybackConfigurationResponse_slateAdUrl :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
putPlaybackConfigurationResponse_slateAdUrl = Lens.lens (\PutPlaybackConfigurationResponse' {slateAdUrl} -> slateAdUrl) (\s@PutPlaybackConfigurationResponse' {} a -> s {slateAdUrl = a} :: PutPlaybackConfigurationResponse)

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
putPlaybackConfigurationResponse_cdnConfiguration :: Lens.Lens' PutPlaybackConfigurationResponse (Prelude.Maybe CdnConfiguration)
putPlaybackConfigurationResponse_cdnConfiguration = Lens.lens (\PutPlaybackConfigurationResponse' {cdnConfiguration} -> cdnConfiguration) (\s@PutPlaybackConfigurationResponse' {} a -> s {cdnConfiguration = a} :: PutPlaybackConfigurationResponse)

-- | The response's http status code.
putPlaybackConfigurationResponse_httpStatus :: Lens.Lens' PutPlaybackConfigurationResponse Prelude.Int
putPlaybackConfigurationResponse_httpStatus = Lens.lens (\PutPlaybackConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutPlaybackConfigurationResponse' {} a -> s {httpStatus = a} :: PutPlaybackConfigurationResponse)

instance
  Prelude.NFData
    PutPlaybackConfigurationResponse
