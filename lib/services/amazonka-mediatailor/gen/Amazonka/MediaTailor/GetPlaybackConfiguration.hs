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
-- Module      : Amazonka.MediaTailor.GetPlaybackConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the playback configuration for the specified name.
module Amazonka.MediaTailor.GetPlaybackConfiguration
  ( -- * Creating a Request
    GetPlaybackConfiguration (..),
    newGetPlaybackConfiguration,

    -- * Request Lenses
    getPlaybackConfiguration_name,

    -- * Destructuring the Response
    GetPlaybackConfigurationResponse (..),
    newGetPlaybackConfigurationResponse,

    -- * Response Lenses
    getPlaybackConfigurationResponse_playbackEndpointPrefix,
    getPlaybackConfigurationResponse_hlsConfiguration,
    getPlaybackConfigurationResponse_personalizationThresholdSeconds,
    getPlaybackConfigurationResponse_playbackConfigurationArn,
    getPlaybackConfigurationResponse_availSuppression,
    getPlaybackConfigurationResponse_bumper,
    getPlaybackConfigurationResponse_adDecisionServerUrl,
    getPlaybackConfigurationResponse_videoContentSourceUrl,
    getPlaybackConfigurationResponse_dashConfiguration,
    getPlaybackConfigurationResponse_manifestProcessingRules,
    getPlaybackConfigurationResponse_logConfiguration,
    getPlaybackConfigurationResponse_livePreRollConfiguration,
    getPlaybackConfigurationResponse_name,
    getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix,
    getPlaybackConfigurationResponse_configurationAliases,
    getPlaybackConfigurationResponse_transcodeProfileName,
    getPlaybackConfigurationResponse_tags,
    getPlaybackConfigurationResponse_slateAdUrl,
    getPlaybackConfigurationResponse_cdnConfiguration,
    getPlaybackConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPlaybackConfiguration' smart constructor.
data GetPlaybackConfiguration = GetPlaybackConfiguration'
  { -- | The identifier for the playback configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlaybackConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getPlaybackConfiguration_name' - The identifier for the playback configuration.
newGetPlaybackConfiguration ::
  -- | 'name'
  Prelude.Text ->
  GetPlaybackConfiguration
newGetPlaybackConfiguration pName_ =
  GetPlaybackConfiguration' {name = pName_}

-- | The identifier for the playback configuration.
getPlaybackConfiguration_name :: Lens.Lens' GetPlaybackConfiguration Prelude.Text
getPlaybackConfiguration_name = Lens.lens (\GetPlaybackConfiguration' {name} -> name) (\s@GetPlaybackConfiguration' {} a -> s {name = a} :: GetPlaybackConfiguration)

instance Core.AWSRequest GetPlaybackConfiguration where
  type
    AWSResponse GetPlaybackConfiguration =
      GetPlaybackConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPlaybackConfigurationResponse'
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

instance Prelude.Hashable GetPlaybackConfiguration

instance Prelude.NFData GetPlaybackConfiguration

instance Core.ToHeaders GetPlaybackConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetPlaybackConfiguration where
  toPath GetPlaybackConfiguration' {..} =
    Prelude.mconcat
      ["/playbackConfiguration/", Core.toBS name]

instance Core.ToQuery GetPlaybackConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPlaybackConfigurationResponse' smart constructor.
data GetPlaybackConfigurationResponse = GetPlaybackConfigurationResponse'
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
-- Create a value of 'GetPlaybackConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playbackEndpointPrefix', 'getPlaybackConfigurationResponse_playbackEndpointPrefix' - The URL that the player accesses to get a manifest from AWS Elemental
-- MediaTailor. This session will use server-side reporting.
--
-- 'hlsConfiguration', 'getPlaybackConfigurationResponse_hlsConfiguration' - The configuration for HLS content.
--
-- 'personalizationThresholdSeconds', 'getPlaybackConfigurationResponse_personalizationThresholdSeconds' - Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
--
-- 'playbackConfigurationArn', 'getPlaybackConfigurationResponse_playbackConfigurationArn' - The Amazon Resource Name (ARN) for the playback configuration.
--
-- 'availSuppression', 'getPlaybackConfigurationResponse_availSuppression' - The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
--
-- 'bumper', 'getPlaybackConfigurationResponse_bumper' - The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
--
-- 'adDecisionServerUrl', 'getPlaybackConfigurationResponse_adDecisionServerUrl' - The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing, you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
--
-- 'videoContentSourceUrl', 'getPlaybackConfigurationResponse_videoContentSourceUrl' - The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
--
-- 'dashConfiguration', 'getPlaybackConfigurationResponse_dashConfiguration' - The configuration for DASH content.
--
-- 'manifestProcessingRules', 'getPlaybackConfigurationResponse_manifestProcessingRules' - The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
--
-- 'logConfiguration', 'getPlaybackConfigurationResponse_logConfiguration' - The Amazon CloudWatch log settings for a playback configuration.
--
-- 'livePreRollConfiguration', 'getPlaybackConfigurationResponse_livePreRollConfiguration' - The configuration for pre-roll ad insertion.
--
-- 'name', 'getPlaybackConfigurationResponse_name' - The identifier for the playback configuration.
--
-- 'sessionInitializationEndpointPrefix', 'getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix' - The URL that the player uses to initialize a session that uses
-- client-side reporting.
--
-- 'configurationAliases', 'getPlaybackConfigurationResponse_configurationAliases' - The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
--
-- 'transcodeProfileName', 'getPlaybackConfigurationResponse_transcodeProfileName' - The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
--
-- 'tags', 'getPlaybackConfigurationResponse_tags' - The tags assigned to the playback configuration.
--
-- 'slateAdUrl', 'getPlaybackConfigurationResponse_slateAdUrl' - The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID playback configurations. For VPAID, the slate is required
-- because MediaTailor provides it in the slots designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
--
-- 'cdnConfiguration', 'getPlaybackConfigurationResponse_cdnConfiguration' - The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
--
-- 'httpStatus', 'getPlaybackConfigurationResponse_httpStatus' - The response's http status code.
newGetPlaybackConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPlaybackConfigurationResponse
newGetPlaybackConfigurationResponse pHttpStatus_ =
  GetPlaybackConfigurationResponse'
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
getPlaybackConfigurationResponse_playbackEndpointPrefix :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_playbackEndpointPrefix = Lens.lens (\GetPlaybackConfigurationResponse' {playbackEndpointPrefix} -> playbackEndpointPrefix) (\s@GetPlaybackConfigurationResponse' {} a -> s {playbackEndpointPrefix = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for HLS content.
getPlaybackConfigurationResponse_hlsConfiguration :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe HlsConfiguration)
getPlaybackConfigurationResponse_hlsConfiguration = Lens.lens (\GetPlaybackConfigurationResponse' {hlsConfiguration} -> hlsConfiguration) (\s@GetPlaybackConfigurationResponse' {} a -> s {hlsConfiguration = a} :: GetPlaybackConfigurationResponse)

-- | Defines the maximum duration of underfilled ad time (in seconds) allowed
-- in an ad break. If the duration of underfilled ad time exceeds the
-- personalization threshold, then the personalization of the ad break is
-- abandoned and the underlying content is shown. This feature applies to
-- /ad replacement/ in live and VOD streams, rather than ad insertion,
-- because it relies on an underlying content stream. For more information
-- about ad break behavior, including ad replacement and insertion, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Behavior in AWS Elemental MediaTailor>.
getPlaybackConfigurationResponse_personalizationThresholdSeconds :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Natural)
getPlaybackConfigurationResponse_personalizationThresholdSeconds = Lens.lens (\GetPlaybackConfigurationResponse' {personalizationThresholdSeconds} -> personalizationThresholdSeconds) (\s@GetPlaybackConfigurationResponse' {} a -> s {personalizationThresholdSeconds = a} :: GetPlaybackConfigurationResponse)

-- | The Amazon Resource Name (ARN) for the playback configuration.
getPlaybackConfigurationResponse_playbackConfigurationArn :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_playbackConfigurationArn = Lens.lens (\GetPlaybackConfigurationResponse' {playbackConfigurationArn} -> playbackConfigurationArn) (\s@GetPlaybackConfigurationResponse' {} a -> s {playbackConfigurationArn = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
getPlaybackConfigurationResponse_availSuppression :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe AvailSuppression)
getPlaybackConfigurationResponse_availSuppression = Lens.lens (\GetPlaybackConfigurationResponse' {availSuppression} -> availSuppression) (\s@GetPlaybackConfigurationResponse' {} a -> s {availSuppression = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for bumpers. Bumpers are short audio or video clips
-- that play at the start or before the end of an ad break. To learn more
-- about bumpers, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/bumpers.html Bumpers>.
getPlaybackConfigurationResponse_bumper :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Bumper)
getPlaybackConfigurationResponse_bumper = Lens.lens (\GetPlaybackConfigurationResponse' {bumper} -> bumper) (\s@GetPlaybackConfigurationResponse' {} a -> s {bumper = a} :: GetPlaybackConfigurationResponse)

-- | The URL for the ad decision server (ADS). This includes the
-- specification of static parameters and placeholders for dynamic
-- parameters. AWS Elemental MediaTailor substitutes player-specific and
-- session-specific parameters as needed when calling the ADS. Alternately,
-- for testing, you can provide a static VAST URL. The maximum length is
-- 25,000 characters.
getPlaybackConfigurationResponse_adDecisionServerUrl :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_adDecisionServerUrl = Lens.lens (\GetPlaybackConfigurationResponse' {adDecisionServerUrl} -> adDecisionServerUrl) (\s@GetPlaybackConfigurationResponse' {} a -> s {adDecisionServerUrl = a} :: GetPlaybackConfigurationResponse)

-- | The URL prefix for the parent manifest for the stream, minus the asset
-- ID. The maximum length is 512 characters.
getPlaybackConfigurationResponse_videoContentSourceUrl :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_videoContentSourceUrl = Lens.lens (\GetPlaybackConfigurationResponse' {videoContentSourceUrl} -> videoContentSourceUrl) (\s@GetPlaybackConfigurationResponse' {} a -> s {videoContentSourceUrl = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for DASH content.
getPlaybackConfigurationResponse_dashConfiguration :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe DashConfiguration)
getPlaybackConfigurationResponse_dashConfiguration = Lens.lens (\GetPlaybackConfigurationResponse' {dashConfiguration} -> dashConfiguration) (\s@GetPlaybackConfigurationResponse' {} a -> s {dashConfiguration = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for manifest processing rules. Manifest processing
-- rules enable customization of the personalized manifests created by
-- MediaTailor.
getPlaybackConfigurationResponse_manifestProcessingRules :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe ManifestProcessingRules)
getPlaybackConfigurationResponse_manifestProcessingRules = Lens.lens (\GetPlaybackConfigurationResponse' {manifestProcessingRules} -> manifestProcessingRules) (\s@GetPlaybackConfigurationResponse' {} a -> s {manifestProcessingRules = a} :: GetPlaybackConfigurationResponse)

-- | The Amazon CloudWatch log settings for a playback configuration.
getPlaybackConfigurationResponse_logConfiguration :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe LogConfiguration)
getPlaybackConfigurationResponse_logConfiguration = Lens.lens (\GetPlaybackConfigurationResponse' {logConfiguration} -> logConfiguration) (\s@GetPlaybackConfigurationResponse' {} a -> s {logConfiguration = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for pre-roll ad insertion.
getPlaybackConfigurationResponse_livePreRollConfiguration :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe LivePreRollConfiguration)
getPlaybackConfigurationResponse_livePreRollConfiguration = Lens.lens (\GetPlaybackConfigurationResponse' {livePreRollConfiguration} -> livePreRollConfiguration) (\s@GetPlaybackConfigurationResponse' {} a -> s {livePreRollConfiguration = a} :: GetPlaybackConfigurationResponse)

-- | The identifier for the playback configuration.
getPlaybackConfigurationResponse_name :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_name = Lens.lens (\GetPlaybackConfigurationResponse' {name} -> name) (\s@GetPlaybackConfigurationResponse' {} a -> s {name = a} :: GetPlaybackConfigurationResponse)

-- | The URL that the player uses to initialize a session that uses
-- client-side reporting.
getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_sessionInitializationEndpointPrefix = Lens.lens (\GetPlaybackConfigurationResponse' {sessionInitializationEndpointPrefix} -> sessionInitializationEndpointPrefix) (\s@GetPlaybackConfigurationResponse' {} a -> s {sessionInitializationEndpointPrefix = a} :: GetPlaybackConfigurationResponse)

-- | The player parameters and aliases used as dynamic variables during
-- session initialization. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables-domain.html Domain Variables>.
getPlaybackConfigurationResponse_configurationAliases :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
getPlaybackConfigurationResponse_configurationAliases = Lens.lens (\GetPlaybackConfigurationResponse' {configurationAliases} -> configurationAliases) (\s@GetPlaybackConfigurationResponse' {} a -> s {configurationAliases = a} :: GetPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name that is used to associate this playback configuration with a
-- custom transcode profile. This overrides the dynamic transcoding
-- defaults of MediaTailor. Use this only if you have already set up custom
-- profiles with the help of AWS Support.
getPlaybackConfigurationResponse_transcodeProfileName :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_transcodeProfileName = Lens.lens (\GetPlaybackConfigurationResponse' {transcodeProfileName} -> transcodeProfileName) (\s@GetPlaybackConfigurationResponse' {} a -> s {transcodeProfileName = a} :: GetPlaybackConfigurationResponse)

-- | The tags assigned to the playback configuration.
getPlaybackConfigurationResponse_tags :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getPlaybackConfigurationResponse_tags = Lens.lens (\GetPlaybackConfigurationResponse' {tags} -> tags) (\s@GetPlaybackConfigurationResponse' {} a -> s {tags = a} :: GetPlaybackConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The URL for a high-quality video asset to transcode and use to fill in
-- time that\'s not used by ads. AWS Elemental MediaTailor shows the slate
-- to fill in gaps in media content. Configuring the slate is optional for
-- non-VPAID playback configurations. For VPAID, the slate is required
-- because MediaTailor provides it in the slots designated for dynamic ad
-- content. The slate must be a high-quality asset that contains both audio
-- and video.
getPlaybackConfigurationResponse_slateAdUrl :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe Prelude.Text)
getPlaybackConfigurationResponse_slateAdUrl = Lens.lens (\GetPlaybackConfigurationResponse' {slateAdUrl} -> slateAdUrl) (\s@GetPlaybackConfigurationResponse' {} a -> s {slateAdUrl = a} :: GetPlaybackConfigurationResponse)

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
getPlaybackConfigurationResponse_cdnConfiguration :: Lens.Lens' GetPlaybackConfigurationResponse (Prelude.Maybe CdnConfiguration)
getPlaybackConfigurationResponse_cdnConfiguration = Lens.lens (\GetPlaybackConfigurationResponse' {cdnConfiguration} -> cdnConfiguration) (\s@GetPlaybackConfigurationResponse' {} a -> s {cdnConfiguration = a} :: GetPlaybackConfigurationResponse)

-- | The response's http status code.
getPlaybackConfigurationResponse_httpStatus :: Lens.Lens' GetPlaybackConfigurationResponse Prelude.Int
getPlaybackConfigurationResponse_httpStatus = Lens.lens (\GetPlaybackConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetPlaybackConfigurationResponse' {} a -> s {httpStatus = a} :: GetPlaybackConfigurationResponse)

instance
  Prelude.NFData
    GetPlaybackConfigurationResponse
