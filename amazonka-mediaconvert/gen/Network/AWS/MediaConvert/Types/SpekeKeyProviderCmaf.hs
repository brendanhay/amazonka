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
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | If your output group type is CMAF, use these settings when doing DRM
-- encryption with a SPEKE-compliant key provider. If your output group
-- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
-- settings instead.
--
-- /See:/ 'newSpekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { -- | Specify the resource ID that your SPEKE-compliant key provider uses to
    -- identify this content.
    resourceId :: Core.Maybe Core.Text,
    -- | If you want your key provider to encrypt the content keys that it
    -- provides to MediaConvert, set up a certificate with a master key using
    -- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
    -- (ARN) here.
    certificateArn :: Core.Maybe Core.Text,
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key
    -- provider uses to provide keys for encrypting your content.
    url :: Core.Maybe Core.Text,
    -- | Specify the DRM system ID that you want signaled in the HLS manifest
    -- that MediaConvert creates as part of this CMAF package. The HLS manifest
    -- can currently signal only one system ID. For more information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    hlsSignaledSystemIds :: Core.Maybe [Core.Text],
    -- | Specify the DRM system IDs that you want signaled in the DASH manifest
    -- that MediaConvert creates as part of this CMAF package. The DASH
    -- manifest can currently signal up to three system IDs. For more
    -- information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    dashSignaledSystemIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpekeKeyProviderCmaf' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'spekeKeyProviderCmaf_resourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
--
-- 'certificateArn', 'spekeKeyProviderCmaf_certificateArn' - If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
--
-- 'url', 'spekeKeyProviderCmaf_url' - Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
--
-- 'hlsSignaledSystemIds', 'spekeKeyProviderCmaf_hlsSignaledSystemIds' - Specify the DRM system ID that you want signaled in the HLS manifest
-- that MediaConvert creates as part of this CMAF package. The HLS manifest
-- can currently signal only one system ID. For more information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
--
-- 'dashSignaledSystemIds', 'spekeKeyProviderCmaf_dashSignaledSystemIds' - Specify the DRM system IDs that you want signaled in the DASH manifest
-- that MediaConvert creates as part of this CMAF package. The DASH
-- manifest can currently signal up to three system IDs. For more
-- information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
newSpekeKeyProviderCmaf ::
  SpekeKeyProviderCmaf
newSpekeKeyProviderCmaf =
  SpekeKeyProviderCmaf'
    { resourceId = Core.Nothing,
      certificateArn = Core.Nothing,
      url = Core.Nothing,
      hlsSignaledSystemIds = Core.Nothing,
      dashSignaledSystemIds = Core.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
spekeKeyProviderCmaf_resourceId :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
spekeKeyProviderCmaf_resourceId = Lens.lens (\SpekeKeyProviderCmaf' {resourceId} -> resourceId) (\s@SpekeKeyProviderCmaf' {} a -> s {resourceId = a} :: SpekeKeyProviderCmaf)

-- | If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
spekeKeyProviderCmaf_certificateArn :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
spekeKeyProviderCmaf_certificateArn = Lens.lens (\SpekeKeyProviderCmaf' {certificateArn} -> certificateArn) (\s@SpekeKeyProviderCmaf' {} a -> s {certificateArn = a} :: SpekeKeyProviderCmaf)

-- | Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
spekeKeyProviderCmaf_url :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe Core.Text)
spekeKeyProviderCmaf_url = Lens.lens (\SpekeKeyProviderCmaf' {url} -> url) (\s@SpekeKeyProviderCmaf' {} a -> s {url = a} :: SpekeKeyProviderCmaf)

-- | Specify the DRM system ID that you want signaled in the HLS manifest
-- that MediaConvert creates as part of this CMAF package. The HLS manifest
-- can currently signal only one system ID. For more information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_hlsSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe [Core.Text])
spekeKeyProviderCmaf_hlsSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {hlsSignaledSystemIds} -> hlsSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {hlsSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Core.. Lens.mapping Lens._Coerce

-- | Specify the DRM system IDs that you want signaled in the DASH manifest
-- that MediaConvert creates as part of this CMAF package. The DASH
-- manifest can currently signal up to three system IDs. For more
-- information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_dashSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Core.Maybe [Core.Text])
spekeKeyProviderCmaf_dashSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {dashSignaledSystemIds} -> dashSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {dashSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    Core.withObject
      "SpekeKeyProviderCmaf"
      ( \x ->
          SpekeKeyProviderCmaf'
            Core.<$> (x Core..:? "resourceId")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "url")
            Core.<*> ( x Core..:? "hlsSignaledSystemIds"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "dashSignaledSystemIds"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable SpekeKeyProviderCmaf

instance Core.NFData SpekeKeyProviderCmaf

instance Core.ToJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf' {..} =
    Core.object
      ( Core.catMaybes
          [ ("resourceId" Core..=) Core.<$> resourceId,
            ("certificateArn" Core..=) Core.<$> certificateArn,
            ("url" Core..=) Core.<$> url,
            ("hlsSignaledSystemIds" Core..=)
              Core.<$> hlsSignaledSystemIds,
            ("dashSignaledSystemIds" Core..=)
              Core.<$> dashSignaledSystemIds
          ]
      )
