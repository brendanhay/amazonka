{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If your output group type is CMAF, use these settings when doing DRM
-- encryption with a SPEKE-compliant key provider. If your output group
-- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
-- settings instead.
--
-- /See:/ 'newSpekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { -- | Specify the resource ID that your SPEKE-compliant key provider uses to
    -- identify this content.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | If you want your key provider to encrypt the content keys that it
    -- provides to MediaConvert, set up a certificate with a master key using
    -- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
    -- (ARN) here.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key
    -- provider uses to provide keys for encrypting your content.
    url :: Prelude.Maybe Prelude.Text,
    -- | Specify the DRM system ID that you want signaled in the HLS manifest
    -- that MediaConvert creates as part of this CMAF package. The HLS manifest
    -- can currently signal only one system ID. For more information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    hlsSignaledSystemIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify the DRM system IDs that you want signaled in the DASH manifest
    -- that MediaConvert creates as part of this CMAF package. The DASH
    -- manifest can currently signal up to three system IDs. For more
    -- information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    dashSignaledSystemIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      url = Prelude.Nothing,
      hlsSignaledSystemIds = Prelude.Nothing,
      dashSignaledSystemIds = Prelude.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
spekeKeyProviderCmaf_resourceId :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_resourceId = Lens.lens (\SpekeKeyProviderCmaf' {resourceId} -> resourceId) (\s@SpekeKeyProviderCmaf' {} a -> s {resourceId = a} :: SpekeKeyProviderCmaf)

-- | If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
spekeKeyProviderCmaf_certificateArn :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_certificateArn = Lens.lens (\SpekeKeyProviderCmaf' {certificateArn} -> certificateArn) (\s@SpekeKeyProviderCmaf' {} a -> s {certificateArn = a} :: SpekeKeyProviderCmaf)

-- | Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
spekeKeyProviderCmaf_url :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_url = Lens.lens (\SpekeKeyProviderCmaf' {url} -> url) (\s@SpekeKeyProviderCmaf' {} a -> s {url = a} :: SpekeKeyProviderCmaf)

-- | Specify the DRM system ID that you want signaled in the HLS manifest
-- that MediaConvert creates as part of this CMAF package. The HLS manifest
-- can currently signal only one system ID. For more information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_hlsSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe [Prelude.Text])
spekeKeyProviderCmaf_hlsSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {hlsSignaledSystemIds} -> hlsSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {hlsSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify the DRM system IDs that you want signaled in the DASH manifest
-- that MediaConvert creates as part of this CMAF package. The DASH
-- manifest can currently signal up to three system IDs. For more
-- information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_dashSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe [Prelude.Text])
spekeKeyProviderCmaf_dashSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {dashSignaledSystemIds} -> dashSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {dashSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    Prelude.withObject
      "SpekeKeyProviderCmaf"
      ( \x ->
          SpekeKeyProviderCmaf'
            Prelude.<$> (x Prelude..:? "resourceId")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> ( x Prelude..:? "hlsSignaledSystemIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "dashSignaledSystemIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SpekeKeyProviderCmaf

instance Prelude.NFData SpekeKeyProviderCmaf

instance Prelude.ToJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("resourceId" Prelude..=) Prelude.<$> resourceId,
            ("certificateArn" Prelude..=)
              Prelude.<$> certificateArn,
            ("url" Prelude..=) Prelude.<$> url,
            ("hlsSignaledSystemIds" Prelude..=)
              Prelude.<$> hlsSignaledSystemIds,
            ("dashSignaledSystemIds" Prelude..=)
              Prelude.<$> dashSignaledSystemIds
          ]
      )
