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
-- Module      : Amazonka.MediaConvert.Types.SpekeKeyProviderCmaf
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SpekeKeyProviderCmaf where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If your output group type is CMAF, use these settings when doing DRM
-- encryption with a SPEKE-compliant key provider. If your output group
-- type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider
-- settings instead.
--
-- /See:/ 'newSpekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { -- | If you want your key provider to encrypt the content keys that it
    -- provides to MediaConvert, set up a certificate with a master key using
    -- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
    -- (ARN) here.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Specify the DRM system IDs that you want signaled in the DASH manifest
    -- that MediaConvert creates as part of this CMAF package. The DASH
    -- manifest can currently signal up to three system IDs. For more
    -- information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    dashSignaledSystemIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify the DRM system ID that you want signaled in the HLS manifest
    -- that MediaConvert creates as part of this CMAF package. The HLS manifest
    -- can currently signal only one system ID. For more information, see
    -- https:\/\/dashif.org\/identifiers\/content_protection\/.
    hlsSignaledSystemIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify the resource ID that your SPEKE-compliant key provider uses to
    -- identify this content.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key
    -- provider uses to provide keys for encrypting your content.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpekeKeyProviderCmaf' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'spekeKeyProviderCmaf_certificateArn' - If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
--
-- 'dashSignaledSystemIds', 'spekeKeyProviderCmaf_dashSignaledSystemIds' - Specify the DRM system IDs that you want signaled in the DASH manifest
-- that MediaConvert creates as part of this CMAF package. The DASH
-- manifest can currently signal up to three system IDs. For more
-- information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
--
-- 'hlsSignaledSystemIds', 'spekeKeyProviderCmaf_hlsSignaledSystemIds' - Specify the DRM system ID that you want signaled in the HLS manifest
-- that MediaConvert creates as part of this CMAF package. The HLS manifest
-- can currently signal only one system ID. For more information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
--
-- 'resourceId', 'spekeKeyProviderCmaf_resourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
--
-- 'url', 'spekeKeyProviderCmaf_url' - Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
newSpekeKeyProviderCmaf ::
  SpekeKeyProviderCmaf
newSpekeKeyProviderCmaf =
  SpekeKeyProviderCmaf'
    { certificateArn =
        Prelude.Nothing,
      dashSignaledSystemIds = Prelude.Nothing,
      hlsSignaledSystemIds = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
spekeKeyProviderCmaf_certificateArn :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_certificateArn = Lens.lens (\SpekeKeyProviderCmaf' {certificateArn} -> certificateArn) (\s@SpekeKeyProviderCmaf' {} a -> s {certificateArn = a} :: SpekeKeyProviderCmaf)

-- | Specify the DRM system IDs that you want signaled in the DASH manifest
-- that MediaConvert creates as part of this CMAF package. The DASH
-- manifest can currently signal up to three system IDs. For more
-- information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_dashSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe [Prelude.Text])
spekeKeyProviderCmaf_dashSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {dashSignaledSystemIds} -> dashSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {dashSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Prelude.. Lens.mapping Lens.coerced

-- | Specify the DRM system ID that you want signaled in the HLS manifest
-- that MediaConvert creates as part of this CMAF package. The HLS manifest
-- can currently signal only one system ID. For more information, see
-- https:\/\/dashif.org\/identifiers\/content_protection\/.
spekeKeyProviderCmaf_hlsSignaledSystemIds :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe [Prelude.Text])
spekeKeyProviderCmaf_hlsSignaledSystemIds = Lens.lens (\SpekeKeyProviderCmaf' {hlsSignaledSystemIds} -> hlsSignaledSystemIds) (\s@SpekeKeyProviderCmaf' {} a -> s {hlsSignaledSystemIds = a} :: SpekeKeyProviderCmaf) Prelude.. Lens.mapping Lens.coerced

-- | Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
spekeKeyProviderCmaf_resourceId :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_resourceId = Lens.lens (\SpekeKeyProviderCmaf' {resourceId} -> resourceId) (\s@SpekeKeyProviderCmaf' {} a -> s {resourceId = a} :: SpekeKeyProviderCmaf)

-- | Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
spekeKeyProviderCmaf_url :: Lens.Lens' SpekeKeyProviderCmaf (Prelude.Maybe Prelude.Text)
spekeKeyProviderCmaf_url = Lens.lens (\SpekeKeyProviderCmaf' {url} -> url) (\s@SpekeKeyProviderCmaf' {} a -> s {url = a} :: SpekeKeyProviderCmaf)

instance Data.FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    Data.withObject
      "SpekeKeyProviderCmaf"
      ( \x ->
          SpekeKeyProviderCmaf'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> ( x
                            Data..:? "dashSignaledSystemIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "hlsSignaledSystemIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable SpekeKeyProviderCmaf where
  hashWithSalt _salt SpekeKeyProviderCmaf' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` dashSignaledSystemIds
      `Prelude.hashWithSalt` hlsSignaledSystemIds
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` url

instance Prelude.NFData SpekeKeyProviderCmaf where
  rnf SpekeKeyProviderCmaf' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf dashSignaledSystemIds
      `Prelude.seq` Prelude.rnf hlsSignaledSystemIds
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf url

instance Data.ToJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("dashSignaledSystemIds" Data..=)
              Prelude.<$> dashSignaledSystemIds,
            ("hlsSignaledSystemIds" Data..=)
              Prelude.<$> hlsSignaledSystemIds,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("url" Data..=) Prelude.<$> url
          ]
      )
