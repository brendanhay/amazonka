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
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProvider where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
--
-- /See:/ 'newSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { -- | Specify the resource ID that your SPEKE-compliant key provider uses to
    -- identify this content.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | If you want your key provider to encrypt the content keys that it
    -- provides to MediaConvert, set up a certificate with a master key using
    -- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
    -- (ARN) here.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Relates to SPEKE implementation. DRM system identifiers. DASH output
    -- groups support a max of two system ids. Other group types support one
    -- system id. See https:\/\/dashif.org\/identifiers\/content_protection\/
    -- for more details.
    systemIds :: Prelude.Maybe [Prelude.Text],
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key
    -- provider uses to provide keys for encrypting your content.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpekeKeyProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'spekeKeyProvider_resourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
--
-- 'certificateArn', 'spekeKeyProvider_certificateArn' - If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
--
-- 'systemIds', 'spekeKeyProvider_systemIds' - Relates to SPEKE implementation. DRM system identifiers. DASH output
-- groups support a max of two system ids. Other group types support one
-- system id. See https:\/\/dashif.org\/identifiers\/content_protection\/
-- for more details.
--
-- 'url', 'spekeKeyProvider_url' - Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
newSpekeKeyProvider ::
  SpekeKeyProvider
newSpekeKeyProvider =
  SpekeKeyProvider'
    { resourceId = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      systemIds = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

-- | If you want your key provider to encrypt the content keys that it
-- provides to MediaConvert, set up a certificate with a master key using
-- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
-- (ARN) here.
spekeKeyProvider_certificateArn :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_certificateArn = Lens.lens (\SpekeKeyProvider' {certificateArn} -> certificateArn) (\s@SpekeKeyProvider' {} a -> s {certificateArn = a} :: SpekeKeyProvider)

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output
-- groups support a max of two system ids. Other group types support one
-- system id. See https:\/\/dashif.org\/identifiers\/content_protection\/
-- for more details.
spekeKeyProvider_systemIds :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe [Prelude.Text])
spekeKeyProvider_systemIds = Lens.lens (\SpekeKeyProvider' {systemIds} -> systemIds) (\s@SpekeKeyProvider' {} a -> s {systemIds = a} :: SpekeKeyProvider) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

instance Prelude.FromJSON SpekeKeyProvider where
  parseJSON =
    Prelude.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Prelude.<$> (x Prelude..:? "resourceId")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> ( x Prelude..:? "systemIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "url")
      )

instance Prelude.Hashable SpekeKeyProvider

instance Prelude.NFData SpekeKeyProvider

instance Prelude.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("resourceId" Prelude..=) Prelude.<$> resourceId,
            ("certificateArn" Prelude..=)
              Prelude.<$> certificateArn,
            ("systemIds" Prelude..=) Prelude.<$> systemIds,
            ("url" Prelude..=) Prelude.<$> url
          ]
      )
