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
-- Module      : Amazonka.MediaConvert.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SpekeKeyProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    -- | Specify the URL to the key server that your SPEKE-compliant DRM key
    -- provider uses to provide keys for encrypting your content.
    url :: Prelude.Maybe Prelude.Text,
    -- | If you want your key provider to encrypt the content keys that it
    -- provides to MediaConvert, set up a certificate with a master key using
    -- AWS Certificate Manager. Specify the certificate\'s Amazon Resource Name
    -- (ARN) here.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Relates to SPEKE implementation. DRM system identifiers. DASH output
    -- groups support a max of two system ids. Other group types support one
    -- system id. See https:\/\/dashif.org\/identifiers\/content_protection\/
    -- for more details.
    systemIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'url', 'spekeKeyProvider_url' - Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
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
newSpekeKeyProvider ::
  SpekeKeyProvider
newSpekeKeyProvider =
  SpekeKeyProvider'
    { resourceId = Prelude.Nothing,
      url = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      systemIds = Prelude.Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to
-- identify this content.
spekeKeyProvider_resourceId :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_resourceId = Lens.lens (\SpekeKeyProvider' {resourceId} -> resourceId) (\s@SpekeKeyProvider' {} a -> s {resourceId = a} :: SpekeKeyProvider)

-- | Specify the URL to the key server that your SPEKE-compliant DRM key
-- provider uses to provide keys for encrypting your content.
spekeKeyProvider_url :: Lens.Lens' SpekeKeyProvider (Prelude.Maybe Prelude.Text)
spekeKeyProvider_url = Lens.lens (\SpekeKeyProvider' {url} -> url) (\s@SpekeKeyProvider' {} a -> s {url = a} :: SpekeKeyProvider)

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
spekeKeyProvider_systemIds = Lens.lens (\SpekeKeyProvider' {systemIds} -> systemIds) (\s@SpekeKeyProvider' {} a -> s {systemIds = a} :: SpekeKeyProvider) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SpekeKeyProvider where
  parseJSON =
    Data.withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            Prelude.<$> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "systemIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SpekeKeyProvider where
  hashWithSalt _salt SpekeKeyProvider' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` systemIds

instance Prelude.NFData SpekeKeyProvider where
  rnf SpekeKeyProvider' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf systemIds

instance Data.ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceId" Data..=) Prelude.<$> resourceId,
            ("url" Data..=) Prelude.<$> url,
            ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("systemIds" Data..=) Prelude.<$> systemIds
          ]
      )
