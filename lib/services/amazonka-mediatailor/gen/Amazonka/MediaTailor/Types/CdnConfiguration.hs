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
-- Module      : Amazonka.MediaTailor.Types.CdnConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.CdnConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for using a content delivery network (CDN), like
-- Amazon CloudFront, for content and ad segment management.
--
-- /See:/ 'newCdnConfiguration' smart constructor.
data CdnConfiguration = CdnConfiguration'
  { -- | A non-default content delivery network (CDN) to serve ad segments. By
    -- default, AWS Elemental MediaTailor uses Amazon CloudFront with default
    -- cache settings as its CDN for ad segments. To set up an alternate CDN,
    -- create a rule in your CDN for the origin
    -- ads.mediatailor./\<region>/.amazonaws.com. Then specify the rule\'s name
    -- in this @AdSegmentUrlPrefix@. When AWS Elemental MediaTailor serves a
    -- manifest, it reports your CDN as the source for ad segments.
    adSegmentUrlPrefix :: Prelude.Maybe Prelude.Text,
    -- | A content delivery network (CDN) to cache content segments, so that
    -- content requests don’t always have to go to the origin server. First,
    -- create a rule in your CDN for the content segment origin server. Then
    -- specify the rule\'s name in this @ContentSegmentUrlPrefix@. When AWS
    -- Elemental MediaTailor serves a manifest, it reports your CDN as the
    -- source for content segments.
    contentSegmentUrlPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CdnConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adSegmentUrlPrefix', 'cdnConfiguration_adSegmentUrlPrefix' - A non-default content delivery network (CDN) to serve ad segments. By
-- default, AWS Elemental MediaTailor uses Amazon CloudFront with default
-- cache settings as its CDN for ad segments. To set up an alternate CDN,
-- create a rule in your CDN for the origin
-- ads.mediatailor./\<region>/.amazonaws.com. Then specify the rule\'s name
-- in this @AdSegmentUrlPrefix@. When AWS Elemental MediaTailor serves a
-- manifest, it reports your CDN as the source for ad segments.
--
-- 'contentSegmentUrlPrefix', 'cdnConfiguration_contentSegmentUrlPrefix' - A content delivery network (CDN) to cache content segments, so that
-- content requests don’t always have to go to the origin server. First,
-- create a rule in your CDN for the content segment origin server. Then
-- specify the rule\'s name in this @ContentSegmentUrlPrefix@. When AWS
-- Elemental MediaTailor serves a manifest, it reports your CDN as the
-- source for content segments.
newCdnConfiguration ::
  CdnConfiguration
newCdnConfiguration =
  CdnConfiguration'
    { adSegmentUrlPrefix =
        Prelude.Nothing,
      contentSegmentUrlPrefix = Prelude.Nothing
    }

-- | A non-default content delivery network (CDN) to serve ad segments. By
-- default, AWS Elemental MediaTailor uses Amazon CloudFront with default
-- cache settings as its CDN for ad segments. To set up an alternate CDN,
-- create a rule in your CDN for the origin
-- ads.mediatailor./\<region>/.amazonaws.com. Then specify the rule\'s name
-- in this @AdSegmentUrlPrefix@. When AWS Elemental MediaTailor serves a
-- manifest, it reports your CDN as the source for ad segments.
cdnConfiguration_adSegmentUrlPrefix :: Lens.Lens' CdnConfiguration (Prelude.Maybe Prelude.Text)
cdnConfiguration_adSegmentUrlPrefix = Lens.lens (\CdnConfiguration' {adSegmentUrlPrefix} -> adSegmentUrlPrefix) (\s@CdnConfiguration' {} a -> s {adSegmentUrlPrefix = a} :: CdnConfiguration)

-- | A content delivery network (CDN) to cache content segments, so that
-- content requests don’t always have to go to the origin server. First,
-- create a rule in your CDN for the content segment origin server. Then
-- specify the rule\'s name in this @ContentSegmentUrlPrefix@. When AWS
-- Elemental MediaTailor serves a manifest, it reports your CDN as the
-- source for content segments.
cdnConfiguration_contentSegmentUrlPrefix :: Lens.Lens' CdnConfiguration (Prelude.Maybe Prelude.Text)
cdnConfiguration_contentSegmentUrlPrefix = Lens.lens (\CdnConfiguration' {contentSegmentUrlPrefix} -> contentSegmentUrlPrefix) (\s@CdnConfiguration' {} a -> s {contentSegmentUrlPrefix = a} :: CdnConfiguration)

instance Core.FromJSON CdnConfiguration where
  parseJSON =
    Core.withObject
      "CdnConfiguration"
      ( \x ->
          CdnConfiguration'
            Prelude.<$> (x Core..:? "AdSegmentUrlPrefix")
            Prelude.<*> (x Core..:? "ContentSegmentUrlPrefix")
      )

instance Prelude.Hashable CdnConfiguration where
  hashWithSalt _salt CdnConfiguration' {..} =
    _salt `Prelude.hashWithSalt` adSegmentUrlPrefix
      `Prelude.hashWithSalt` contentSegmentUrlPrefix

instance Prelude.NFData CdnConfiguration where
  rnf CdnConfiguration' {..} =
    Prelude.rnf adSegmentUrlPrefix
      `Prelude.seq` Prelude.rnf contentSegmentUrlPrefix

instance Core.ToJSON CdnConfiguration where
  toJSON CdnConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AdSegmentUrlPrefix" Core..=)
              Prelude.<$> adSegmentUrlPrefix,
            ("ContentSegmentUrlPrefix" Core..=)
              Prelude.<$> contentSegmentUrlPrefix
          ]
      )
