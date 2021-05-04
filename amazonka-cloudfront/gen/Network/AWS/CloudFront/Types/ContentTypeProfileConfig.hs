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
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfileConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfileConfig where

import Network.AWS.CloudFront.Types.ContentTypeProfiles
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration for a field-level encryption content type-profile
-- mapping.
--
-- /See:/ 'newContentTypeProfileConfig' smart constructor.
data ContentTypeProfileConfig = ContentTypeProfileConfig'
  { -- | The configuration for a field-level encryption content type-profile.
    contentTypeProfiles :: Prelude.Maybe ContentTypeProfiles,
    -- | The setting in a field-level encryption content type-profile mapping
    -- that specifies what to do when an unknown content type is provided for
    -- the profile. If true, content is forwarded without being encrypted when
    -- the content type is unknown. If false (the default), an error is
    -- returned when the content type is unknown.
    forwardWhenContentTypeIsUnknown :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContentTypeProfileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentTypeProfiles', 'contentTypeProfileConfig_contentTypeProfiles' - The configuration for a field-level encryption content type-profile.
--
-- 'forwardWhenContentTypeIsUnknown', 'contentTypeProfileConfig_forwardWhenContentTypeIsUnknown' - The setting in a field-level encryption content type-profile mapping
-- that specifies what to do when an unknown content type is provided for
-- the profile. If true, content is forwarded without being encrypted when
-- the content type is unknown. If false (the default), an error is
-- returned when the content type is unknown.
newContentTypeProfileConfig ::
  -- | 'forwardWhenContentTypeIsUnknown'
  Prelude.Bool ->
  ContentTypeProfileConfig
newContentTypeProfileConfig
  pForwardWhenContentTypeIsUnknown_ =
    ContentTypeProfileConfig'
      { contentTypeProfiles =
          Prelude.Nothing,
        forwardWhenContentTypeIsUnknown =
          pForwardWhenContentTypeIsUnknown_
      }

-- | The configuration for a field-level encryption content type-profile.
contentTypeProfileConfig_contentTypeProfiles :: Lens.Lens' ContentTypeProfileConfig (Prelude.Maybe ContentTypeProfiles)
contentTypeProfileConfig_contentTypeProfiles = Lens.lens (\ContentTypeProfileConfig' {contentTypeProfiles} -> contentTypeProfiles) (\s@ContentTypeProfileConfig' {} a -> s {contentTypeProfiles = a} :: ContentTypeProfileConfig)

-- | The setting in a field-level encryption content type-profile mapping
-- that specifies what to do when an unknown content type is provided for
-- the profile. If true, content is forwarded without being encrypted when
-- the content type is unknown. If false (the default), an error is
-- returned when the content type is unknown.
contentTypeProfileConfig_forwardWhenContentTypeIsUnknown :: Lens.Lens' ContentTypeProfileConfig Prelude.Bool
contentTypeProfileConfig_forwardWhenContentTypeIsUnknown = Lens.lens (\ContentTypeProfileConfig' {forwardWhenContentTypeIsUnknown} -> forwardWhenContentTypeIsUnknown) (\s@ContentTypeProfileConfig' {} a -> s {forwardWhenContentTypeIsUnknown = a} :: ContentTypeProfileConfig)

instance Prelude.FromXML ContentTypeProfileConfig where
  parseXML x =
    ContentTypeProfileConfig'
      Prelude.<$> (x Prelude..@? "ContentTypeProfiles")
      Prelude.<*> (x Prelude..@ "ForwardWhenContentTypeIsUnknown")

instance Prelude.Hashable ContentTypeProfileConfig

instance Prelude.NFData ContentTypeProfileConfig

instance Prelude.ToXML ContentTypeProfileConfig where
  toXML ContentTypeProfileConfig' {..} =
    Prelude.mconcat
      [ "ContentTypeProfiles"
          Prelude.@= contentTypeProfiles,
        "ForwardWhenContentTypeIsUnknown"
          Prelude.@= forwardWhenContentTypeIsUnknown
      ]
