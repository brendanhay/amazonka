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
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfile where

import Network.AWS.CloudFront.Types.Format
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A field-level encryption content type profile.
--
-- /See:/ 'newContentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { -- | The profile ID for a field-level encryption content type-profile
    -- mapping.
    profileId :: Core.Maybe Core.Text,
    -- | The format for a field-level encryption content type-profile mapping.
    format :: Format,
    -- | The content type for a field-level encryption content type-profile
    -- mapping.
    contentType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContentTypeProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'contentTypeProfile_profileId' - The profile ID for a field-level encryption content type-profile
-- mapping.
--
-- 'format', 'contentTypeProfile_format' - The format for a field-level encryption content type-profile mapping.
--
-- 'contentType', 'contentTypeProfile_contentType' - The content type for a field-level encryption content type-profile
-- mapping.
newContentTypeProfile ::
  -- | 'format'
  Format ->
  -- | 'contentType'
  Core.Text ->
  ContentTypeProfile
newContentTypeProfile pFormat_ pContentType_ =
  ContentTypeProfile'
    { profileId = Core.Nothing,
      format = pFormat_,
      contentType = pContentType_
    }

-- | The profile ID for a field-level encryption content type-profile
-- mapping.
contentTypeProfile_profileId :: Lens.Lens' ContentTypeProfile (Core.Maybe Core.Text)
contentTypeProfile_profileId = Lens.lens (\ContentTypeProfile' {profileId} -> profileId) (\s@ContentTypeProfile' {} a -> s {profileId = a} :: ContentTypeProfile)

-- | The format for a field-level encryption content type-profile mapping.
contentTypeProfile_format :: Lens.Lens' ContentTypeProfile Format
contentTypeProfile_format = Lens.lens (\ContentTypeProfile' {format} -> format) (\s@ContentTypeProfile' {} a -> s {format = a} :: ContentTypeProfile)

-- | The content type for a field-level encryption content type-profile
-- mapping.
contentTypeProfile_contentType :: Lens.Lens' ContentTypeProfile Core.Text
contentTypeProfile_contentType = Lens.lens (\ContentTypeProfile' {contentType} -> contentType) (\s@ContentTypeProfile' {} a -> s {contentType = a} :: ContentTypeProfile)

instance Core.FromXML ContentTypeProfile where
  parseXML x =
    ContentTypeProfile'
      Core.<$> (x Core..@? "ProfileId")
      Core.<*> (x Core..@ "Format")
      Core.<*> (x Core..@ "ContentType")

instance Core.Hashable ContentTypeProfile

instance Core.NFData ContentTypeProfile

instance Core.ToXML ContentTypeProfile where
  toXML ContentTypeProfile' {..} =
    Core.mconcat
      [ "ProfileId" Core.@= profileId,
        "Format" Core.@= format,
        "ContentType" Core.@= contentType
      ]
