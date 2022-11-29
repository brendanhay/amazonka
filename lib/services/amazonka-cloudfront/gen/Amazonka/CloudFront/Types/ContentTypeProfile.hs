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
-- Module      : Amazonka.CloudFront.Types.ContentTypeProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContentTypeProfile where

import Amazonka.CloudFront.Types.Format
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A field-level encryption content type profile.
--
-- /See:/ 'newContentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { -- | The profile ID for a field-level encryption content type-profile
    -- mapping.
    profileId :: Prelude.Maybe Prelude.Text,
    -- | The format for a field-level encryption content type-profile mapping.
    format :: Format,
    -- | The content type for a field-level encryption content type-profile
    -- mapping.
    contentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ContentTypeProfile
newContentTypeProfile pFormat_ pContentType_ =
  ContentTypeProfile'
    { profileId = Prelude.Nothing,
      format = pFormat_,
      contentType = pContentType_
    }

-- | The profile ID for a field-level encryption content type-profile
-- mapping.
contentTypeProfile_profileId :: Lens.Lens' ContentTypeProfile (Prelude.Maybe Prelude.Text)
contentTypeProfile_profileId = Lens.lens (\ContentTypeProfile' {profileId} -> profileId) (\s@ContentTypeProfile' {} a -> s {profileId = a} :: ContentTypeProfile)

-- | The format for a field-level encryption content type-profile mapping.
contentTypeProfile_format :: Lens.Lens' ContentTypeProfile Format
contentTypeProfile_format = Lens.lens (\ContentTypeProfile' {format} -> format) (\s@ContentTypeProfile' {} a -> s {format = a} :: ContentTypeProfile)

-- | The content type for a field-level encryption content type-profile
-- mapping.
contentTypeProfile_contentType :: Lens.Lens' ContentTypeProfile Prelude.Text
contentTypeProfile_contentType = Lens.lens (\ContentTypeProfile' {contentType} -> contentType) (\s@ContentTypeProfile' {} a -> s {contentType = a} :: ContentTypeProfile)

instance Core.FromXML ContentTypeProfile where
  parseXML x =
    ContentTypeProfile'
      Prelude.<$> (x Core..@? "ProfileId")
      Prelude.<*> (x Core..@ "Format")
      Prelude.<*> (x Core..@ "ContentType")

instance Prelude.Hashable ContentTypeProfile where
  hashWithSalt _salt ContentTypeProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` contentType

instance Prelude.NFData ContentTypeProfile where
  rnf ContentTypeProfile' {..} =
    Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf contentType

instance Core.ToXML ContentTypeProfile where
  toXML ContentTypeProfile' {..} =
    Prelude.mconcat
      [ "ProfileId" Core.@= profileId,
        "Format" Core.@= format,
        "ContentType" Core.@= contentType
      ]
