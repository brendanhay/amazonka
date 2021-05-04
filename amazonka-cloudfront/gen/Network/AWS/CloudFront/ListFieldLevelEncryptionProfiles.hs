{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of field-level encryption profiles that have been created
-- in CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
  ( -- * Creating a Request
    ListFieldLevelEncryptionProfiles (..),
    newListFieldLevelEncryptionProfiles,

    -- * Request Lenses
    listFieldLevelEncryptionProfiles_maxItems,
    listFieldLevelEncryptionProfiles_marker,

    -- * Destructuring the Response
    ListFieldLevelEncryptionProfilesResponse (..),
    newListFieldLevelEncryptionProfilesResponse,

    -- * Response Lenses
    listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList,
    listFieldLevelEncryptionProfilesResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFieldLevelEncryptionProfiles' smart constructor.
data ListFieldLevelEncryptionProfiles = ListFieldLevelEncryptionProfiles'
  { -- | The maximum number of field-level encryption profiles you want in the
    -- response body.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this when paginating results to indicate where to begin in your list
    -- of profiles. The results include profiles in the list that occur after
    -- the marker. To get the next page of results, set the @Marker@ to the
    -- value of the @NextMarker@ from the current page\'s response (which is
    -- also the ID of the last profile on that page).
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFieldLevelEncryptionProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listFieldLevelEncryptionProfiles_maxItems' - The maximum number of field-level encryption profiles you want in the
-- response body.
--
-- 'marker', 'listFieldLevelEncryptionProfiles_marker' - Use this when paginating results to indicate where to begin in your list
-- of profiles. The results include profiles in the list that occur after
-- the marker. To get the next page of results, set the @Marker@ to the
-- value of the @NextMarker@ from the current page\'s response (which is
-- also the ID of the last profile on that page).
newListFieldLevelEncryptionProfiles ::
  ListFieldLevelEncryptionProfiles
newListFieldLevelEncryptionProfiles =
  ListFieldLevelEncryptionProfiles'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of field-level encryption profiles you want in the
-- response body.
listFieldLevelEncryptionProfiles_maxItems :: Lens.Lens' ListFieldLevelEncryptionProfiles (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionProfiles_maxItems = Lens.lens (\ListFieldLevelEncryptionProfiles' {maxItems} -> maxItems) (\s@ListFieldLevelEncryptionProfiles' {} a -> s {maxItems = a} :: ListFieldLevelEncryptionProfiles)

-- | Use this when paginating results to indicate where to begin in your list
-- of profiles. The results include profiles in the list that occur after
-- the marker. To get the next page of results, set the @Marker@ to the
-- value of the @NextMarker@ from the current page\'s response (which is
-- also the ID of the last profile on that page).
listFieldLevelEncryptionProfiles_marker :: Lens.Lens' ListFieldLevelEncryptionProfiles (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionProfiles_marker = Lens.lens (\ListFieldLevelEncryptionProfiles' {marker} -> marker) (\s@ListFieldLevelEncryptionProfiles' {} a -> s {marker = a} :: ListFieldLevelEncryptionProfiles)

instance
  Prelude.AWSRequest
    ListFieldLevelEncryptionProfiles
  where
  type
    Rs ListFieldLevelEncryptionProfiles =
      ListFieldLevelEncryptionProfilesResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionProfilesResponse'
            Prelude.<$> (Prelude.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFieldLevelEncryptionProfiles

instance
  Prelude.NFData
    ListFieldLevelEncryptionProfiles

instance
  Prelude.ToHeaders
    ListFieldLevelEncryptionProfiles
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ListFieldLevelEncryptionProfiles
  where
  toPath =
    Prelude.const
      "/2020-05-31/field-level-encryption-profile"

instance
  Prelude.ToQuery
    ListFieldLevelEncryptionProfiles
  where
  toQuery ListFieldLevelEncryptionProfiles' {..} =
    Prelude.mconcat
      [ "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker
      ]

-- | /See:/ 'newListFieldLevelEncryptionProfilesResponse' smart constructor.
data ListFieldLevelEncryptionProfilesResponse = ListFieldLevelEncryptionProfilesResponse'
  { -- | Returns a list of the field-level encryption profiles that have been
    -- created in CloudFront for this account.
    fieldLevelEncryptionProfileList :: Prelude.Maybe FieldLevelEncryptionProfileList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListFieldLevelEncryptionProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionProfileList', 'listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList' - Returns a list of the field-level encryption profiles that have been
-- created in CloudFront for this account.
--
-- 'httpStatus', 'listFieldLevelEncryptionProfilesResponse_httpStatus' - The response's http status code.
newListFieldLevelEncryptionProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFieldLevelEncryptionProfilesResponse
newListFieldLevelEncryptionProfilesResponse
  pHttpStatus_ =
    ListFieldLevelEncryptionProfilesResponse'
      { fieldLevelEncryptionProfileList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of the field-level encryption profiles that have been
-- created in CloudFront for this account.
listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse (Prelude.Maybe FieldLevelEncryptionProfileList)
listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList = Lens.lens (\ListFieldLevelEncryptionProfilesResponse' {fieldLevelEncryptionProfileList} -> fieldLevelEncryptionProfileList) (\s@ListFieldLevelEncryptionProfilesResponse' {} a -> s {fieldLevelEncryptionProfileList = a} :: ListFieldLevelEncryptionProfilesResponse)

-- | The response's http status code.
listFieldLevelEncryptionProfilesResponse_httpStatus :: Lens.Lens' ListFieldLevelEncryptionProfilesResponse Prelude.Int
listFieldLevelEncryptionProfilesResponse_httpStatus = Lens.lens (\ListFieldLevelEncryptionProfilesResponse' {httpStatus} -> httpStatus) (\s@ListFieldLevelEncryptionProfilesResponse' {} a -> s {httpStatus = a} :: ListFieldLevelEncryptionProfilesResponse)

instance
  Prelude.NFData
    ListFieldLevelEncryptionProfilesResponse
