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
-- Module      : Amazonka.CloudFront.ListFieldLevelEncryptionProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of field-level encryption profiles that have been created
-- in CloudFront for this account.
module Amazonka.CloudFront.ListFieldLevelEncryptionProfiles
  ( -- * Creating a Request
    ListFieldLevelEncryptionProfiles (..),
    newListFieldLevelEncryptionProfiles,

    -- * Request Lenses
    listFieldLevelEncryptionProfiles_marker,
    listFieldLevelEncryptionProfiles_maxItems,

    -- * Destructuring the Response
    ListFieldLevelEncryptionProfilesResponse (..),
    newListFieldLevelEncryptionProfilesResponse,

    -- * Response Lenses
    listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList,
    listFieldLevelEncryptionProfilesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFieldLevelEncryptionProfiles' smart constructor.
data ListFieldLevelEncryptionProfiles = ListFieldLevelEncryptionProfiles'
  { -- | Use this when paginating results to indicate where to begin in your list
    -- of profiles. The results include profiles in the list that occur after
    -- the marker. To get the next page of results, set the @Marker@ to the
    -- value of the @NextMarker@ from the current page\'s response (which is
    -- also the ID of the last profile on that page).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of field-level encryption profiles you want in the
    -- response body.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldLevelEncryptionProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listFieldLevelEncryptionProfiles_marker' - Use this when paginating results to indicate where to begin in your list
-- of profiles. The results include profiles in the list that occur after
-- the marker. To get the next page of results, set the @Marker@ to the
-- value of the @NextMarker@ from the current page\'s response (which is
-- also the ID of the last profile on that page).
--
-- 'maxItems', 'listFieldLevelEncryptionProfiles_maxItems' - The maximum number of field-level encryption profiles you want in the
-- response body.
newListFieldLevelEncryptionProfiles ::
  ListFieldLevelEncryptionProfiles
newListFieldLevelEncryptionProfiles =
  ListFieldLevelEncryptionProfiles'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of profiles. The results include profiles in the list that occur after
-- the marker. To get the next page of results, set the @Marker@ to the
-- value of the @NextMarker@ from the current page\'s response (which is
-- also the ID of the last profile on that page).
listFieldLevelEncryptionProfiles_marker :: Lens.Lens' ListFieldLevelEncryptionProfiles (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionProfiles_marker = Lens.lens (\ListFieldLevelEncryptionProfiles' {marker} -> marker) (\s@ListFieldLevelEncryptionProfiles' {} a -> s {marker = a} :: ListFieldLevelEncryptionProfiles)

-- | The maximum number of field-level encryption profiles you want in the
-- response body.
listFieldLevelEncryptionProfiles_maxItems :: Lens.Lens' ListFieldLevelEncryptionProfiles (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionProfiles_maxItems = Lens.lens (\ListFieldLevelEncryptionProfiles' {maxItems} -> maxItems) (\s@ListFieldLevelEncryptionProfiles' {} a -> s {maxItems = a} :: ListFieldLevelEncryptionProfiles)

instance
  Core.AWSRequest
    ListFieldLevelEncryptionProfiles
  where
  type
    AWSResponse ListFieldLevelEncryptionProfiles =
      ListFieldLevelEncryptionProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionProfilesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFieldLevelEncryptionProfiles
  where
  hashWithSalt
    _salt
    ListFieldLevelEncryptionProfiles' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems

instance
  Prelude.NFData
    ListFieldLevelEncryptionProfiles
  where
  rnf ListFieldLevelEncryptionProfiles' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance
  Data.ToHeaders
    ListFieldLevelEncryptionProfiles
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFieldLevelEncryptionProfiles where
  toPath =
    Prelude.const
      "/2020-05-31/field-level-encryption-profile"

instance
  Data.ToQuery
    ListFieldLevelEncryptionProfiles
  where
  toQuery ListFieldLevelEncryptionProfiles' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListFieldLevelEncryptionProfilesResponse' smart constructor.
data ListFieldLevelEncryptionProfilesResponse = ListFieldLevelEncryptionProfilesResponse'
  { -- | Returns a list of the field-level encryption profiles that have been
    -- created in CloudFront for this account.
    fieldLevelEncryptionProfileList :: Prelude.Maybe FieldLevelEncryptionProfileList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf ListFieldLevelEncryptionProfilesResponse' {..} =
    Prelude.rnf fieldLevelEncryptionProfileList
      `Prelude.seq` Prelude.rnf httpStatus
