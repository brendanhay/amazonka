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
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all field-level encryption configurations that have been created in
-- CloudFront for this account.
module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
  ( -- * Creating a Request
    ListFieldLevelEncryptionConfigs (..),
    newListFieldLevelEncryptionConfigs,

    -- * Request Lenses
    listFieldLevelEncryptionConfigs_maxItems,
    listFieldLevelEncryptionConfigs_marker,

    -- * Destructuring the Response
    ListFieldLevelEncryptionConfigsResponse (..),
    newListFieldLevelEncryptionConfigsResponse,

    -- * Response Lenses
    listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList,
    listFieldLevelEncryptionConfigsResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { -- | The maximum number of field-level encryption configurations you want in
    -- the response body.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this when paginating results to indicate where to begin in your list
    -- of configurations. The results include configurations in the list that
    -- occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last configuration on that page).
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldLevelEncryptionConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listFieldLevelEncryptionConfigs_maxItems' - The maximum number of field-level encryption configurations you want in
-- the response body.
--
-- 'marker', 'listFieldLevelEncryptionConfigs_marker' - Use this when paginating results to indicate where to begin in your list
-- of configurations. The results include configurations in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last configuration on that page).
newListFieldLevelEncryptionConfigs ::
  ListFieldLevelEncryptionConfigs
newListFieldLevelEncryptionConfigs =
  ListFieldLevelEncryptionConfigs'
    { maxItems =
        Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of field-level encryption configurations you want in
-- the response body.
listFieldLevelEncryptionConfigs_maxItems :: Lens.Lens' ListFieldLevelEncryptionConfigs (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionConfigs_maxItems = Lens.lens (\ListFieldLevelEncryptionConfigs' {maxItems} -> maxItems) (\s@ListFieldLevelEncryptionConfigs' {} a -> s {maxItems = a} :: ListFieldLevelEncryptionConfigs)

-- | Use this when paginating results to indicate where to begin in your list
-- of configurations. The results include configurations in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last configuration on that page).
listFieldLevelEncryptionConfigs_marker :: Lens.Lens' ListFieldLevelEncryptionConfigs (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionConfigs_marker = Lens.lens (\ListFieldLevelEncryptionConfigs' {marker} -> marker) (\s@ListFieldLevelEncryptionConfigs' {} a -> s {marker = a} :: ListFieldLevelEncryptionConfigs)

instance
  Core.AWSRequest
    ListFieldLevelEncryptionConfigs
  where
  type
    AWSResponse ListFieldLevelEncryptionConfigs =
      ListFieldLevelEncryptionConfigsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionConfigsResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFieldLevelEncryptionConfigs

instance
  Prelude.NFData
    ListFieldLevelEncryptionConfigs

instance
  Core.ToHeaders
    ListFieldLevelEncryptionConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListFieldLevelEncryptionConfigs where
  toPath =
    Prelude.const "/2020-05-31/field-level-encryption"

instance Core.ToQuery ListFieldLevelEncryptionConfigs where
  toQuery ListFieldLevelEncryptionConfigs' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListFieldLevelEncryptionConfigsResponse' smart constructor.
data ListFieldLevelEncryptionConfigsResponse = ListFieldLevelEncryptionConfigsResponse'
  { -- | Returns a list of all field-level encryption configurations that have
    -- been created in CloudFront for this account.
    fieldLevelEncryptionList :: Prelude.Maybe FieldLevelEncryptionList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFieldLevelEncryptionConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionList', 'listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList' - Returns a list of all field-level encryption configurations that have
-- been created in CloudFront for this account.
--
-- 'httpStatus', 'listFieldLevelEncryptionConfigsResponse_httpStatus' - The response's http status code.
newListFieldLevelEncryptionConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFieldLevelEncryptionConfigsResponse
newListFieldLevelEncryptionConfigsResponse
  pHttpStatus_ =
    ListFieldLevelEncryptionConfigsResponse'
      { fieldLevelEncryptionList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of all field-level encryption configurations that have
-- been created in CloudFront for this account.
listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse (Prelude.Maybe FieldLevelEncryptionList)
listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList = Lens.lens (\ListFieldLevelEncryptionConfigsResponse' {fieldLevelEncryptionList} -> fieldLevelEncryptionList) (\s@ListFieldLevelEncryptionConfigsResponse' {} a -> s {fieldLevelEncryptionList = a} :: ListFieldLevelEncryptionConfigsResponse)

-- | The response's http status code.
listFieldLevelEncryptionConfigsResponse_httpStatus :: Lens.Lens' ListFieldLevelEncryptionConfigsResponse Prelude.Int
listFieldLevelEncryptionConfigsResponse_httpStatus = Lens.lens (\ListFieldLevelEncryptionConfigsResponse' {httpStatus} -> httpStatus) (\s@ListFieldLevelEncryptionConfigsResponse' {} a -> s {httpStatus = a} :: ListFieldLevelEncryptionConfigsResponse)

instance
  Prelude.NFData
    ListFieldLevelEncryptionConfigsResponse
