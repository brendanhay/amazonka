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
-- Module      : Amazonka.CloudFront.ListFieldLevelEncryptionConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all field-level encryption configurations that have been created in
-- CloudFront for this account.
module Amazonka.CloudFront.ListFieldLevelEncryptionConfigs
  ( -- * Creating a Request
    ListFieldLevelEncryptionConfigs (..),
    newListFieldLevelEncryptionConfigs,

    -- * Request Lenses
    listFieldLevelEncryptionConfigs_marker,
    listFieldLevelEncryptionConfigs_maxItems,

    -- * Destructuring the Response
    ListFieldLevelEncryptionConfigsResponse (..),
    newListFieldLevelEncryptionConfigsResponse,

    -- * Response Lenses
    listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList,
    listFieldLevelEncryptionConfigsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { -- | Use this when paginating results to indicate where to begin in your list
    -- of configurations. The results include configurations in the list that
    -- occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last configuration on that page).
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of field-level encryption configurations you want in
    -- the response body.
    maxItems :: Prelude.Maybe Prelude.Text
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
-- 'marker', 'listFieldLevelEncryptionConfigs_marker' - Use this when paginating results to indicate where to begin in your list
-- of configurations. The results include configurations in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last configuration on that page).
--
-- 'maxItems', 'listFieldLevelEncryptionConfigs_maxItems' - The maximum number of field-level encryption configurations you want in
-- the response body.
newListFieldLevelEncryptionConfigs ::
  ListFieldLevelEncryptionConfigs
newListFieldLevelEncryptionConfigs =
  ListFieldLevelEncryptionConfigs'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of configurations. The results include configurations in the list that
-- occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last configuration on that page).
listFieldLevelEncryptionConfigs_marker :: Lens.Lens' ListFieldLevelEncryptionConfigs (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionConfigs_marker = Lens.lens (\ListFieldLevelEncryptionConfigs' {marker} -> marker) (\s@ListFieldLevelEncryptionConfigs' {} a -> s {marker = a} :: ListFieldLevelEncryptionConfigs)

-- | The maximum number of field-level encryption configurations you want in
-- the response body.
listFieldLevelEncryptionConfigs_maxItems :: Lens.Lens' ListFieldLevelEncryptionConfigs (Prelude.Maybe Prelude.Text)
listFieldLevelEncryptionConfigs_maxItems = Lens.lens (\ListFieldLevelEncryptionConfigs' {maxItems} -> maxItems) (\s@ListFieldLevelEncryptionConfigs' {} a -> s {maxItems = a} :: ListFieldLevelEncryptionConfigs)

instance
  Core.AWSRequest
    ListFieldLevelEncryptionConfigs
  where
  type
    AWSResponse ListFieldLevelEncryptionConfigs =
      ListFieldLevelEncryptionConfigsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListFieldLevelEncryptionConfigsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFieldLevelEncryptionConfigs
  where
  hashWithSalt
    _salt
    ListFieldLevelEncryptionConfigs' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems

instance
  Prelude.NFData
    ListFieldLevelEncryptionConfigs
  where
  rnf ListFieldLevelEncryptionConfigs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems

instance
  Data.ToHeaders
    ListFieldLevelEncryptionConfigs
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListFieldLevelEncryptionConfigs where
  toPath =
    Prelude.const "/2020-05-31/field-level-encryption"

instance Data.ToQuery ListFieldLevelEncryptionConfigs where
  toQuery ListFieldLevelEncryptionConfigs' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
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
  where
  rnf ListFieldLevelEncryptionConfigsResponse' {..} =
    Prelude.rnf fieldLevelEncryptionList
      `Prelude.seq` Prelude.rnf httpStatus
