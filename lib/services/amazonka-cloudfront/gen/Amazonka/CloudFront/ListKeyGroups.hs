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
-- Module      : Amazonka.CloudFront.ListKeyGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of key groups.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListKeyGroups
  ( -- * Creating a Request
    ListKeyGroups (..),
    newListKeyGroups,

    -- * Request Lenses
    listKeyGroups_marker,
    listKeyGroups_maxItems,

    -- * Destructuring the Response
    ListKeyGroupsResponse (..),
    newListKeyGroupsResponse,

    -- * Response Lenses
    listKeyGroupsResponse_keyGroupList,
    listKeyGroupsResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { -- | Use this field when paginating results to indicate where to begin in
    -- your list of key groups. The response includes key groups in the list
    -- that occur after the marker. To get the next page of the list, set this
    -- field\'s value to the value of @NextMarker@ from the current page\'s
    -- response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of key groups that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listKeyGroups_marker' - Use this field when paginating results to indicate where to begin in
-- your list of key groups. The response includes key groups in the list
-- that occur after the marker. To get the next page of the list, set this
-- field\'s value to the value of @NextMarker@ from the current page\'s
-- response.
--
-- 'maxItems', 'listKeyGroups_maxItems' - The maximum number of key groups that you want in the response.
newListKeyGroups ::
  ListKeyGroups
newListKeyGroups =
  ListKeyGroups'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in
-- your list of key groups. The response includes key groups in the list
-- that occur after the marker. To get the next page of the list, set this
-- field\'s value to the value of @NextMarker@ from the current page\'s
-- response.
listKeyGroups_marker :: Lens.Lens' ListKeyGroups (Prelude.Maybe Prelude.Text)
listKeyGroups_marker = Lens.lens (\ListKeyGroups' {marker} -> marker) (\s@ListKeyGroups' {} a -> s {marker = a} :: ListKeyGroups)

-- | The maximum number of key groups that you want in the response.
listKeyGroups_maxItems :: Lens.Lens' ListKeyGroups (Prelude.Maybe Prelude.Text)
listKeyGroups_maxItems = Lens.lens (\ListKeyGroups' {maxItems} -> maxItems) (\s@ListKeyGroups' {} a -> s {maxItems = a} :: ListKeyGroups)

instance Core.AWSRequest ListKeyGroups where
  type
    AWSResponse ListKeyGroups =
      ListKeyGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListKeyGroupsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyGroups where
  hashWithSalt _salt ListKeyGroups' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListKeyGroups where
  rnf ListKeyGroups' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems

instance Data.ToHeaders ListKeyGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListKeyGroups where
  toPath = Prelude.const "/2020-05-31/key-group"

instance Data.ToQuery ListKeyGroups where
  toQuery ListKeyGroups' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | /See:/ 'newListKeyGroupsResponse' smart constructor.
data ListKeyGroupsResponse = ListKeyGroupsResponse'
  { -- | A list of key groups.
    keyGroupList :: Prelude.Maybe KeyGroupList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyGroupList', 'listKeyGroupsResponse_keyGroupList' - A list of key groups.
--
-- 'httpStatus', 'listKeyGroupsResponse_httpStatus' - The response's http status code.
newListKeyGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeyGroupsResponse
newListKeyGroupsResponse pHttpStatus_ =
  ListKeyGroupsResponse'
    { keyGroupList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of key groups.
listKeyGroupsResponse_keyGroupList :: Lens.Lens' ListKeyGroupsResponse (Prelude.Maybe KeyGroupList)
listKeyGroupsResponse_keyGroupList = Lens.lens (\ListKeyGroupsResponse' {keyGroupList} -> keyGroupList) (\s@ListKeyGroupsResponse' {} a -> s {keyGroupList = a} :: ListKeyGroupsResponse)

-- | The response's http status code.
listKeyGroupsResponse_httpStatus :: Lens.Lens' ListKeyGroupsResponse Prelude.Int
listKeyGroupsResponse_httpStatus = Lens.lens (\ListKeyGroupsResponse' {httpStatus} -> httpStatus) (\s@ListKeyGroupsResponse' {} a -> s {httpStatus = a} :: ListKeyGroupsResponse)

instance Prelude.NFData ListKeyGroupsResponse where
  rnf ListKeyGroupsResponse' {..} =
    Prelude.rnf keyGroupList `Prelude.seq`
      Prelude.rnf httpStatus
