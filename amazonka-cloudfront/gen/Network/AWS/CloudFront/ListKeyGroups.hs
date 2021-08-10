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
-- Module      : Network.AWS.CloudFront.ListKeyGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CloudFront.ListKeyGroups
  ( -- * Creating a Request
    ListKeyGroups (..),
    newListKeyGroups,

    -- * Request Lenses
    listKeyGroups_maxItems,
    listKeyGroups_marker,

    -- * Destructuring the Response
    ListKeyGroupsResponse (..),
    newListKeyGroupsResponse,

    -- * Response Lenses
    listKeyGroupsResponse_keyGroupList,
    listKeyGroupsResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { -- | The maximum number of key groups that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of key groups. The response includes key groups in the list
    -- that occur after the marker. To get the next page of the list, set this
    -- field’s value to the value of @NextMarker@ from the current page’s
    -- response.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'maxItems', 'listKeyGroups_maxItems' - The maximum number of key groups that you want in the response.
--
-- 'marker', 'listKeyGroups_marker' - Use this field when paginating results to indicate where to begin in
-- your list of key groups. The response includes key groups in the list
-- that occur after the marker. To get the next page of the list, set this
-- field’s value to the value of @NextMarker@ from the current page’s
-- response.
newListKeyGroups ::
  ListKeyGroups
newListKeyGroups =
  ListKeyGroups'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of key groups that you want in the response.
listKeyGroups_maxItems :: Lens.Lens' ListKeyGroups (Prelude.Maybe Prelude.Text)
listKeyGroups_maxItems = Lens.lens (\ListKeyGroups' {maxItems} -> maxItems) (\s@ListKeyGroups' {} a -> s {maxItems = a} :: ListKeyGroups)

-- | Use this field when paginating results to indicate where to begin in
-- your list of key groups. The response includes key groups in the list
-- that occur after the marker. To get the next page of the list, set this
-- field’s value to the value of @NextMarker@ from the current page’s
-- response.
listKeyGroups_marker :: Lens.Lens' ListKeyGroups (Prelude.Maybe Prelude.Text)
listKeyGroups_marker = Lens.lens (\ListKeyGroups' {marker} -> marker) (\s@ListKeyGroups' {} a -> s {marker = a} :: ListKeyGroups)

instance Core.AWSRequest ListKeyGroups where
  type
    AWSResponse ListKeyGroups =
      ListKeyGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListKeyGroupsResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyGroups

instance Prelude.NFData ListKeyGroups

instance Core.ToHeaders ListKeyGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListKeyGroups where
  toPath = Prelude.const "/2020-05-31/key-group"

instance Core.ToQuery ListKeyGroups where
  toQuery ListKeyGroups' {..} =
    Prelude.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
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

instance Prelude.NFData ListKeyGroupsResponse
