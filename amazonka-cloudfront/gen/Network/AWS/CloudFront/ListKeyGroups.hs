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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { -- | The maximum number of key groups that you want in the response.
    maxItems :: Core.Maybe Core.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of key groups. The response includes key groups in the list
    -- that occur after the marker. To get the next page of the list, set this
    -- field’s value to the value of @NextMarker@ from the current page’s
    -- response.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { maxItems = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of key groups that you want in the response.
listKeyGroups_maxItems :: Lens.Lens' ListKeyGroups (Core.Maybe Core.Text)
listKeyGroups_maxItems = Lens.lens (\ListKeyGroups' {maxItems} -> maxItems) (\s@ListKeyGroups' {} a -> s {maxItems = a} :: ListKeyGroups)

-- | Use this field when paginating results to indicate where to begin in
-- your list of key groups. The response includes key groups in the list
-- that occur after the marker. To get the next page of the list, set this
-- field’s value to the value of @NextMarker@ from the current page’s
-- response.
listKeyGroups_marker :: Lens.Lens' ListKeyGroups (Core.Maybe Core.Text)
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
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListKeyGroups

instance Core.NFData ListKeyGroups

instance Core.ToHeaders ListKeyGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListKeyGroups where
  toPath = Core.const "/2020-05-31/key-group"

instance Core.ToQuery ListKeyGroups where
  toQuery ListKeyGroups' {..} =
    Core.mconcat
      [ "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newListKeyGroupsResponse' smart constructor.
data ListKeyGroupsResponse = ListKeyGroupsResponse'
  { -- | A list of key groups.
    keyGroupList :: Core.Maybe KeyGroupList,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListKeyGroupsResponse
newListKeyGroupsResponse pHttpStatus_ =
  ListKeyGroupsResponse'
    { keyGroupList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of key groups.
listKeyGroupsResponse_keyGroupList :: Lens.Lens' ListKeyGroupsResponse (Core.Maybe KeyGroupList)
listKeyGroupsResponse_keyGroupList = Lens.lens (\ListKeyGroupsResponse' {keyGroupList} -> keyGroupList) (\s@ListKeyGroupsResponse' {} a -> s {keyGroupList = a} :: ListKeyGroupsResponse)

-- | The response's http status code.
listKeyGroupsResponse_httpStatus :: Lens.Lens' ListKeyGroupsResponse Core.Int
listKeyGroupsResponse_httpStatus = Lens.lens (\ListKeyGroupsResponse' {httpStatus} -> httpStatus) (\s@ListKeyGroupsResponse' {} a -> s {httpStatus = a} :: ListKeyGroupsResponse)

instance Core.NFData ListKeyGroupsResponse
