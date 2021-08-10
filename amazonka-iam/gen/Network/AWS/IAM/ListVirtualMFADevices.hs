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
-- Module      : Network.AWS.IAM.ListVirtualMFADevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual MFA devices defined in the AWS account by assignment
-- status. If you do not specify an assignment status, the operation
-- returns a list of all virtual MFA devices. Assignment status can be
-- @Assigned@, @Unassigned@, or @Any@.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for a virtual MFA device, see
-- ListVirtualMFADevices.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListVirtualMFADevices
  ( -- * Creating a Request
    ListVirtualMFADevices (..),
    newListVirtualMFADevices,

    -- * Request Lenses
    listVirtualMFADevices_assignmentStatus,
    listVirtualMFADevices_maxItems,
    listVirtualMFADevices_marker,

    -- * Destructuring the Response
    ListVirtualMFADevicesResponse (..),
    newListVirtualMFADevicesResponse,

    -- * Response Lenses
    listVirtualMFADevicesResponse_isTruncated,
    listVirtualMFADevicesResponse_marker,
    listVirtualMFADevicesResponse_httpStatus,
    listVirtualMFADevicesResponse_virtualMFADevices,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListVirtualMFADevices' smart constructor.
data ListVirtualMFADevices = ListVirtualMFADevices'
  { -- | The status (@Unassigned@ or @Assigned@) of the devices to list. If you
    -- do not specify an @AssignmentStatus@, the operation defaults to @Any@,
    -- which lists both assigned and unassigned virtual MFA devices.,
    assignmentStatus :: Prelude.Maybe AssignmentStatusType,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualMFADevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentStatus', 'listVirtualMFADevices_assignmentStatus' - The status (@Unassigned@ or @Assigned@) of the devices to list. If you
-- do not specify an @AssignmentStatus@, the operation defaults to @Any@,
-- which lists both assigned and unassigned virtual MFA devices.,
--
-- 'maxItems', 'listVirtualMFADevices_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listVirtualMFADevices_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListVirtualMFADevices ::
  ListVirtualMFADevices
newListVirtualMFADevices =
  ListVirtualMFADevices'
    { assignmentStatus =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The status (@Unassigned@ or @Assigned@) of the devices to list. If you
-- do not specify an @AssignmentStatus@, the operation defaults to @Any@,
-- which lists both assigned and unassigned virtual MFA devices.,
listVirtualMFADevices_assignmentStatus :: Lens.Lens' ListVirtualMFADevices (Prelude.Maybe AssignmentStatusType)
listVirtualMFADevices_assignmentStatus = Lens.lens (\ListVirtualMFADevices' {assignmentStatus} -> assignmentStatus) (\s@ListVirtualMFADevices' {} a -> s {assignmentStatus = a} :: ListVirtualMFADevices)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listVirtualMFADevices_maxItems :: Lens.Lens' ListVirtualMFADevices (Prelude.Maybe Prelude.Natural)
listVirtualMFADevices_maxItems = Lens.lens (\ListVirtualMFADevices' {maxItems} -> maxItems) (\s@ListVirtualMFADevices' {} a -> s {maxItems = a} :: ListVirtualMFADevices)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listVirtualMFADevices_marker :: Lens.Lens' ListVirtualMFADevices (Prelude.Maybe Prelude.Text)
listVirtualMFADevices_marker = Lens.lens (\ListVirtualMFADevices' {marker} -> marker) (\s@ListVirtualMFADevices' {} a -> s {marker = a} :: ListVirtualMFADevices)

instance Core.AWSPager ListVirtualMFADevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualMFADevicesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listVirtualMFADevicesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVirtualMFADevices_marker
          Lens..~ rs
          Lens.^? listVirtualMFADevicesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualMFADevices where
  type
    AWSResponse ListVirtualMFADevices =
      ListVirtualMFADevicesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListVirtualMFADevicesResult"
      ( \s h x ->
          ListVirtualMFADevicesResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "VirtualMFADevices"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListVirtualMFADevices

instance Prelude.NFData ListVirtualMFADevices

instance Core.ToHeaders ListVirtualMFADevices where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListVirtualMFADevices where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVirtualMFADevices where
  toQuery ListVirtualMFADevices' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListVirtualMFADevices" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "AssignmentStatus" Core.=: assignmentStatus,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListVirtualMFADevices request.
--
-- /See:/ 'newListVirtualMFADevicesResponse' smart constructor.
data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of virtual MFA devices in the current account that match the
    -- @AssignmentStatus@ value that was passed in the request.
    virtualMFADevices :: [VirtualMFADevice]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualMFADevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listVirtualMFADevicesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listVirtualMFADevicesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listVirtualMFADevicesResponse_httpStatus' - The response's http status code.
--
-- 'virtualMFADevices', 'listVirtualMFADevicesResponse_virtualMFADevices' - The list of virtual MFA devices in the current account that match the
-- @AssignmentStatus@ value that was passed in the request.
newListVirtualMFADevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualMFADevicesResponse
newListVirtualMFADevicesResponse pHttpStatus_ =
  ListVirtualMFADevicesResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      virtualMFADevices = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listVirtualMFADevicesResponse_isTruncated :: Lens.Lens' ListVirtualMFADevicesResponse (Prelude.Maybe Prelude.Bool)
listVirtualMFADevicesResponse_isTruncated = Lens.lens (\ListVirtualMFADevicesResponse' {isTruncated} -> isTruncated) (\s@ListVirtualMFADevicesResponse' {} a -> s {isTruncated = a} :: ListVirtualMFADevicesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listVirtualMFADevicesResponse_marker :: Lens.Lens' ListVirtualMFADevicesResponse (Prelude.Maybe Prelude.Text)
listVirtualMFADevicesResponse_marker = Lens.lens (\ListVirtualMFADevicesResponse' {marker} -> marker) (\s@ListVirtualMFADevicesResponse' {} a -> s {marker = a} :: ListVirtualMFADevicesResponse)

-- | The response's http status code.
listVirtualMFADevicesResponse_httpStatus :: Lens.Lens' ListVirtualMFADevicesResponse Prelude.Int
listVirtualMFADevicesResponse_httpStatus = Lens.lens (\ListVirtualMFADevicesResponse' {httpStatus} -> httpStatus) (\s@ListVirtualMFADevicesResponse' {} a -> s {httpStatus = a} :: ListVirtualMFADevicesResponse)

-- | The list of virtual MFA devices in the current account that match the
-- @AssignmentStatus@ value that was passed in the request.
listVirtualMFADevicesResponse_virtualMFADevices :: Lens.Lens' ListVirtualMFADevicesResponse [VirtualMFADevice]
listVirtualMFADevicesResponse_virtualMFADevices = Lens.lens (\ListVirtualMFADevicesResponse' {virtualMFADevices} -> virtualMFADevices) (\s@ListVirtualMFADevicesResponse' {} a -> s {virtualMFADevices = a} :: ListVirtualMFADevicesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListVirtualMFADevicesResponse
