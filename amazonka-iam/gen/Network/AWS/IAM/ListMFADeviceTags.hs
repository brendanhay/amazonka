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
-- Module      : Network.AWS.IAM.ListMFADeviceTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified IAM virtual
-- multi-factor authentication (MFA) device. The returned list of tags is
-- sorted by tag key. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ListMFADeviceTags
  ( -- * Creating a Request
    ListMFADeviceTags (..),
    newListMFADeviceTags,

    -- * Request Lenses
    listMFADeviceTags_maxItems,
    listMFADeviceTags_marker,
    listMFADeviceTags_serialNumber,

    -- * Destructuring the Response
    ListMFADeviceTagsResponse (..),
    newListMFADeviceTagsResponse,

    -- * Response Lenses
    listMFADeviceTagsResponse_isTruncated,
    listMFADeviceTagsResponse_marker,
    listMFADeviceTagsResponse_httpStatus,
    listMFADeviceTagsResponse_tags,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMFADeviceTags' smart constructor.
data ListMFADeviceTags = ListMFADeviceTags'
  { -- | (Optional) Use this only when paginating results to indicate the maximum
    -- number of items that you want in the response. If additional items exist
    -- beyond the maximum that you specify, the @IsTruncated@ response element
    -- is @true@.
    --
    -- If you do not include this parameter, it defaults to 100. Note that IAM
    -- might return fewer results, even when more results are available. In
    -- that case, the @IsTruncated@ response element returns @true@, and
    -- @Marker@ contains a value to include in the subsequent call that tells
    -- the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the IAM virtual MFA device whose tags you want
    -- to see. For virtual MFA devices, the serial number is the same as the
    -- ARN.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    serialNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMFADeviceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listMFADeviceTags_maxItems' - (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
--
-- 'marker', 'listMFADeviceTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'serialNumber', 'listMFADeviceTags_serialNumber' - The unique identifier for the IAM virtual MFA device whose tags you want
-- to see. For virtual MFA devices, the serial number is the same as the
-- ARN.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
newListMFADeviceTags ::
  -- | 'serialNumber'
  Prelude.Text ->
  ListMFADeviceTags
newListMFADeviceTags pSerialNumber_ =
  ListMFADeviceTags'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      serialNumber = pSerialNumber_
    }

-- | (Optional) Use this only when paginating results to indicate the maximum
-- number of items that you want in the response. If additional items exist
-- beyond the maximum that you specify, the @IsTruncated@ response element
-- is @true@.
--
-- If you do not include this parameter, it defaults to 100. Note that IAM
-- might return fewer results, even when more results are available. In
-- that case, the @IsTruncated@ response element returns @true@, and
-- @Marker@ contains a value to include in the subsequent call that tells
-- the service where to continue from.
listMFADeviceTags_maxItems :: Lens.Lens' ListMFADeviceTags (Prelude.Maybe Prelude.Natural)
listMFADeviceTags_maxItems = Lens.lens (\ListMFADeviceTags' {maxItems} -> maxItems) (\s@ListMFADeviceTags' {} a -> s {maxItems = a} :: ListMFADeviceTags)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listMFADeviceTags_marker :: Lens.Lens' ListMFADeviceTags (Prelude.Maybe Prelude.Text)
listMFADeviceTags_marker = Lens.lens (\ListMFADeviceTags' {marker} -> marker) (\s@ListMFADeviceTags' {} a -> s {marker = a} :: ListMFADeviceTags)

-- | The unique identifier for the IAM virtual MFA device whose tags you want
-- to see. For virtual MFA devices, the serial number is the same as the
-- ARN.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
listMFADeviceTags_serialNumber :: Lens.Lens' ListMFADeviceTags Prelude.Text
listMFADeviceTags_serialNumber = Lens.lens (\ListMFADeviceTags' {serialNumber} -> serialNumber) (\s@ListMFADeviceTags' {} a -> s {serialNumber = a} :: ListMFADeviceTags)

instance Prelude.AWSRequest ListMFADeviceTags where
  type Rs ListMFADeviceTags = ListMFADeviceTagsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListMFADeviceTagsResult"
      ( \s h x ->
          ListMFADeviceTagsResponse'
            Prelude.<$> (x Prelude..@? "IsTruncated")
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListMFADeviceTags

instance Prelude.NFData ListMFADeviceTags

instance Prelude.ToHeaders ListMFADeviceTags where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListMFADeviceTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListMFADeviceTags where
  toQuery ListMFADeviceTags' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListMFADeviceTags" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker,
        "SerialNumber" Prelude.=: serialNumber
      ]

-- | /See:/ 'newListMFADeviceTagsResponse' smart constructor.
data ListMFADeviceTagsResponse = ListMFADeviceTagsResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can use the @Marker@ request parameter to
    -- make a subsequent pagination request that retrieves more items. Note
    -- that IAM might return fewer than the @MaxItems@ number of results even
    -- when more results are available. Check @IsTruncated@ after every call to
    -- ensure that you receive all of your results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of tags that are currently attached to the virtual MFA device.
    -- Each tag consists of a key name and an associated value. If no tags are
    -- attached to the specified resource, the response contains an empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMFADeviceTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listMFADeviceTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
--
-- 'marker', 'listMFADeviceTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listMFADeviceTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listMFADeviceTagsResponse_tags' - The list of tags that are currently attached to the virtual MFA device.
-- Each tag consists of a key name and an associated value. If no tags are
-- attached to the specified resource, the response contains an empty list.
newListMFADeviceTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMFADeviceTagsResponse
newListMFADeviceTagsResponse pHttpStatus_ =
  ListMFADeviceTagsResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can use the @Marker@ request parameter to
-- make a subsequent pagination request that retrieves more items. Note
-- that IAM might return fewer than the @MaxItems@ number of results even
-- when more results are available. Check @IsTruncated@ after every call to
-- ensure that you receive all of your results.
listMFADeviceTagsResponse_isTruncated :: Lens.Lens' ListMFADeviceTagsResponse (Prelude.Maybe Prelude.Bool)
listMFADeviceTagsResponse_isTruncated = Lens.lens (\ListMFADeviceTagsResponse' {isTruncated} -> isTruncated) (\s@ListMFADeviceTagsResponse' {} a -> s {isTruncated = a} :: ListMFADeviceTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listMFADeviceTagsResponse_marker :: Lens.Lens' ListMFADeviceTagsResponse (Prelude.Maybe Prelude.Text)
listMFADeviceTagsResponse_marker = Lens.lens (\ListMFADeviceTagsResponse' {marker} -> marker) (\s@ListMFADeviceTagsResponse' {} a -> s {marker = a} :: ListMFADeviceTagsResponse)

-- | The response's http status code.
listMFADeviceTagsResponse_httpStatus :: Lens.Lens' ListMFADeviceTagsResponse Prelude.Int
listMFADeviceTagsResponse_httpStatus = Lens.lens (\ListMFADeviceTagsResponse' {httpStatus} -> httpStatus) (\s@ListMFADeviceTagsResponse' {} a -> s {httpStatus = a} :: ListMFADeviceTagsResponse)

-- | The list of tags that are currently attached to the virtual MFA device.
-- Each tag consists of a key name and an associated value. If no tags are
-- attached to the specified resource, the response contains an empty list.
listMFADeviceTagsResponse_tags :: Lens.Lens' ListMFADeviceTagsResponse [Tag]
listMFADeviceTagsResponse_tags = Lens.lens (\ListMFADeviceTagsResponse' {tags} -> tags) (\s@ListMFADeviceTagsResponse' {} a -> s {tags = a} :: ListMFADeviceTagsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListMFADeviceTagsResponse
