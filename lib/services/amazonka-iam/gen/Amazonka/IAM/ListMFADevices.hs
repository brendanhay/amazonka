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
-- Module      : Amazonka.IAM.ListMFADevices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the MFA devices for an IAM user. If the request includes a IAM
-- user name, then this operation lists all the MFA devices associated with
-- the specified user. If you do not specify a user name, IAM determines
-- the user name implicitly based on the Amazon Web Services access key ID
-- signing the request for this operation.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListMFADevices
  ( -- * Creating a Request
    ListMFADevices (..),
    newListMFADevices,

    -- * Request Lenses
    listMFADevices_marker,
    listMFADevices_maxItems,
    listMFADevices_userName,

    -- * Destructuring the Response
    ListMFADevicesResponse (..),
    newListMFADevicesResponse,

    -- * Response Lenses
    listMFADevicesResponse_isTruncated,
    listMFADevicesResponse_marker,
    listMFADevicesResponse_httpStatus,
    listMFADevicesResponse_mfaDevices,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMFADevices' smart constructor.
data ListMFADevices = ListMFADevices'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
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
    -- | The name of the user whose MFA devices you want to list.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMFADevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listMFADevices_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listMFADevices_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'userName', 'listMFADevices_userName' - The name of the user whose MFA devices you want to list.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListMFADevices ::
  ListMFADevices
newListMFADevices =
  ListMFADevices'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listMFADevices_marker :: Lens.Lens' ListMFADevices (Prelude.Maybe Prelude.Text)
listMFADevices_marker = Lens.lens (\ListMFADevices' {marker} -> marker) (\s@ListMFADevices' {} a -> s {marker = a} :: ListMFADevices)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listMFADevices_maxItems :: Lens.Lens' ListMFADevices (Prelude.Maybe Prelude.Natural)
listMFADevices_maxItems = Lens.lens (\ListMFADevices' {maxItems} -> maxItems) (\s@ListMFADevices' {} a -> s {maxItems = a} :: ListMFADevices)

-- | The name of the user whose MFA devices you want to list.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listMFADevices_userName :: Lens.Lens' ListMFADevices (Prelude.Maybe Prelude.Text)
listMFADevices_userName = Lens.lens (\ListMFADevices' {userName} -> userName) (\s@ListMFADevices' {} a -> s {userName = a} :: ListMFADevices)

instance Core.AWSPager ListMFADevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMFADevicesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listMFADevicesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMFADevices_marker
          Lens..~ rs
          Lens.^? listMFADevicesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListMFADevices where
  type
    AWSResponse ListMFADevices =
      ListMFADevicesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListMFADevicesResult"
      ( \s h x ->
          ListMFADevicesResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "MFADevices" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListMFADevices where
  hashWithSalt _salt ListMFADevices' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListMFADevices where
  rnf ListMFADevices' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ListMFADevices where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMFADevices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMFADevices where
  toQuery ListMFADevices' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListMFADevices" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "UserName" Data.=: userName
      ]

-- | Contains the response to a successful ListMFADevices request.
--
-- /See:/ 'newListMFADevicesResponse' smart constructor.
data ListMFADevicesResponse = ListMFADevicesResponse'
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
    -- | A list of MFA devices.
    mfaDevices :: [MFADevice]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMFADevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listMFADevicesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listMFADevicesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listMFADevicesResponse_httpStatus' - The response's http status code.
--
-- 'mfaDevices', 'listMFADevicesResponse_mfaDevices' - A list of MFA devices.
newListMFADevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMFADevicesResponse
newListMFADevicesResponse pHttpStatus_ =
  ListMFADevicesResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      mfaDevices = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listMFADevicesResponse_isTruncated :: Lens.Lens' ListMFADevicesResponse (Prelude.Maybe Prelude.Bool)
listMFADevicesResponse_isTruncated = Lens.lens (\ListMFADevicesResponse' {isTruncated} -> isTruncated) (\s@ListMFADevicesResponse' {} a -> s {isTruncated = a} :: ListMFADevicesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listMFADevicesResponse_marker :: Lens.Lens' ListMFADevicesResponse (Prelude.Maybe Prelude.Text)
listMFADevicesResponse_marker = Lens.lens (\ListMFADevicesResponse' {marker} -> marker) (\s@ListMFADevicesResponse' {} a -> s {marker = a} :: ListMFADevicesResponse)

-- | The response's http status code.
listMFADevicesResponse_httpStatus :: Lens.Lens' ListMFADevicesResponse Prelude.Int
listMFADevicesResponse_httpStatus = Lens.lens (\ListMFADevicesResponse' {httpStatus} -> httpStatus) (\s@ListMFADevicesResponse' {} a -> s {httpStatus = a} :: ListMFADevicesResponse)

-- | A list of MFA devices.
listMFADevicesResponse_mfaDevices :: Lens.Lens' ListMFADevicesResponse [MFADevice]
listMFADevicesResponse_mfaDevices = Lens.lens (\ListMFADevicesResponse' {mfaDevices} -> mfaDevices) (\s@ListMFADevicesResponse' {} a -> s {mfaDevices = a} :: ListMFADevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMFADevicesResponse where
  rnf ListMFADevicesResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mfaDevices
