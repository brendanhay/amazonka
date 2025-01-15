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
-- Module      : Amazonka.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used.
-- The information includes the date and time of last use, along with the
-- Amazon Web Services service and Region that were specified in the last
-- request made with that key.
module Amazonka.IAM.GetAccessKeyLastUsed
  ( -- * Creating a Request
    GetAccessKeyLastUsed (..),
    newGetAccessKeyLastUsed,

    -- * Request Lenses
    getAccessKeyLastUsed_accessKeyId,

    -- * Destructuring the Response
    GetAccessKeyLastUsedResponse (..),
    newGetAccessKeyLastUsedResponse,

    -- * Response Lenses
    getAccessKeyLastUsedResponse_accessKeyLastUsed,
    getAccessKeyLastUsedResponse_userName,
    getAccessKeyLastUsedResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccessKeyLastUsed' smart constructor.
data GetAccessKeyLastUsed = GetAccessKeyLastUsed'
  { -- | The identifier of an access key.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Core.AccessKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessKeyLastUsed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'getAccessKeyLastUsed_accessKeyId' - The identifier of an access key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
newGetAccessKeyLastUsed ::
  -- | 'accessKeyId'
  Core.AccessKey ->
  GetAccessKeyLastUsed
newGetAccessKeyLastUsed pAccessKeyId_ =
  GetAccessKeyLastUsed' {accessKeyId = pAccessKeyId_}

-- | The identifier of an access key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
getAccessKeyLastUsed_accessKeyId :: Lens.Lens' GetAccessKeyLastUsed Core.AccessKey
getAccessKeyLastUsed_accessKeyId = Lens.lens (\GetAccessKeyLastUsed' {accessKeyId} -> accessKeyId) (\s@GetAccessKeyLastUsed' {} a -> s {accessKeyId = a} :: GetAccessKeyLastUsed)

instance Core.AWSRequest GetAccessKeyLastUsed where
  type
    AWSResponse GetAccessKeyLastUsed =
      GetAccessKeyLastUsedResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAccessKeyLastUsedResult"
      ( \s h x ->
          GetAccessKeyLastUsedResponse'
            Prelude.<$> (x Data..@? "AccessKeyLastUsed")
            Prelude.<*> (x Data..@? "UserName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessKeyLastUsed where
  hashWithSalt _salt GetAccessKeyLastUsed' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId

instance Prelude.NFData GetAccessKeyLastUsed where
  rnf GetAccessKeyLastUsed' {..} =
    Prelude.rnf accessKeyId

instance Data.ToHeaders GetAccessKeyLastUsed where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccessKeyLastUsed where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccessKeyLastUsed where
  toQuery GetAccessKeyLastUsed' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetAccessKeyLastUsed" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "AccessKeyId" Data.=: accessKeyId
      ]

-- | Contains the response to a successful GetAccessKeyLastUsed request. It
-- is also returned as a member of the AccessKeyMetaData structure returned
-- by the ListAccessKeys action.
--
-- /See:/ 'newGetAccessKeyLastUsedResponse' smart constructor.
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
  { -- | Contains information about the last time the access key was used.
    accessKeyLastUsed :: Prelude.Maybe AccessKeyLastUsed,
    -- | The name of the IAM user that owns this access key.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessKeyLastUsedResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyLastUsed', 'getAccessKeyLastUsedResponse_accessKeyLastUsed' - Contains information about the last time the access key was used.
--
-- 'userName', 'getAccessKeyLastUsedResponse_userName' - The name of the IAM user that owns this access key.
--
-- 'httpStatus', 'getAccessKeyLastUsedResponse_httpStatus' - The response's http status code.
newGetAccessKeyLastUsedResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessKeyLastUsedResponse
newGetAccessKeyLastUsedResponse pHttpStatus_ =
  GetAccessKeyLastUsedResponse'
    { accessKeyLastUsed =
        Prelude.Nothing,
      userName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the last time the access key was used.
getAccessKeyLastUsedResponse_accessKeyLastUsed :: Lens.Lens' GetAccessKeyLastUsedResponse (Prelude.Maybe AccessKeyLastUsed)
getAccessKeyLastUsedResponse_accessKeyLastUsed = Lens.lens (\GetAccessKeyLastUsedResponse' {accessKeyLastUsed} -> accessKeyLastUsed) (\s@GetAccessKeyLastUsedResponse' {} a -> s {accessKeyLastUsed = a} :: GetAccessKeyLastUsedResponse)

-- | The name of the IAM user that owns this access key.
getAccessKeyLastUsedResponse_userName :: Lens.Lens' GetAccessKeyLastUsedResponse (Prelude.Maybe Prelude.Text)
getAccessKeyLastUsedResponse_userName = Lens.lens (\GetAccessKeyLastUsedResponse' {userName} -> userName) (\s@GetAccessKeyLastUsedResponse' {} a -> s {userName = a} :: GetAccessKeyLastUsedResponse)

-- | The response's http status code.
getAccessKeyLastUsedResponse_httpStatus :: Lens.Lens' GetAccessKeyLastUsedResponse Prelude.Int
getAccessKeyLastUsedResponse_httpStatus = Lens.lens (\GetAccessKeyLastUsedResponse' {httpStatus} -> httpStatus) (\s@GetAccessKeyLastUsedResponse' {} a -> s {httpStatus = a} :: GetAccessKeyLastUsedResponse)

instance Prelude.NFData GetAccessKeyLastUsedResponse where
  rnf GetAccessKeyLastUsedResponse' {..} =
    Prelude.rnf accessKeyLastUsed `Prelude.seq`
      Prelude.rnf userName `Prelude.seq`
        Prelude.rnf httpStatus
