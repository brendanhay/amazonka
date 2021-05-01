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
-- Module      : Network.AWS.IAM.GetAccessKeyLastUsed
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about when the specified access key was last used.
-- The information includes the date and time of last use, along with the
-- AWS service and Region that were specified in the last request made with
-- that key.
module Network.AWS.IAM.GetAccessKeyLastUsed
  ( -- * Creating a Request
    GetAccessKeyLastUsed (..),
    newGetAccessKeyLastUsed,

    -- * Request Lenses
    getAccessKeyLastUsed_accessKeyId,

    -- * Destructuring the Response
    GetAccessKeyLastUsedResponse (..),
    newGetAccessKeyLastUsedResponse,

    -- * Response Lenses
    getAccessKeyLastUsedResponse_userName,
    getAccessKeyLastUsedResponse_accessKeyLastUsed,
    getAccessKeyLastUsedResponse_httpStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccessKeyLastUsed' smart constructor.
data GetAccessKeyLastUsed = GetAccessKeyLastUsed'
  { -- | The identifier of an access key.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that can consist of any upper or lowercased letter or digit.
    accessKeyId :: Prelude.AccessKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AccessKey ->
  GetAccessKeyLastUsed
newGetAccessKeyLastUsed pAccessKeyId_ =
  GetAccessKeyLastUsed' {accessKeyId = pAccessKeyId_}

-- | The identifier of an access key.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that can consist of any upper or lowercased letter or digit.
getAccessKeyLastUsed_accessKeyId :: Lens.Lens' GetAccessKeyLastUsed Prelude.AccessKey
getAccessKeyLastUsed_accessKeyId = Lens.lens (\GetAccessKeyLastUsed' {accessKeyId} -> accessKeyId) (\s@GetAccessKeyLastUsed' {} a -> s {accessKeyId = a} :: GetAccessKeyLastUsed)

instance Prelude.AWSRequest GetAccessKeyLastUsed where
  type
    Rs GetAccessKeyLastUsed =
      GetAccessKeyLastUsedResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccessKeyLastUsedResult"
      ( \s h x ->
          GetAccessKeyLastUsedResponse'
            Prelude.<$> (x Prelude..@? "UserName")
            Prelude.<*> (x Prelude..@? "AccessKeyLastUsed")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessKeyLastUsed

instance Prelude.NFData GetAccessKeyLastUsed

instance Prelude.ToHeaders GetAccessKeyLastUsed where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetAccessKeyLastUsed where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAccessKeyLastUsed where
  toQuery GetAccessKeyLastUsed' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetAccessKeyLastUsed" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "AccessKeyId" Prelude.=: accessKeyId
      ]

-- | Contains the response to a successful GetAccessKeyLastUsed request. It
-- is also returned as a member of the AccessKeyMetaData structure returned
-- by the ListAccessKeys action.
--
-- /See:/ 'newGetAccessKeyLastUsedResponse' smart constructor.
data GetAccessKeyLastUsedResponse = GetAccessKeyLastUsedResponse'
  { -- | The name of the AWS IAM user that owns this access key.
    userName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the last time the access key was used.
    accessKeyLastUsed :: Prelude.Maybe AccessKeyLastUsed,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccessKeyLastUsedResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'getAccessKeyLastUsedResponse_userName' - The name of the AWS IAM user that owns this access key.
--
-- 'accessKeyLastUsed', 'getAccessKeyLastUsedResponse_accessKeyLastUsed' - Contains information about the last time the access key was used.
--
-- 'httpStatus', 'getAccessKeyLastUsedResponse_httpStatus' - The response's http status code.
newGetAccessKeyLastUsedResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessKeyLastUsedResponse
newGetAccessKeyLastUsedResponse pHttpStatus_ =
  GetAccessKeyLastUsedResponse'
    { userName =
        Prelude.Nothing,
      accessKeyLastUsed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the AWS IAM user that owns this access key.
getAccessKeyLastUsedResponse_userName :: Lens.Lens' GetAccessKeyLastUsedResponse (Prelude.Maybe Prelude.Text)
getAccessKeyLastUsedResponse_userName = Lens.lens (\GetAccessKeyLastUsedResponse' {userName} -> userName) (\s@GetAccessKeyLastUsedResponse' {} a -> s {userName = a} :: GetAccessKeyLastUsedResponse)

-- | Contains information about the last time the access key was used.
getAccessKeyLastUsedResponse_accessKeyLastUsed :: Lens.Lens' GetAccessKeyLastUsedResponse (Prelude.Maybe AccessKeyLastUsed)
getAccessKeyLastUsedResponse_accessKeyLastUsed = Lens.lens (\GetAccessKeyLastUsedResponse' {accessKeyLastUsed} -> accessKeyLastUsed) (\s@GetAccessKeyLastUsedResponse' {} a -> s {accessKeyLastUsed = a} :: GetAccessKeyLastUsedResponse)

-- | The response's http status code.
getAccessKeyLastUsedResponse_httpStatus :: Lens.Lens' GetAccessKeyLastUsedResponse Prelude.Int
getAccessKeyLastUsedResponse_httpStatus = Lens.lens (\GetAccessKeyLastUsedResponse' {httpStatus} -> httpStatus) (\s@GetAccessKeyLastUsedResponse' {} a -> s {httpStatus = a} :: GetAccessKeyLastUsedResponse)

instance Prelude.NFData GetAccessKeyLastUsedResponse
