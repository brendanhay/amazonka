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
-- Module      : Amazonka.ChimeSDKIdentity.DescribeAppInstanceUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of an @AppInstanceUser@.
module Amazonka.ChimeSDKIdentity.DescribeAppInstanceUser
  ( -- * Creating a Request
    DescribeAppInstanceUser (..),
    newDescribeAppInstanceUser,

    -- * Request Lenses
    describeAppInstanceUser_appInstanceUserArn,

    -- * Destructuring the Response
    DescribeAppInstanceUserResponse (..),
    newDescribeAppInstanceUserResponse,

    -- * Response Lenses
    describeAppInstanceUserResponse_appInstanceUser,
    describeAppInstanceUserResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppInstanceUser' smart constructor.
data DescribeAppInstanceUser = DescribeAppInstanceUser'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'describeAppInstanceUser_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
newDescribeAppInstanceUser ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  DescribeAppInstanceUser
newDescribeAppInstanceUser pAppInstanceUserArn_ =
  DescribeAppInstanceUser'
    { appInstanceUserArn =
        pAppInstanceUserArn_
    }

-- | The ARN of the @AppInstanceUser@.
describeAppInstanceUser_appInstanceUserArn :: Lens.Lens' DescribeAppInstanceUser Prelude.Text
describeAppInstanceUser_appInstanceUserArn = Lens.lens (\DescribeAppInstanceUser' {appInstanceUserArn} -> appInstanceUserArn) (\s@DescribeAppInstanceUser' {} a -> s {appInstanceUserArn = a} :: DescribeAppInstanceUser)

instance Core.AWSRequest DescribeAppInstanceUser where
  type
    AWSResponse DescribeAppInstanceUser =
      DescribeAppInstanceUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppInstanceUserResponse'
            Prelude.<$> (x Data..?> "AppInstanceUser")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAppInstanceUser where
  hashWithSalt _salt DescribeAppInstanceUser' {..} =
    _salt `Prelude.hashWithSalt` appInstanceUserArn

instance Prelude.NFData DescribeAppInstanceUser where
  rnf DescribeAppInstanceUser' {..} =
    Prelude.rnf appInstanceUserArn

instance Data.ToHeaders DescribeAppInstanceUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAppInstanceUser where
  toPath DescribeAppInstanceUser' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn
      ]

instance Data.ToQuery DescribeAppInstanceUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppInstanceUserResponse' smart constructor.
data DescribeAppInstanceUserResponse = DescribeAppInstanceUserResponse'
  { -- | The name of the @AppInstanceUser@.
    appInstanceUser :: Prelude.Maybe AppInstanceUser,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUser', 'describeAppInstanceUserResponse_appInstanceUser' - The name of the @AppInstanceUser@.
--
-- 'httpStatus', 'describeAppInstanceUserResponse_httpStatus' - The response's http status code.
newDescribeAppInstanceUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppInstanceUserResponse
newDescribeAppInstanceUserResponse pHttpStatus_ =
  DescribeAppInstanceUserResponse'
    { appInstanceUser =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the @AppInstanceUser@.
describeAppInstanceUserResponse_appInstanceUser :: Lens.Lens' DescribeAppInstanceUserResponse (Prelude.Maybe AppInstanceUser)
describeAppInstanceUserResponse_appInstanceUser = Lens.lens (\DescribeAppInstanceUserResponse' {appInstanceUser} -> appInstanceUser) (\s@DescribeAppInstanceUserResponse' {} a -> s {appInstanceUser = a} :: DescribeAppInstanceUserResponse)

-- | The response's http status code.
describeAppInstanceUserResponse_httpStatus :: Lens.Lens' DescribeAppInstanceUserResponse Prelude.Int
describeAppInstanceUserResponse_httpStatus = Lens.lens (\DescribeAppInstanceUserResponse' {httpStatus} -> httpStatus) (\s@DescribeAppInstanceUserResponse' {} a -> s {httpStatus = a} :: DescribeAppInstanceUserResponse)

instance
  Prelude.NFData
    DescribeAppInstanceUserResponse
  where
  rnf DescribeAppInstanceUserResponse' {..} =
    Prelude.rnf appInstanceUser
      `Prelude.seq` Prelude.rnf httpStatus
