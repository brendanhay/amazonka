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
-- Module      : Amazonka.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user\'s SSH information.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribeMyUserProfile
  ( -- * Creating a Request
    DescribeMyUserProfile (..),
    newDescribeMyUserProfile,

    -- * Destructuring the Response
    DescribeMyUserProfileResponse (..),
    newDescribeMyUserProfileResponse,

    -- * Response Lenses
    describeMyUserProfileResponse_userProfile,
    describeMyUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMyUserProfile' smart constructor.
data DescribeMyUserProfile = DescribeMyUserProfile'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMyUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeMyUserProfile ::
  DescribeMyUserProfile
newDescribeMyUserProfile = DescribeMyUserProfile'

instance Core.AWSRequest DescribeMyUserProfile where
  type
    AWSResponse DescribeMyUserProfile =
      DescribeMyUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMyUserProfileResponse'
            Prelude.<$> (x Data..?> "UserProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMyUserProfile where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeMyUserProfile where
  rnf _ = ()

instance Data.ToHeaders DescribeMyUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeMyUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMyUserProfile where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeMyUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMyUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeMyUserProfile@ request.
--
-- /See:/ 'newDescribeMyUserProfileResponse' smart constructor.
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
  { -- | A @UserProfile@ object that describes the user\'s SSH information.
    userProfile :: Prelude.Maybe SelfUserProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMyUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userProfile', 'describeMyUserProfileResponse_userProfile' - A @UserProfile@ object that describes the user\'s SSH information.
--
-- 'httpStatus', 'describeMyUserProfileResponse_httpStatus' - The response's http status code.
newDescribeMyUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMyUserProfileResponse
newDescribeMyUserProfileResponse pHttpStatus_ =
  DescribeMyUserProfileResponse'
    { userProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @UserProfile@ object that describes the user\'s SSH information.
describeMyUserProfileResponse_userProfile :: Lens.Lens' DescribeMyUserProfileResponse (Prelude.Maybe SelfUserProfile)
describeMyUserProfileResponse_userProfile = Lens.lens (\DescribeMyUserProfileResponse' {userProfile} -> userProfile) (\s@DescribeMyUserProfileResponse' {} a -> s {userProfile = a} :: DescribeMyUserProfileResponse)

-- | The response's http status code.
describeMyUserProfileResponse_httpStatus :: Lens.Lens' DescribeMyUserProfileResponse Prelude.Int
describeMyUserProfileResponse_httpStatus = Lens.lens (\DescribeMyUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeMyUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeMyUserProfileResponse)

instance Prelude.NFData DescribeMyUserProfileResponse where
  rnf DescribeMyUserProfileResponse' {..} =
    Prelude.rnf userProfile
      `Prelude.seq` Prelude.rnf httpStatus
