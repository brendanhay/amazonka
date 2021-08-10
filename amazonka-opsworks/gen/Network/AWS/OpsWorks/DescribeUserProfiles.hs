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
-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe specified users.
--
-- __Required Permissions__: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information
-- about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeUserProfiles
  ( -- * Creating a Request
    DescribeUserProfiles (..),
    newDescribeUserProfiles,

    -- * Request Lenses
    describeUserProfiles_iamUserArns,

    -- * Destructuring the Response
    DescribeUserProfilesResponse (..),
    newDescribeUserProfilesResponse,

    -- * Response Lenses
    describeUserProfilesResponse_userProfiles,
    describeUserProfilesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserProfiles' smart constructor.
data DescribeUserProfiles = DescribeUserProfiles'
  { -- | An array of IAM or federated user ARNs that identify the users to be
    -- described.
    iamUserArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArns', 'describeUserProfiles_iamUserArns' - An array of IAM or federated user ARNs that identify the users to be
-- described.
newDescribeUserProfiles ::
  DescribeUserProfiles
newDescribeUserProfiles =
  DescribeUserProfiles'
    { iamUserArns =
        Prelude.Nothing
    }

-- | An array of IAM or federated user ARNs that identify the users to be
-- described.
describeUserProfiles_iamUserArns :: Lens.Lens' DescribeUserProfiles (Prelude.Maybe [Prelude.Text])
describeUserProfiles_iamUserArns = Lens.lens (\DescribeUserProfiles' {iamUserArns} -> iamUserArns) (\s@DescribeUserProfiles' {} a -> s {iamUserArns = a} :: DescribeUserProfiles) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeUserProfiles where
  type
    AWSResponse DescribeUserProfiles =
      DescribeUserProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfilesResponse'
            Prelude.<$> (x Core..?> "UserProfiles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserProfiles

instance Prelude.NFData DescribeUserProfiles

instance Core.ToHeaders DescribeUserProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeUserProfiles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUserProfiles where
  toJSON DescribeUserProfiles' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IamUserArns" Core..=) Prelude.<$> iamUserArns]
      )

instance Core.ToPath DescribeUserProfiles where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUserProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeUserProfiles@ request.
--
-- /See:/ 'newDescribeUserProfilesResponse' smart constructor.
data DescribeUserProfilesResponse = DescribeUserProfilesResponse'
  { -- | A @Users@ object that describes the specified users.
    userProfiles :: Prelude.Maybe [UserProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userProfiles', 'describeUserProfilesResponse_userProfiles' - A @Users@ object that describes the specified users.
--
-- 'httpStatus', 'describeUserProfilesResponse_httpStatus' - The response's http status code.
newDescribeUserProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserProfilesResponse
newDescribeUserProfilesResponse pHttpStatus_ =
  DescribeUserProfilesResponse'
    { userProfiles =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @Users@ object that describes the specified users.
describeUserProfilesResponse_userProfiles :: Lens.Lens' DescribeUserProfilesResponse (Prelude.Maybe [UserProfile])
describeUserProfilesResponse_userProfiles = Lens.lens (\DescribeUserProfilesResponse' {userProfiles} -> userProfiles) (\s@DescribeUserProfilesResponse' {} a -> s {userProfiles = a} :: DescribeUserProfilesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeUserProfilesResponse_httpStatus :: Lens.Lens' DescribeUserProfilesResponse Prelude.Int
describeUserProfilesResponse_httpStatus = Lens.lens (\DescribeUserProfilesResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfilesResponse' {} a -> s {httpStatus = a} :: DescribeUserProfilesResponse)

instance Prelude.NFData DescribeUserProfilesResponse
