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
-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user\'s SSH information.
--
-- __Required Permissions__: To use this action, an IAM user must have
-- self-management enabled or an attached policy that explicitly grants
-- permissions. For more information about user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeMyUserProfile
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMyUserProfile' smart constructor.
data DescribeMyUserProfile = DescribeMyUserProfile'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMyUserProfileResponse'
            Core.<$> (x Core..?> "UserProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMyUserProfile

instance Core.NFData DescribeMyUserProfile

instance Core.ToHeaders DescribeMyUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeMyUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMyUserProfile where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeMyUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMyUserProfile where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeMyUserProfile@ request.
--
-- /See:/ 'newDescribeMyUserProfileResponse' smart constructor.
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
  { -- | A @UserProfile@ object that describes the user\'s SSH information.
    userProfile :: Core.Maybe SelfUserProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeMyUserProfileResponse
newDescribeMyUserProfileResponse pHttpStatus_ =
  DescribeMyUserProfileResponse'
    { userProfile =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @UserProfile@ object that describes the user\'s SSH information.
describeMyUserProfileResponse_userProfile :: Lens.Lens' DescribeMyUserProfileResponse (Core.Maybe SelfUserProfile)
describeMyUserProfileResponse_userProfile = Lens.lens (\DescribeMyUserProfileResponse' {userProfile} -> userProfile) (\s@DescribeMyUserProfileResponse' {} a -> s {userProfile = a} :: DescribeMyUserProfileResponse)

-- | The response's http status code.
describeMyUserProfileResponse_httpStatus :: Lens.Lens' DescribeMyUserProfileResponse Core.Int
describeMyUserProfileResponse_httpStatus = Lens.lens (\DescribeMyUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeMyUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeMyUserProfileResponse)

instance Core.NFData DescribeMyUserProfileResponse
