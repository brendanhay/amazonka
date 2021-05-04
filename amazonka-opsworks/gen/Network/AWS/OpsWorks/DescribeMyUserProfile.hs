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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMyUserProfile' smart constructor.
data DescribeMyUserProfile = DescribeMyUserProfile'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeMyUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeMyUserProfile ::
  DescribeMyUserProfile
newDescribeMyUserProfile = DescribeMyUserProfile'

instance Prelude.AWSRequest DescribeMyUserProfile where
  type
    Rs DescribeMyUserProfile =
      DescribeMyUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMyUserProfileResponse'
            Prelude.<$> (x Prelude..?> "UserProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMyUserProfile

instance Prelude.NFData DescribeMyUserProfile

instance Prelude.ToHeaders DescribeMyUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.DescribeMyUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeMyUserProfile where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DescribeMyUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeMyUserProfile where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeMyUserProfileResponse
