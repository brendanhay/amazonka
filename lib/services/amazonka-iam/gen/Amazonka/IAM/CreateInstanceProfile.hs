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
-- Module      : Amazonka.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance profile. For information about instance profiles,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_switch-role-ec2.html Using roles for applications on Amazon EC2>
-- in the /IAM User Guide/, and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html#ec2-instance-profile Instance profiles>
-- in the /Amazon EC2 User Guide/.
--
-- For information about the number of instance profiles you can create,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM object quotas>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateInstanceProfile
  ( -- * Creating a Request
    CreateInstanceProfile (..),
    newCreateInstanceProfile,

    -- * Request Lenses
    createInstanceProfile_tags,
    createInstanceProfile_path,
    createInstanceProfile_instanceProfileName,

    -- * Destructuring the Response
    CreateInstanceProfileResponse (..),
    newCreateInstanceProfileResponse,

    -- * Response Lenses
    createInstanceProfileResponse_httpStatus,
    createInstanceProfileResponse_instanceProfile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { -- | A list of tags that you want to attach to the newly created IAM instance
    -- profile. Each tag consists of a key name and an associated value. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The path to the instance profile. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/).
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    path :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile to create.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createInstanceProfile_tags' - A list of tags that you want to attach to the newly created IAM instance
-- profile. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'path', 'createInstanceProfile_path' - The path to the instance profile. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'instanceProfileName', 'createInstanceProfile_instanceProfileName' - The name of the instance profile to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newCreateInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  CreateInstanceProfile
newCreateInstanceProfile pInstanceProfileName_ =
  CreateInstanceProfile'
    { tags = Prelude.Nothing,
      path = Prelude.Nothing,
      instanceProfileName = pInstanceProfileName_
    }

-- | A list of tags that you want to attach to the newly created IAM instance
-- profile. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createInstanceProfile_tags :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe [Tag])
createInstanceProfile_tags = Lens.lens (\CreateInstanceProfile' {tags} -> tags) (\s@CreateInstanceProfile' {} a -> s {tags = a} :: CreateInstanceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The path to the instance profile. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
createInstanceProfile_path :: Lens.Lens' CreateInstanceProfile (Prelude.Maybe Prelude.Text)
createInstanceProfile_path = Lens.lens (\CreateInstanceProfile' {path} -> path) (\s@CreateInstanceProfile' {} a -> s {path = a} :: CreateInstanceProfile)

-- | The name of the instance profile to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createInstanceProfile_instanceProfileName :: Lens.Lens' CreateInstanceProfile Prelude.Text
createInstanceProfile_instanceProfileName = Lens.lens (\CreateInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@CreateInstanceProfile' {} a -> s {instanceProfileName = a} :: CreateInstanceProfile)

instance Core.AWSRequest CreateInstanceProfile where
  type
    AWSResponse CreateInstanceProfile =
      CreateInstanceProfileResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateInstanceProfileResult"
      ( \s h x ->
          CreateInstanceProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "InstanceProfile")
      )

instance Prelude.Hashable CreateInstanceProfile where
  hashWithSalt _salt CreateInstanceProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` instanceProfileName

instance Prelude.NFData CreateInstanceProfile where
  rnf CreateInstanceProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf instanceProfileName

instance Core.ToHeaders CreateInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateInstanceProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateInstanceProfile where
  toQuery CreateInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateInstanceProfile" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "Path" Core.=: path,
        "InstanceProfileName" Core.=: instanceProfileName
      ]

-- | Contains the response to a successful CreateInstanceProfile request.
--
-- /See:/ 'newCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the new instance profile.
    instanceProfile :: InstanceProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInstanceProfileResponse_httpStatus' - The response's http status code.
--
-- 'instanceProfile', 'createInstanceProfileResponse_instanceProfile' - A structure containing details about the new instance profile.
newCreateInstanceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'instanceProfile'
  InstanceProfile ->
  CreateInstanceProfileResponse
newCreateInstanceProfileResponse
  pHttpStatus_
  pInstanceProfile_ =
    CreateInstanceProfileResponse'
      { httpStatus =
          pHttpStatus_,
        instanceProfile = pInstanceProfile_
      }

-- | The response's http status code.
createInstanceProfileResponse_httpStatus :: Lens.Lens' CreateInstanceProfileResponse Prelude.Int
createInstanceProfileResponse_httpStatus = Lens.lens (\CreateInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceProfileResponse' {} a -> s {httpStatus = a} :: CreateInstanceProfileResponse)

-- | A structure containing details about the new instance profile.
createInstanceProfileResponse_instanceProfile :: Lens.Lens' CreateInstanceProfileResponse InstanceProfile
createInstanceProfileResponse_instanceProfile = Lens.lens (\CreateInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@CreateInstanceProfileResponse' {} a -> s {instanceProfile = a} :: CreateInstanceProfileResponse)

instance Prelude.NFData CreateInstanceProfileResponse where
  rnf CreateInstanceProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf instanceProfile
