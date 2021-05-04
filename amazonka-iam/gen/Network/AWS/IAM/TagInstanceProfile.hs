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
-- Module      : Network.AWS.IAM.TagInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM instance profile. If a tag with the same
-- key name already exists, then that tag is overwritten with the new
-- value.
--
-- Each tag consists of a key name and an associated value. By assigning
-- tags to your resources, you can do the following:
--
-- -   __Administrative grouping and discovery__ - Attach tags to resources
--     to aid in organization and search. For example, you could search for
--     all resources with the key name /Project/ and the value
--     /MyImportantProject/. Or search for all resources with the key name
--     /Cost Center/ and the value /41200/.
--
-- -   __Access control__ - Include tags in IAM user-based and
--     resource-based policies. You can use tags to restrict access to only
--     an IAM instance profile that has a specified tag attached. For
--     examples of policies that show how to use tags to control access,
--     see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control access using IAM tags>
--     in the /IAM User Guide/.
--
-- -   If any one of the tags is invalid or if you exceed the allowed
--     maximum number of tags, then the entire request fails and the
--     resource is not created. For more information about tagging, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
--     in the /IAM User Guide/.
--
-- -   AWS always interprets the tag @Value@ as a single string. If you
--     need to store an array, you can store comma-separated values in the
--     string. However, you must interpret the value in your code.
module Network.AWS.IAM.TagInstanceProfile
  ( -- * Creating a Request
    TagInstanceProfile (..),
    newTagInstanceProfile,

    -- * Request Lenses
    tagInstanceProfile_instanceProfileName,
    tagInstanceProfile_tags,

    -- * Destructuring the Response
    TagInstanceProfileResponse (..),
    newTagInstanceProfileResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagInstanceProfile' smart constructor.
data TagInstanceProfile = TagInstanceProfile'
  { -- | The name of the IAM instance profile to which you want to add tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    instanceProfileName :: Prelude.Text,
    -- | The list of tags that you want to attach to the IAM instance profile.
    -- Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfileName', 'tagInstanceProfile_instanceProfileName' - The name of the IAM instance profile to which you want to add tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
--
-- 'tags', 'tagInstanceProfile_tags' - The list of tags that you want to attach to the IAM instance profile.
-- Each tag consists of a key name and an associated value.
newTagInstanceProfile ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  TagInstanceProfile
newTagInstanceProfile pInstanceProfileName_ =
  TagInstanceProfile'
    { instanceProfileName =
        pInstanceProfileName_,
      tags = Prelude.mempty
    }

-- | The name of the IAM instance profile to which you want to add tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
tagInstanceProfile_instanceProfileName :: Lens.Lens' TagInstanceProfile Prelude.Text
tagInstanceProfile_instanceProfileName = Lens.lens (\TagInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@TagInstanceProfile' {} a -> s {instanceProfileName = a} :: TagInstanceProfile)

-- | The list of tags that you want to attach to the IAM instance profile.
-- Each tag consists of a key name and an associated value.
tagInstanceProfile_tags :: Lens.Lens' TagInstanceProfile [Tag]
tagInstanceProfile_tags = Lens.lens (\TagInstanceProfile' {tags} -> tags) (\s@TagInstanceProfile' {} a -> s {tags = a} :: TagInstanceProfile) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagInstanceProfile where
  type
    Rs TagInstanceProfile =
      TagInstanceProfileResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull TagInstanceProfileResponse'

instance Prelude.Hashable TagInstanceProfile

instance Prelude.NFData TagInstanceProfile

instance Prelude.ToHeaders TagInstanceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath TagInstanceProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagInstanceProfile where
  toQuery TagInstanceProfile' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("TagInstanceProfile" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "InstanceProfileName" Prelude.=: instanceProfileName,
        "Tags" Prelude.=: Prelude.toQueryList "member" tags
      ]

-- | /See:/ 'newTagInstanceProfileResponse' smart constructor.
data TagInstanceProfileResponse = TagInstanceProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagInstanceProfileResponse ::
  TagInstanceProfileResponse
newTagInstanceProfileResponse =
  TagInstanceProfileResponse'

instance Prelude.NFData TagInstanceProfileResponse
