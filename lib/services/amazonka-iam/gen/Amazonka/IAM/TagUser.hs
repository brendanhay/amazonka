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
-- Module      : Amazonka.IAM.TagUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM user. If a tag with the same key name
-- already exists, then that tag is overwritten with the new value.
--
-- A tag consists of a key name and an associated value. By assigning tags
-- to your resources, you can do the following:
--
-- -   __Administrative grouping and discovery__ - Attach tags to resources
--     to aid in organization and search. For example, you could search for
--     all resources with the key name /Project/ and the value
--     /MyImportantProject/. Or search for all resources with the key name
--     /Cost Center/ and the value /41200/.
--
-- -   __Access control__ - Include tags in IAM identity-based and
--     resource-based policies. You can use tags to restrict access to only
--     an IAM requesting user that has a specified tag attached. You can
--     also restrict access to only those resources that have a certain tag
--     attached. For examples of policies that show how to use tags to
--     control access, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control access using IAM tags>
--     in the /IAM User Guide/.
--
-- -   __Cost allocation__ - Use tags to help track which individuals and
--     teams are using which Amazon Web Services resources.
--
-- -   If any one of the tags is invalid or if you exceed the allowed
--     maximum number of tags, then the entire request fails and the
--     resource is not created. For more information about tagging, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
--     in the /IAM User Guide/.
--
-- -   Amazon Web Services always interprets the tag @Value@ as a single
--     string. If you need to store an array, you can store comma-separated
--     values in the string. However, you must interpret the value in your
--     code.
--
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM identities>
-- in the /IAM User Guide/.
module Amazonka.IAM.TagUser
  ( -- * Creating a Request
    TagUser (..),
    newTagUser,

    -- * Request Lenses
    tagUser_userName,
    tagUser_tags,

    -- * Destructuring the Response
    TagUserResponse (..),
    newTagUserResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagUser' smart constructor.
data TagUser = TagUser'
  { -- | The name of the IAM user to which you want to add tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The list of tags that you want to attach to the IAM user. Each tag
    -- consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'tagUser_userName' - The name of the IAM user to which you want to add tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tags', 'tagUser_tags' - The list of tags that you want to attach to the IAM user. Each tag
-- consists of a key name and an associated value.
newTagUser ::
  -- | 'userName'
  Prelude.Text ->
  TagUser
newTagUser pUserName_ =
  TagUser'
    { userName = pUserName_,
      tags = Prelude.mempty
    }

-- | The name of the IAM user to which you want to add tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
tagUser_userName :: Lens.Lens' TagUser Prelude.Text
tagUser_userName = Lens.lens (\TagUser' {userName} -> userName) (\s@TagUser' {} a -> s {userName = a} :: TagUser)

-- | The list of tags that you want to attach to the IAM user. Each tag
-- consists of a key name and an associated value.
tagUser_tags :: Lens.Lens' TagUser [Tag]
tagUser_tags = Lens.lens (\TagUser' {tags} -> tags) (\s@TagUser' {} a -> s {tags = a} :: TagUser) Prelude.. Lens.coerced

instance Core.AWSRequest TagUser where
  type AWSResponse TagUser = TagUserResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull TagUserResponse'

instance Prelude.Hashable TagUser where
  hashWithSalt _salt TagUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagUser where
  rnf TagUser' {..} =
    Prelude.rnf userName `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TagUser where
  toPath = Prelude.const "/"

instance Data.ToQuery TagUser where
  toQuery TagUser' {..} =
    Prelude.mconcat
      [ "Action" Data.=: ("TagUser" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | /See:/ 'newTagUserResponse' smart constructor.
data TagUserResponse = TagUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagUserResponse ::
  TagUserResponse
newTagUserResponse = TagUserResponse'

instance Prelude.NFData TagUserResponse where
  rnf _ = ()
