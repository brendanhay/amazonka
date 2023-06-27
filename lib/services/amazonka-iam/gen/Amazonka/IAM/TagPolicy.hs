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
-- Module      : Amazonka.IAM.TagPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM customer managed policy. If a tag with
-- the same key name already exists, then that tag is overwritten with the
-- new value.
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
-- -   __Access control__ - Include tags in IAM user-based and
--     resource-based policies. You can use tags to restrict access to only
--     an IAM customer managed policy that has a specified tag attached.
--     For examples of policies that show how to use tags to control
--     access, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control access using IAM tags>
--     in the /IAM User Guide/.
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
module Amazonka.IAM.TagPolicy
  ( -- * Creating a Request
    TagPolicy (..),
    newTagPolicy,

    -- * Request Lenses
    tagPolicy_policyArn,
    tagPolicy_tags,

    -- * Destructuring the Response
    TagPolicyResponse (..),
    newTagPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagPolicy' smart constructor.
data TagPolicy = TagPolicy'
  { -- | The ARN of the IAM customer managed policy to which you want to add
    -- tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyArn :: Prelude.Text,
    -- | The list of tags that you want to attach to the IAM customer managed
    -- policy. Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'tagPolicy_policyArn' - The ARN of the IAM customer managed policy to which you want to add
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tags', 'tagPolicy_tags' - The list of tags that you want to attach to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value.
newTagPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  TagPolicy
newTagPolicy pPolicyArn_ =
  TagPolicy'
    { policyArn = pPolicyArn_,
      tags = Prelude.mempty
    }

-- | The ARN of the IAM customer managed policy to which you want to add
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
tagPolicy_policyArn :: Lens.Lens' TagPolicy Prelude.Text
tagPolicy_policyArn = Lens.lens (\TagPolicy' {policyArn} -> policyArn) (\s@TagPolicy' {} a -> s {policyArn = a} :: TagPolicy)

-- | The list of tags that you want to attach to the IAM customer managed
-- policy. Each tag consists of a key name and an associated value.
tagPolicy_tags :: Lens.Lens' TagPolicy [Tag]
tagPolicy_tags = Lens.lens (\TagPolicy' {tags} -> tags) (\s@TagPolicy' {} a -> s {tags = a} :: TagPolicy) Prelude.. Lens.coerced

instance Core.AWSRequest TagPolicy where
  type AWSResponse TagPolicy = TagPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response = Response.receiveNull TagPolicyResponse'

instance Prelude.Hashable TagPolicy where
  hashWithSalt _salt TagPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagPolicy where
  rnf TagPolicy' {..} =
    Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TagPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery TagPolicy where
  toQuery TagPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TagPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyArn" Data.=: policyArn,
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | /See:/ 'newTagPolicyResponse' smart constructor.
data TagPolicyResponse = TagPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagPolicyResponse ::
  TagPolicyResponse
newTagPolicyResponse = TagPolicyResponse'

instance Prelude.NFData TagPolicyResponse where
  rnf _ = ()
