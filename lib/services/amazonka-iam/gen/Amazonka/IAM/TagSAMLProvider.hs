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
-- Module      : Amazonka.IAM.TagSAMLProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a Security Assertion Markup Language (SAML)
-- identity provider. For more information about these providers, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>
-- . If a tag with the same key name already exists, then that tag is
-- overwritten with the new value.
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
--     a SAML identity provider that has a specified tag attached. For
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
-- -   Amazon Web Services always interprets the tag @Value@ as a single
--     string. If you need to store an array, you can store comma-separated
--     values in the string. However, you must interpret the value in your
--     code.
module Amazonka.IAM.TagSAMLProvider
  ( -- * Creating a Request
    TagSAMLProvider (..),
    newTagSAMLProvider,

    -- * Request Lenses
    tagSAMLProvider_sAMLProviderArn,
    tagSAMLProvider_tags,

    -- * Destructuring the Response
    TagSAMLProviderResponse (..),
    newTagSAMLProviderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTagSAMLProvider' smart constructor.
data TagSAMLProvider = TagSAMLProvider'
  { -- | The ARN of the SAML identity provider in IAM to which you want to add
    -- tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    sAMLProviderArn :: Prelude.Text,
    -- | The list of tags that you want to attach to the SAML identity provider
    -- in IAM. Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagSAMLProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'tagSAMLProvider_sAMLProviderArn' - The ARN of the SAML identity provider in IAM to which you want to add
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tags', 'tagSAMLProvider_tags' - The list of tags that you want to attach to the SAML identity provider
-- in IAM. Each tag consists of a key name and an associated value.
newTagSAMLProvider ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
  TagSAMLProvider
newTagSAMLProvider pSAMLProviderArn_ =
  TagSAMLProvider'
    { sAMLProviderArn =
        pSAMLProviderArn_,
      tags = Prelude.mempty
    }

-- | The ARN of the SAML identity provider in IAM to which you want to add
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
tagSAMLProvider_sAMLProviderArn :: Lens.Lens' TagSAMLProvider Prelude.Text
tagSAMLProvider_sAMLProviderArn = Lens.lens (\TagSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@TagSAMLProvider' {} a -> s {sAMLProviderArn = a} :: TagSAMLProvider)

-- | The list of tags that you want to attach to the SAML identity provider
-- in IAM. Each tag consists of a key name and an associated value.
tagSAMLProvider_tags :: Lens.Lens' TagSAMLProvider [Tag]
tagSAMLProvider_tags = Lens.lens (\TagSAMLProvider' {tags} -> tags) (\s@TagSAMLProvider' {} a -> s {tags = a} :: TagSAMLProvider) Prelude.. Lens.coerced

instance Core.AWSRequest TagSAMLProvider where
  type
    AWSResponse TagSAMLProvider =
      TagSAMLProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull TagSAMLProviderResponse'

instance Prelude.Hashable TagSAMLProvider where
  hashWithSalt _salt TagSAMLProvider' {..} =
    _salt
      `Prelude.hashWithSalt` sAMLProviderArn
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TagSAMLProvider where
  rnf TagSAMLProvider' {..} =
    Prelude.rnf sAMLProviderArn
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders TagSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TagSAMLProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery TagSAMLProvider where
  toQuery TagSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TagSAMLProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Data.=: sAMLProviderArn,
        "Tags" Data.=: Data.toQueryList "member" tags
      ]

-- | /See:/ 'newTagSAMLProviderResponse' smart constructor.
data TagSAMLProviderResponse = TagSAMLProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagSAMLProviderResponse ::
  TagSAMLProviderResponse
newTagSAMLProviderResponse = TagSAMLProviderResponse'

instance Prelude.NFData TagSAMLProviderResponse where
  rnf _ = ()
