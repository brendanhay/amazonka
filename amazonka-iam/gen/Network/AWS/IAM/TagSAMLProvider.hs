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
-- Module      : Network.AWS.IAM.TagSAMLProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- -   AWS always interprets the tag @Value@ as a single string. If you
--     need to store an array, you can store comma-separated values in the
--     string. However, you must interpret the value in your code.
module Network.AWS.IAM.TagSAMLProvider
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagSAMLProvider' smart constructor.
data TagSAMLProvider = TagSAMLProvider'
  { -- | The ARN of the SAML identity provider in IAM to which you want to add
    -- tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    sAMLProviderArn :: Prelude.Text,
    -- | The list of tags that you want to attach to the SAML identity provider
    -- in IAM. Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
tagSAMLProvider_sAMLProviderArn :: Lens.Lens' TagSAMLProvider Prelude.Text
tagSAMLProvider_sAMLProviderArn = Lens.lens (\TagSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@TagSAMLProvider' {} a -> s {sAMLProviderArn = a} :: TagSAMLProvider)

-- | The list of tags that you want to attach to the SAML identity provider
-- in IAM. Each tag consists of a key name and an associated value.
tagSAMLProvider_tags :: Lens.Lens' TagSAMLProvider [Tag]
tagSAMLProvider_tags = Lens.lens (\TagSAMLProvider' {tags} -> tags) (\s@TagSAMLProvider' {} a -> s {tags = a} :: TagSAMLProvider) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagSAMLProvider where
  type Rs TagSAMLProvider = TagSAMLProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull TagSAMLProviderResponse'

instance Prelude.Hashable TagSAMLProvider

instance Prelude.NFData TagSAMLProvider

instance Prelude.ToHeaders TagSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath TagSAMLProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagSAMLProvider where
  toQuery TagSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("TagSAMLProvider" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Prelude.=: sAMLProviderArn,
        "Tags" Prelude.=: Prelude.toQueryList "member" tags
      ]

-- | /See:/ 'newTagSAMLProviderResponse' smart constructor.
data TagSAMLProviderResponse = TagSAMLProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagSAMLProviderResponse ::
  TagSAMLProviderResponse
newTagSAMLProviderResponse = TagSAMLProviderResponse'

instance Prelude.NFData TagSAMLProviderResponse
