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
-- Module      : Network.AWS.IAM.TagOpenIDConnectProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an OpenID Connect (OIDC)-compatible identity
-- provider. For more information about these providers, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc.html About web identity federation>.
-- If a tag with the same key name already exists, then that tag is
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
--     an OIDC provider that has a specified tag attached. For examples of
--     policies that show how to use tags to control access, see
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
module Network.AWS.IAM.TagOpenIDConnectProvider
  ( -- * Creating a Request
    TagOpenIDConnectProvider (..),
    newTagOpenIDConnectProvider,

    -- * Request Lenses
    tagOpenIDConnectProvider_openIDConnectProviderArn,
    tagOpenIDConnectProvider_tags,

    -- * Destructuring the Response
    TagOpenIDConnectProviderResponse (..),
    newTagOpenIDConnectProviderResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagOpenIDConnectProvider' smart constructor.
data TagOpenIDConnectProvider = TagOpenIDConnectProvider'
  { -- | The ARN of the OIDC identity provider in IAM to which you want to add
    -- tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    openIDConnectProviderArn :: Prelude.Text,
    -- | The list of tags that you want to attach to the OIDC identity provider
    -- in IAM. Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'tagOpenIDConnectProvider_openIDConnectProviderArn' - The ARN of the OIDC identity provider in IAM to which you want to add
-- tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
--
-- 'tags', 'tagOpenIDConnectProvider_tags' - The list of tags that you want to attach to the OIDC identity provider
-- in IAM. Each tag consists of a key name and an associated value.
newTagOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  TagOpenIDConnectProvider
newTagOpenIDConnectProvider
  pOpenIDConnectProviderArn_ =
    TagOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_,
        tags = Prelude.mempty
      }

-- | The ARN of the OIDC identity provider in IAM to which you want to add
-- tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
tagOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' TagOpenIDConnectProvider Prelude.Text
tagOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\TagOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@TagOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: TagOpenIDConnectProvider)

-- | The list of tags that you want to attach to the OIDC identity provider
-- in IAM. Each tag consists of a key name and an associated value.
tagOpenIDConnectProvider_tags :: Lens.Lens' TagOpenIDConnectProvider [Tag]
tagOpenIDConnectProvider_tags = Lens.lens (\TagOpenIDConnectProvider' {tags} -> tags) (\s@TagOpenIDConnectProvider' {} a -> s {tags = a} :: TagOpenIDConnectProvider) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagOpenIDConnectProvider where
  type
    Rs TagOpenIDConnectProvider =
      TagOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      TagOpenIDConnectProviderResponse'

instance Prelude.Hashable TagOpenIDConnectProvider

instance Prelude.NFData TagOpenIDConnectProvider

instance Prelude.ToHeaders TagOpenIDConnectProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath TagOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagOpenIDConnectProvider where
  toQuery TagOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("TagOpenIDConnectProvider" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Prelude.=: openIDConnectProviderArn,
        "Tags" Prelude.=: Prelude.toQueryList "member" tags
      ]

-- | /See:/ 'newTagOpenIDConnectProviderResponse' smart constructor.
data TagOpenIDConnectProviderResponse = TagOpenIDConnectProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagOpenIDConnectProviderResponse ::
  TagOpenIDConnectProviderResponse
newTagOpenIDConnectProviderResponse =
  TagOpenIDConnectProviderResponse'

instance
  Prelude.NFData
    TagOpenIDConnectProviderResponse
