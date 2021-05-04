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
-- Module      : Network.AWS.IAM.TagServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an IAM server certificate. If a tag with the
-- same key name already exists, then that tag is overwritten with the new
-- value.
--
-- For certificates in a Region supported by AWS Certificate Manager (ACM),
-- we recommend that you don\'t use IAM server certificates. Instead, use
-- ACM to provision, manage, and deploy your server certificates. For more
-- information about IAM server certificates,
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/.
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
--     a server certificate that has a specified tag attached. For examples
--     of policies that show how to use tags to control access, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Control access using IAM tags>
--     in the /IAM User Guide/.
--
-- -   __Cost allocation__ - Use tags to help track which individuals and
--     teams are using which AWS resources.
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
module Network.AWS.IAM.TagServerCertificate
  ( -- * Creating a Request
    TagServerCertificate (..),
    newTagServerCertificate,

    -- * Request Lenses
    tagServerCertificate_serverCertificateName,
    tagServerCertificate_tags,

    -- * Destructuring the Response
    TagServerCertificateResponse (..),
    newTagServerCertificateResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagServerCertificate' smart constructor.
data TagServerCertificate = TagServerCertificate'
  { -- | The name of the IAM server certificate to which you want to add tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    serverCertificateName :: Prelude.Text,
    -- | The list of tags that you want to attach to the IAM server certificate.
    -- Each tag consists of a key name and an associated value.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateName', 'tagServerCertificate_serverCertificateName' - The name of the IAM server certificate to which you want to add tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
--
-- 'tags', 'tagServerCertificate_tags' - The list of tags that you want to attach to the IAM server certificate.
-- Each tag consists of a key name and an associated value.
newTagServerCertificate ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  TagServerCertificate
newTagServerCertificate pServerCertificateName_ =
  TagServerCertificate'
    { serverCertificateName =
        pServerCertificateName_,
      tags = Prelude.mempty
    }

-- | The name of the IAM server certificate to which you want to add tags.
--
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
tagServerCertificate_serverCertificateName :: Lens.Lens' TagServerCertificate Prelude.Text
tagServerCertificate_serverCertificateName = Lens.lens (\TagServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@TagServerCertificate' {} a -> s {serverCertificateName = a} :: TagServerCertificate)

-- | The list of tags that you want to attach to the IAM server certificate.
-- Each tag consists of a key name and an associated value.
tagServerCertificate_tags :: Lens.Lens' TagServerCertificate [Tag]
tagServerCertificate_tags = Lens.lens (\TagServerCertificate' {tags} -> tags) (\s@TagServerCertificate' {} a -> s {tags = a} :: TagServerCertificate) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest TagServerCertificate where
  type
    Rs TagServerCertificate =
      TagServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull TagServerCertificateResponse'

instance Prelude.Hashable TagServerCertificate

instance Prelude.NFData TagServerCertificate

instance Prelude.ToHeaders TagServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath TagServerCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery TagServerCertificate where
  toQuery TagServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("TagServerCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "ServerCertificateName"
          Prelude.=: serverCertificateName,
        "Tags" Prelude.=: Prelude.toQueryList "member" tags
      ]

-- | /See:/ 'newTagServerCertificateResponse' smart constructor.
data TagServerCertificateResponse = TagServerCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTagServerCertificateResponse ::
  TagServerCertificateResponse
newTagServerCertificateResponse =
  TagServerCertificateResponse'

instance Prelude.NFData TagServerCertificateResponse
