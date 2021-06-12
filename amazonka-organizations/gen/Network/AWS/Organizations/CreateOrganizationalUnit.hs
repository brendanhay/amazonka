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
-- Module      : Network.AWS.Organizations.CreateOrganizationalUnit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an organizational unit (OU) within a root or parent OU. An OU is
-- a container for accounts that enables you to organize your accounts to
-- apply policies according to your business requirements. The number of
-- levels deep that you can nest OUs is dependent upon the policy types
-- enabled for that root. For service control policies, the limit is five.
--
-- For more information about OUs, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing Organizational Units>
-- in the /AWS Organizations User Guide./
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.CreateOrganizationalUnit
  ( -- * Creating a Request
    CreateOrganizationalUnit (..),
    newCreateOrganizationalUnit,

    -- * Request Lenses
    createOrganizationalUnit_tags,
    createOrganizationalUnit_parentId,
    createOrganizationalUnit_name,

    -- * Destructuring the Response
    CreateOrganizationalUnitResponse (..),
    newCreateOrganizationalUnitResponse,

    -- * Response Lenses
    createOrganizationalUnitResponse_organizationalUnit,
    createOrganizationalUnitResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateOrganizationalUnit' smart constructor.
data CreateOrganizationalUnit = CreateOrganizationalUnit'
  { -- | A list of tags that you want to attach to the newly created OU. For each
    -- tag in the list, you must specify both a tag key and a value. You can
    -- set the value to an empty string, but you can\'t set it to @null@. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
    -- in the AWS Organizations User Guide.
    --
    -- If any one of the tags is invalid or if you exceed the allowed number of
    -- tags for an OU, then the entire request fails and the OU is not created.
    tags :: Core.Maybe [Tag],
    -- | The unique identifier (ID) of the parent root or OU that you want to
    -- create the new OU in.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    parentId :: Core.Text,
    -- | The friendly name to assign to the new OU.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOrganizationalUnit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOrganizationalUnit_tags' - A list of tags that you want to attach to the newly created OU. For each
-- tag in the list, you must specify both a tag key and a value. You can
-- set the value to an empty string, but you can\'t set it to @null@. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an OU, then the entire request fails and the OU is not created.
--
-- 'parentId', 'createOrganizationalUnit_parentId' - The unique identifier (ID) of the parent root or OU that you want to
-- create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
--
-- 'name', 'createOrganizationalUnit_name' - The friendly name to assign to the new OU.
newCreateOrganizationalUnit ::
  -- | 'parentId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateOrganizationalUnit
newCreateOrganizationalUnit pParentId_ pName_ =
  CreateOrganizationalUnit'
    { tags = Core.Nothing,
      parentId = pParentId_,
      name = pName_
    }

-- | A list of tags that you want to attach to the newly created OU. For each
-- tag in the list, you must specify both a tag key and a value. You can
-- set the value to an empty string, but you can\'t set it to @null@. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an OU, then the entire request fails and the OU is not created.
createOrganizationalUnit_tags :: Lens.Lens' CreateOrganizationalUnit (Core.Maybe [Tag])
createOrganizationalUnit_tags = Lens.lens (\CreateOrganizationalUnit' {tags} -> tags) (\s@CreateOrganizationalUnit' {} a -> s {tags = a} :: CreateOrganizationalUnit) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier (ID) of the parent root or OU that you want to
-- create the new OU in.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
createOrganizationalUnit_parentId :: Lens.Lens' CreateOrganizationalUnit Core.Text
createOrganizationalUnit_parentId = Lens.lens (\CreateOrganizationalUnit' {parentId} -> parentId) (\s@CreateOrganizationalUnit' {} a -> s {parentId = a} :: CreateOrganizationalUnit)

-- | The friendly name to assign to the new OU.
createOrganizationalUnit_name :: Lens.Lens' CreateOrganizationalUnit Core.Text
createOrganizationalUnit_name = Lens.lens (\CreateOrganizationalUnit' {name} -> name) (\s@CreateOrganizationalUnit' {} a -> s {name = a} :: CreateOrganizationalUnit)

instance Core.AWSRequest CreateOrganizationalUnit where
  type
    AWSResponse CreateOrganizationalUnit =
      CreateOrganizationalUnitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrganizationalUnitResponse'
            Core.<$> (x Core..?> "OrganizationalUnit")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOrganizationalUnit

instance Core.NFData CreateOrganizationalUnit

instance Core.ToHeaders CreateOrganizationalUnit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.CreateOrganizationalUnit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateOrganizationalUnit where
  toJSON CreateOrganizationalUnit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just ("ParentId" Core..= parentId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateOrganizationalUnit where
  toPath = Core.const "/"

instance Core.ToQuery CreateOrganizationalUnit where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOrganizationalUnitResponse' smart constructor.
data CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse'
  { -- | A structure that contains details about the newly created OU.
    organizationalUnit :: Core.Maybe OrganizationalUnit,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOrganizationalUnitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnit', 'createOrganizationalUnitResponse_organizationalUnit' - A structure that contains details about the newly created OU.
--
-- 'httpStatus', 'createOrganizationalUnitResponse_httpStatus' - The response's http status code.
newCreateOrganizationalUnitResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOrganizationalUnitResponse
newCreateOrganizationalUnitResponse pHttpStatus_ =
  CreateOrganizationalUnitResponse'
    { organizationalUnit =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the newly created OU.
createOrganizationalUnitResponse_organizationalUnit :: Lens.Lens' CreateOrganizationalUnitResponse (Core.Maybe OrganizationalUnit)
createOrganizationalUnitResponse_organizationalUnit = Lens.lens (\CreateOrganizationalUnitResponse' {organizationalUnit} -> organizationalUnit) (\s@CreateOrganizationalUnitResponse' {} a -> s {organizationalUnit = a} :: CreateOrganizationalUnitResponse)

-- | The response's http status code.
createOrganizationalUnitResponse_httpStatus :: Lens.Lens' CreateOrganizationalUnitResponse Core.Int
createOrganizationalUnitResponse_httpStatus = Lens.lens (\CreateOrganizationalUnitResponse' {httpStatus} -> httpStatus) (\s@CreateOrganizationalUnitResponse' {} a -> s {httpStatus = a} :: CreateOrganizationalUnitResponse)

instance Core.NFData CreateOrganizationalUnitResponse
