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
-- Module      : Amazonka.Organizations.CreateOrganizationalUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- in the /Organizations User Guide./
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission.
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.CreateOrganizationalUnit
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOrganizationalUnit' smart constructor.
data CreateOrganizationalUnit = CreateOrganizationalUnit'
  { -- | A list of tags that you want to attach to the newly created OU. For each
    -- tag in the list, you must specify both a tag key and a value. You can
    -- set the value to an empty string, but you can\'t set it to @null@. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
    -- in the Organizations User Guide.
    --
    -- If any one of the tags is not valid or if you exceed the allowed number
    -- of tags for an OU, then the entire request fails and the OU is not
    -- created.
    tags :: Prelude.Maybe [Tag],
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
    parentId :: Prelude.Text,
    -- | The friendly name to assign to the new OU.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is not valid or if you exceed the allowed number
-- of tags for an OU, then the entire request fails and the OU is not
-- created.
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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateOrganizationalUnit
newCreateOrganizationalUnit pParentId_ pName_ =
  CreateOrganizationalUnit'
    { tags = Prelude.Nothing,
      parentId = pParentId_,
      name = pName_
    }

-- | A list of tags that you want to attach to the newly created OU. For each
-- tag in the list, you must specify both a tag key and a value. You can
-- set the value to an empty string, but you can\'t set it to @null@. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is not valid or if you exceed the allowed number
-- of tags for an OU, then the entire request fails and the OU is not
-- created.
createOrganizationalUnit_tags :: Lens.Lens' CreateOrganizationalUnit (Prelude.Maybe [Tag])
createOrganizationalUnit_tags = Lens.lens (\CreateOrganizationalUnit' {tags} -> tags) (\s@CreateOrganizationalUnit' {} a -> s {tags = a} :: CreateOrganizationalUnit) Prelude.. Lens.mapping Lens.coerced

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
createOrganizationalUnit_parentId :: Lens.Lens' CreateOrganizationalUnit Prelude.Text
createOrganizationalUnit_parentId = Lens.lens (\CreateOrganizationalUnit' {parentId} -> parentId) (\s@CreateOrganizationalUnit' {} a -> s {parentId = a} :: CreateOrganizationalUnit)

-- | The friendly name to assign to the new OU.
createOrganizationalUnit_name :: Lens.Lens' CreateOrganizationalUnit Prelude.Text
createOrganizationalUnit_name = Lens.lens (\CreateOrganizationalUnit' {name} -> name) (\s@CreateOrganizationalUnit' {} a -> s {name = a} :: CreateOrganizationalUnit)

instance Core.AWSRequest CreateOrganizationalUnit where
  type
    AWSResponse CreateOrganizationalUnit =
      CreateOrganizationalUnitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrganizationalUnitResponse'
            Prelude.<$> (x Data..?> "OrganizationalUnit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOrganizationalUnit where
  hashWithSalt _salt CreateOrganizationalUnit' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateOrganizationalUnit where
  rnf CreateOrganizationalUnit' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateOrganizationalUnit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.CreateOrganizationalUnit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateOrganizationalUnit where
  toJSON CreateOrganizationalUnit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ParentId" Data..= parentId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateOrganizationalUnit where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOrganizationalUnit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOrganizationalUnitResponse' smart constructor.
data CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse'
  { -- | A structure that contains details about the newly created OU.
    organizationalUnit :: Prelude.Maybe OrganizationalUnit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateOrganizationalUnitResponse
newCreateOrganizationalUnitResponse pHttpStatus_ =
  CreateOrganizationalUnitResponse'
    { organizationalUnit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the newly created OU.
createOrganizationalUnitResponse_organizationalUnit :: Lens.Lens' CreateOrganizationalUnitResponse (Prelude.Maybe OrganizationalUnit)
createOrganizationalUnitResponse_organizationalUnit = Lens.lens (\CreateOrganizationalUnitResponse' {organizationalUnit} -> organizationalUnit) (\s@CreateOrganizationalUnitResponse' {} a -> s {organizationalUnit = a} :: CreateOrganizationalUnitResponse)

-- | The response's http status code.
createOrganizationalUnitResponse_httpStatus :: Lens.Lens' CreateOrganizationalUnitResponse Prelude.Int
createOrganizationalUnitResponse_httpStatus = Lens.lens (\CreateOrganizationalUnitResponse' {httpStatus} -> httpStatus) (\s@CreateOrganizationalUnitResponse' {} a -> s {httpStatus = a} :: CreateOrganizationalUnitResponse)

instance
  Prelude.NFData
    CreateOrganizationalUnitResponse
  where
  rnf CreateOrganizationalUnitResponse' {..} =
    Prelude.rnf organizationalUnit
      `Prelude.seq` Prelude.rnf httpStatus
