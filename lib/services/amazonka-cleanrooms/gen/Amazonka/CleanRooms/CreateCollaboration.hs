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
-- Module      : Amazonka.CleanRooms.CreateCollaboration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new collaboration.
module Amazonka.CleanRooms.CreateCollaboration
  ( -- * Creating a Request
    CreateCollaboration (..),
    newCreateCollaboration,

    -- * Request Lenses
    createCollaboration_dataEncryptionMetadata,
    createCollaboration_tags,
    createCollaboration_members,
    createCollaboration_name,
    createCollaboration_description,
    createCollaboration_creatorMemberAbilities,
    createCollaboration_creatorDisplayName,
    createCollaboration_queryLogStatus,

    -- * Destructuring the Response
    CreateCollaborationResponse (..),
    newCreateCollaborationResponse,

    -- * Response Lenses
    createCollaborationResponse_httpStatus,
    createCollaborationResponse_collaboration,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCollaboration' smart constructor.
data CreateCollaboration = CreateCollaboration'
  { -- | The settings for client-side encryption with Cryptographic Computing for
    -- Clean Rooms.
    dataEncryptionMetadata :: Prelude.Maybe DataEncryptionMetadata,
    -- | An optional label that you can assign to a resource when you create it.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. When you use tagging, you can also use tag-based access control
    -- in IAM policies to control access to this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of initial members, not including the creator. This list is
    -- immutable.
    members :: [MemberSpecification],
    -- | The display name for a collaboration.
    name :: Prelude.Text,
    -- | A description of the collaboration provided by the collaboration owner.
    description :: Prelude.Text,
    -- | The abilities granted to the collaboration creator.
    creatorMemberAbilities :: [MemberAbility],
    -- | The display name of the collaboration creator.
    creatorDisplayName :: Prelude.Text,
    -- | An indicator as to whether query logging has been enabled or disabled
    -- for the collaboration.
    queryLogStatus :: CollaborationQueryLogStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollaboration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEncryptionMetadata', 'createCollaboration_dataEncryptionMetadata' - The settings for client-side encryption with Cryptographic Computing for
-- Clean Rooms.
--
-- 'tags', 'createCollaboration_tags' - An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
--
-- 'members', 'createCollaboration_members' - A list of initial members, not including the creator. This list is
-- immutable.
--
-- 'name', 'createCollaboration_name' - The display name for a collaboration.
--
-- 'description', 'createCollaboration_description' - A description of the collaboration provided by the collaboration owner.
--
-- 'creatorMemberAbilities', 'createCollaboration_creatorMemberAbilities' - The abilities granted to the collaboration creator.
--
-- 'creatorDisplayName', 'createCollaboration_creatorDisplayName' - The display name of the collaboration creator.
--
-- 'queryLogStatus', 'createCollaboration_queryLogStatus' - An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
newCreateCollaboration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'creatorDisplayName'
  Prelude.Text ->
  -- | 'queryLogStatus'
  CollaborationQueryLogStatus ->
  CreateCollaboration
newCreateCollaboration
  pName_
  pDescription_
  pCreatorDisplayName_
  pQueryLogStatus_ =
    CreateCollaboration'
      { dataEncryptionMetadata =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        members = Prelude.mempty,
        name = pName_,
        description = pDescription_,
        creatorMemberAbilities = Prelude.mempty,
        creatorDisplayName = pCreatorDisplayName_,
        queryLogStatus = pQueryLogStatus_
      }

-- | The settings for client-side encryption with Cryptographic Computing for
-- Clean Rooms.
createCollaboration_dataEncryptionMetadata :: Lens.Lens' CreateCollaboration (Prelude.Maybe DataEncryptionMetadata)
createCollaboration_dataEncryptionMetadata = Lens.lens (\CreateCollaboration' {dataEncryptionMetadata} -> dataEncryptionMetadata) (\s@CreateCollaboration' {} a -> s {dataEncryptionMetadata = a} :: CreateCollaboration)

-- | An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
createCollaboration_tags :: Lens.Lens' CreateCollaboration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCollaboration_tags = Lens.lens (\CreateCollaboration' {tags} -> tags) (\s@CreateCollaboration' {} a -> s {tags = a} :: CreateCollaboration) Prelude.. Lens.mapping Lens.coerced

-- | A list of initial members, not including the creator. This list is
-- immutable.
createCollaboration_members :: Lens.Lens' CreateCollaboration [MemberSpecification]
createCollaboration_members = Lens.lens (\CreateCollaboration' {members} -> members) (\s@CreateCollaboration' {} a -> s {members = a} :: CreateCollaboration) Prelude.. Lens.coerced

-- | The display name for a collaboration.
createCollaboration_name :: Lens.Lens' CreateCollaboration Prelude.Text
createCollaboration_name = Lens.lens (\CreateCollaboration' {name} -> name) (\s@CreateCollaboration' {} a -> s {name = a} :: CreateCollaboration)

-- | A description of the collaboration provided by the collaboration owner.
createCollaboration_description :: Lens.Lens' CreateCollaboration Prelude.Text
createCollaboration_description = Lens.lens (\CreateCollaboration' {description} -> description) (\s@CreateCollaboration' {} a -> s {description = a} :: CreateCollaboration)

-- | The abilities granted to the collaboration creator.
createCollaboration_creatorMemberAbilities :: Lens.Lens' CreateCollaboration [MemberAbility]
createCollaboration_creatorMemberAbilities = Lens.lens (\CreateCollaboration' {creatorMemberAbilities} -> creatorMemberAbilities) (\s@CreateCollaboration' {} a -> s {creatorMemberAbilities = a} :: CreateCollaboration) Prelude.. Lens.coerced

-- | The display name of the collaboration creator.
createCollaboration_creatorDisplayName :: Lens.Lens' CreateCollaboration Prelude.Text
createCollaboration_creatorDisplayName = Lens.lens (\CreateCollaboration' {creatorDisplayName} -> creatorDisplayName) (\s@CreateCollaboration' {} a -> s {creatorDisplayName = a} :: CreateCollaboration)

-- | An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
createCollaboration_queryLogStatus :: Lens.Lens' CreateCollaboration CollaborationQueryLogStatus
createCollaboration_queryLogStatus = Lens.lens (\CreateCollaboration' {queryLogStatus} -> queryLogStatus) (\s@CreateCollaboration' {} a -> s {queryLogStatus = a} :: CreateCollaboration)

instance Core.AWSRequest CreateCollaboration where
  type
    AWSResponse CreateCollaboration =
      CreateCollaborationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCollaborationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "collaboration")
      )

instance Prelude.Hashable CreateCollaboration where
  hashWithSalt _salt CreateCollaboration' {..} =
    _salt
      `Prelude.hashWithSalt` dataEncryptionMetadata
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creatorMemberAbilities
      `Prelude.hashWithSalt` creatorDisplayName
      `Prelude.hashWithSalt` queryLogStatus

instance Prelude.NFData CreateCollaboration where
  rnf CreateCollaboration' {..} =
    Prelude.rnf dataEncryptionMetadata
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creatorMemberAbilities
      `Prelude.seq` Prelude.rnf creatorDisplayName
      `Prelude.seq` Prelude.rnf queryLogStatus

instance Data.ToHeaders CreateCollaboration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCollaboration where
  toJSON CreateCollaboration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataEncryptionMetadata" Data..=)
              Prelude.<$> dataEncryptionMetadata,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("members" Data..= members),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("description" Data..= description),
            Prelude.Just
              ( "creatorMemberAbilities"
                  Data..= creatorMemberAbilities
              ),
            Prelude.Just
              ("creatorDisplayName" Data..= creatorDisplayName),
            Prelude.Just
              ("queryLogStatus" Data..= queryLogStatus)
          ]
      )

instance Data.ToPath CreateCollaboration where
  toPath = Prelude.const "/collaborations"

instance Data.ToQuery CreateCollaboration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCollaborationResponse' smart constructor.
data CreateCollaborationResponse = CreateCollaborationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire created collaboration object.
    collaboration :: Collaboration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCollaborationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCollaborationResponse_httpStatus' - The response's http status code.
--
-- 'collaboration', 'createCollaborationResponse_collaboration' - The entire created collaboration object.
newCreateCollaborationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'collaboration'
  Collaboration ->
  CreateCollaborationResponse
newCreateCollaborationResponse
  pHttpStatus_
  pCollaboration_ =
    CreateCollaborationResponse'
      { httpStatus =
          pHttpStatus_,
        collaboration = pCollaboration_
      }

-- | The response's http status code.
createCollaborationResponse_httpStatus :: Lens.Lens' CreateCollaborationResponse Prelude.Int
createCollaborationResponse_httpStatus = Lens.lens (\CreateCollaborationResponse' {httpStatus} -> httpStatus) (\s@CreateCollaborationResponse' {} a -> s {httpStatus = a} :: CreateCollaborationResponse)

-- | The entire created collaboration object.
createCollaborationResponse_collaboration :: Lens.Lens' CreateCollaborationResponse Collaboration
createCollaborationResponse_collaboration = Lens.lens (\CreateCollaborationResponse' {collaboration} -> collaboration) (\s@CreateCollaborationResponse' {} a -> s {collaboration = a} :: CreateCollaborationResponse)

instance Prelude.NFData CreateCollaborationResponse where
  rnf CreateCollaborationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf collaboration
