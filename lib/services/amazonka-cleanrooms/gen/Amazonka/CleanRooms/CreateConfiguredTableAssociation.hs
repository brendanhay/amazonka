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
-- Module      : Amazonka.CleanRooms.CreateConfiguredTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configured table association. A configured table association
-- links a configured table with a collaboration.
module Amazonka.CleanRooms.CreateConfiguredTableAssociation
  ( -- * Creating a Request
    CreateConfiguredTableAssociation (..),
    newCreateConfiguredTableAssociation,

    -- * Request Lenses
    createConfiguredTableAssociation_description,
    createConfiguredTableAssociation_tags,
    createConfiguredTableAssociation_name,
    createConfiguredTableAssociation_membershipIdentifier,
    createConfiguredTableAssociation_configuredTableIdentifier,
    createConfiguredTableAssociation_roleArn,

    -- * Destructuring the Response
    CreateConfiguredTableAssociationResponse (..),
    newCreateConfiguredTableAssociationResponse,

    -- * Response Lenses
    createConfiguredTableAssociationResponse_httpStatus,
    createConfiguredTableAssociationResponse_configuredTableAssociation,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfiguredTableAssociation' smart constructor.
data CreateConfiguredTableAssociation = CreateConfiguredTableAssociation'
  { -- | A description for the configured table association.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional label that you can assign to a resource when you create it.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. When you use tagging, you can also use tag-based access control
    -- in IAM policies to control access to this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the configured table association. This name is used to query
    -- the underlying configured table.
    name :: Prelude.Text,
    -- | A unique identifier for one of your memberships for a collaboration. The
    -- configured table is associated to the collaboration that this membership
    -- belongs to. Currently accepts a membership ID.
    membershipIdentifier :: Prelude.Text,
    -- | A unique identifier for the configured table to be associated to.
    -- Currently accepts a configured table ID.
    configuredTableIdentifier :: Prelude.Text,
    -- | The service will assume this role to access catalog metadata and query
    -- the table.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createConfiguredTableAssociation_description' - A description for the configured table association.
--
-- 'tags', 'createConfiguredTableAssociation_tags' - An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
--
-- 'name', 'createConfiguredTableAssociation_name' - The name of the configured table association. This name is used to query
-- the underlying configured table.
--
-- 'membershipIdentifier', 'createConfiguredTableAssociation_membershipIdentifier' - A unique identifier for one of your memberships for a collaboration. The
-- configured table is associated to the collaboration that this membership
-- belongs to. Currently accepts a membership ID.
--
-- 'configuredTableIdentifier', 'createConfiguredTableAssociation_configuredTableIdentifier' - A unique identifier for the configured table to be associated to.
-- Currently accepts a configured table ID.
--
-- 'roleArn', 'createConfiguredTableAssociation_roleArn' - The service will assume this role to access catalog metadata and query
-- the table.
newCreateConfiguredTableAssociation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'membershipIdentifier'
  Prelude.Text ->
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateConfiguredTableAssociation
newCreateConfiguredTableAssociation
  pName_
  pMembershipIdentifier_
  pConfiguredTableIdentifier_
  pRoleArn_ =
    CreateConfiguredTableAssociation'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        membershipIdentifier =
          pMembershipIdentifier_,
        configuredTableIdentifier =
          pConfiguredTableIdentifier_,
        roleArn = pRoleArn_
      }

-- | A description for the configured table association.
createConfiguredTableAssociation_description :: Lens.Lens' CreateConfiguredTableAssociation (Prelude.Maybe Prelude.Text)
createConfiguredTableAssociation_description = Lens.lens (\CreateConfiguredTableAssociation' {description} -> description) (\s@CreateConfiguredTableAssociation' {} a -> s {description = a} :: CreateConfiguredTableAssociation)

-- | An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
createConfiguredTableAssociation_tags :: Lens.Lens' CreateConfiguredTableAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfiguredTableAssociation_tags = Lens.lens (\CreateConfiguredTableAssociation' {tags} -> tags) (\s@CreateConfiguredTableAssociation' {} a -> s {tags = a} :: CreateConfiguredTableAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configured table association. This name is used to query
-- the underlying configured table.
createConfiguredTableAssociation_name :: Lens.Lens' CreateConfiguredTableAssociation Prelude.Text
createConfiguredTableAssociation_name = Lens.lens (\CreateConfiguredTableAssociation' {name} -> name) (\s@CreateConfiguredTableAssociation' {} a -> s {name = a} :: CreateConfiguredTableAssociation)

-- | A unique identifier for one of your memberships for a collaboration. The
-- configured table is associated to the collaboration that this membership
-- belongs to. Currently accepts a membership ID.
createConfiguredTableAssociation_membershipIdentifier :: Lens.Lens' CreateConfiguredTableAssociation Prelude.Text
createConfiguredTableAssociation_membershipIdentifier = Lens.lens (\CreateConfiguredTableAssociation' {membershipIdentifier} -> membershipIdentifier) (\s@CreateConfiguredTableAssociation' {} a -> s {membershipIdentifier = a} :: CreateConfiguredTableAssociation)

-- | A unique identifier for the configured table to be associated to.
-- Currently accepts a configured table ID.
createConfiguredTableAssociation_configuredTableIdentifier :: Lens.Lens' CreateConfiguredTableAssociation Prelude.Text
createConfiguredTableAssociation_configuredTableIdentifier = Lens.lens (\CreateConfiguredTableAssociation' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@CreateConfiguredTableAssociation' {} a -> s {configuredTableIdentifier = a} :: CreateConfiguredTableAssociation)

-- | The service will assume this role to access catalog metadata and query
-- the table.
createConfiguredTableAssociation_roleArn :: Lens.Lens' CreateConfiguredTableAssociation Prelude.Text
createConfiguredTableAssociation_roleArn = Lens.lens (\CreateConfiguredTableAssociation' {roleArn} -> roleArn) (\s@CreateConfiguredTableAssociation' {} a -> s {roleArn = a} :: CreateConfiguredTableAssociation)

instance
  Core.AWSRequest
    CreateConfiguredTableAssociation
  where
  type
    AWSResponse CreateConfiguredTableAssociation =
      CreateConfiguredTableAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfiguredTableAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTableAssociation")
      )

instance
  Prelude.Hashable
    CreateConfiguredTableAssociation
  where
  hashWithSalt
    _salt
    CreateConfiguredTableAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` membershipIdentifier
        `Prelude.hashWithSalt` configuredTableIdentifier
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CreateConfiguredTableAssociation
  where
  rnf CreateConfiguredTableAssociation' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf membershipIdentifier
      `Prelude.seq` Prelude.rnf configuredTableIdentifier
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToHeaders
    CreateConfiguredTableAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConfiguredTableAssociation where
  toJSON CreateConfiguredTableAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "configuredTableIdentifier"
                  Data..= configuredTableIdentifier
              ),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateConfiguredTableAssociation where
  toPath CreateConfiguredTableAssociation' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/configuredTableAssociations"
      ]

instance
  Data.ToQuery
    CreateConfiguredTableAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfiguredTableAssociationResponse' smart constructor.
data CreateConfiguredTableAssociationResponse = CreateConfiguredTableAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire configured table association object.
    configuredTableAssociation :: ConfiguredTableAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTableAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfiguredTableAssociationResponse_httpStatus' - The response's http status code.
--
-- 'configuredTableAssociation', 'createConfiguredTableAssociationResponse_configuredTableAssociation' - The entire configured table association object.
newCreateConfiguredTableAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTableAssociation'
  ConfiguredTableAssociation ->
  CreateConfiguredTableAssociationResponse
newCreateConfiguredTableAssociationResponse
  pHttpStatus_
  pConfiguredTableAssociation_ =
    CreateConfiguredTableAssociationResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTableAssociation =
          pConfiguredTableAssociation_
      }

-- | The response's http status code.
createConfiguredTableAssociationResponse_httpStatus :: Lens.Lens' CreateConfiguredTableAssociationResponse Prelude.Int
createConfiguredTableAssociationResponse_httpStatus = Lens.lens (\CreateConfiguredTableAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateConfiguredTableAssociationResponse' {} a -> s {httpStatus = a} :: CreateConfiguredTableAssociationResponse)

-- | The entire configured table association object.
createConfiguredTableAssociationResponse_configuredTableAssociation :: Lens.Lens' CreateConfiguredTableAssociationResponse ConfiguredTableAssociation
createConfiguredTableAssociationResponse_configuredTableAssociation = Lens.lens (\CreateConfiguredTableAssociationResponse' {configuredTableAssociation} -> configuredTableAssociation) (\s@CreateConfiguredTableAssociationResponse' {} a -> s {configuredTableAssociation = a} :: CreateConfiguredTableAssociationResponse)

instance
  Prelude.NFData
    CreateConfiguredTableAssociationResponse
  where
  rnf CreateConfiguredTableAssociationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTableAssociation
