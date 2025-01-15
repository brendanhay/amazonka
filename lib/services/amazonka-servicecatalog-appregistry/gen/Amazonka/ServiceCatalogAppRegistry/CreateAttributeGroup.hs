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
-- Module      : Amazonka.ServiceCatalogAppRegistry.CreateAttributeGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new attribute group as a container for user-defined
-- attributes. This feature enables users to have full control over their
-- cloud application\'s metadata in a rich machine-readable format to
-- facilitate integration with automated workflows and third-party tools.
module Amazonka.ServiceCatalogAppRegistry.CreateAttributeGroup
  ( -- * Creating a Request
    CreateAttributeGroup (..),
    newCreateAttributeGroup,

    -- * Request Lenses
    createAttributeGroup_description,
    createAttributeGroup_tags,
    createAttributeGroup_name,
    createAttributeGroup_attributes,
    createAttributeGroup_clientToken,

    -- * Destructuring the Response
    CreateAttributeGroupResponse (..),
    newCreateAttributeGroupResponse,

    -- * Response Lenses
    createAttributeGroupResponse_attributeGroup,
    createAttributeGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newCreateAttributeGroup' smart constructor.
data CreateAttributeGroup = CreateAttributeGroup'
  { -- | The description of the attribute group that the user provides.
    description :: Prelude.Maybe Prelude.Text,
    -- | Key-value pairs you can use to associate with the attribute group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the attribute group.
    name :: Prelude.Text,
    -- | A JSON string in the form of nested key-value pairs that represent the
    -- attributes in the group and describes an application and its components.
    attributes :: Prelude.Text,
    -- | A unique identifier that you provide to ensure idempotency. If you retry
    -- a request that completed successfully using the same client token and
    -- the same parameters, the retry succeeds without performing any further
    -- actions. If you retry a successful request using the same client token,
    -- but one or more of the parameters are different, the retry fails.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttributeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAttributeGroup_description' - The description of the attribute group that the user provides.
--
-- 'tags', 'createAttributeGroup_tags' - Key-value pairs you can use to associate with the attribute group.
--
-- 'name', 'createAttributeGroup_name' - The name of the attribute group.
--
-- 'attributes', 'createAttributeGroup_attributes' - A JSON string in the form of nested key-value pairs that represent the
-- attributes in the group and describes an application and its components.
--
-- 'clientToken', 'createAttributeGroup_clientToken' - A unique identifier that you provide to ensure idempotency. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, the retry fails.
newCreateAttributeGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'attributes'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateAttributeGroup
newCreateAttributeGroup
  pName_
  pAttributes_
  pClientToken_ =
    CreateAttributeGroup'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        attributes = pAttributes_,
        clientToken = pClientToken_
      }

-- | The description of the attribute group that the user provides.
createAttributeGroup_description :: Lens.Lens' CreateAttributeGroup (Prelude.Maybe Prelude.Text)
createAttributeGroup_description = Lens.lens (\CreateAttributeGroup' {description} -> description) (\s@CreateAttributeGroup' {} a -> s {description = a} :: CreateAttributeGroup)

-- | Key-value pairs you can use to associate with the attribute group.
createAttributeGroup_tags :: Lens.Lens' CreateAttributeGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAttributeGroup_tags = Lens.lens (\CreateAttributeGroup' {tags} -> tags) (\s@CreateAttributeGroup' {} a -> s {tags = a} :: CreateAttributeGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the attribute group.
createAttributeGroup_name :: Lens.Lens' CreateAttributeGroup Prelude.Text
createAttributeGroup_name = Lens.lens (\CreateAttributeGroup' {name} -> name) (\s@CreateAttributeGroup' {} a -> s {name = a} :: CreateAttributeGroup)

-- | A JSON string in the form of nested key-value pairs that represent the
-- attributes in the group and describes an application and its components.
createAttributeGroup_attributes :: Lens.Lens' CreateAttributeGroup Prelude.Text
createAttributeGroup_attributes = Lens.lens (\CreateAttributeGroup' {attributes} -> attributes) (\s@CreateAttributeGroup' {} a -> s {attributes = a} :: CreateAttributeGroup)

-- | A unique identifier that you provide to ensure idempotency. If you retry
-- a request that completed successfully using the same client token and
-- the same parameters, the retry succeeds without performing any further
-- actions. If you retry a successful request using the same client token,
-- but one or more of the parameters are different, the retry fails.
createAttributeGroup_clientToken :: Lens.Lens' CreateAttributeGroup Prelude.Text
createAttributeGroup_clientToken = Lens.lens (\CreateAttributeGroup' {clientToken} -> clientToken) (\s@CreateAttributeGroup' {} a -> s {clientToken = a} :: CreateAttributeGroup)

instance Core.AWSRequest CreateAttributeGroup where
  type
    AWSResponse CreateAttributeGroup =
      CreateAttributeGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAttributeGroupResponse'
            Prelude.<$> (x Data..?> "attributeGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAttributeGroup where
  hashWithSalt _salt CreateAttributeGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateAttributeGroup where
  rnf CreateAttributeGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf attributes `Prelude.seq`
            Prelude.rnf clientToken

instance Data.ToHeaders CreateAttributeGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAttributeGroup where
  toJSON CreateAttributeGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("attributes" Data..= attributes),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateAttributeGroup where
  toPath = Prelude.const "/attribute-groups"

instance Data.ToQuery CreateAttributeGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAttributeGroupResponse' smart constructor.
data CreateAttributeGroupResponse = CreateAttributeGroupResponse'
  { -- | Information about the attribute group.
    attributeGroup :: Prelude.Maybe AttributeGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttributeGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeGroup', 'createAttributeGroupResponse_attributeGroup' - Information about the attribute group.
--
-- 'httpStatus', 'createAttributeGroupResponse_httpStatus' - The response's http status code.
newCreateAttributeGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAttributeGroupResponse
newCreateAttributeGroupResponse pHttpStatus_ =
  CreateAttributeGroupResponse'
    { attributeGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attribute group.
createAttributeGroupResponse_attributeGroup :: Lens.Lens' CreateAttributeGroupResponse (Prelude.Maybe AttributeGroup)
createAttributeGroupResponse_attributeGroup = Lens.lens (\CreateAttributeGroupResponse' {attributeGroup} -> attributeGroup) (\s@CreateAttributeGroupResponse' {} a -> s {attributeGroup = a} :: CreateAttributeGroupResponse)

-- | The response's http status code.
createAttributeGroupResponse_httpStatus :: Lens.Lens' CreateAttributeGroupResponse Prelude.Int
createAttributeGroupResponse_httpStatus = Lens.lens (\CreateAttributeGroupResponse' {httpStatus} -> httpStatus) (\s@CreateAttributeGroupResponse' {} a -> s {httpStatus = a} :: CreateAttributeGroupResponse)

instance Prelude.NFData CreateAttributeGroupResponse where
  rnf CreateAttributeGroupResponse' {..} =
    Prelude.rnf attributeGroup `Prelude.seq`
      Prelude.rnf httpStatus
