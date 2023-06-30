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
-- Module      : Amazonka.Kendra.CreateAccessControlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access configuration for your documents. This includes user
-- and group access information for your documents. This is useful for user
-- context filtering, where search results are filtered based on the user
-- or their group access to documents.
--
-- You can use this to re-configure your existing document level access
-- control without indexing all of your documents again. For example, your
-- index contains top-secret company documents that only certain employees
-- or users should access. One of these users leaves the company or
-- switches to a team that should be blocked from accessing top-secret
-- documents. The user still has access to top-secret documents because the
-- user had access when your documents were previously indexed. You can
-- create a specific access control configuration for the user with deny
-- access. You can later update the access control configuration to allow
-- access if the user returns to the company and re-joins the
-- \'top-secret\' team. You can re-configure access control for your
-- documents as circumstances change.
--
-- To apply your access control configuration to certain documents, you
-- call the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchPutDocument.html BatchPutDocument>
-- API with the @AccessControlConfigurationId@ included in the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Document.html Document>
-- object. If you use an S3 bucket as a data source, you update the
-- @.metadata.json@ with the @AccessControlConfigurationId@ and synchronize
-- your data source. Amazon Kendra currently only supports access control
-- configuration for S3 data sources and documents indexed using the
-- @BatchPutDocument@ API.
module Amazonka.Kendra.CreateAccessControlConfiguration
  ( -- * Creating a Request
    CreateAccessControlConfiguration (..),
    newCreateAccessControlConfiguration,

    -- * Request Lenses
    createAccessControlConfiguration_accessControlList,
    createAccessControlConfiguration_clientToken,
    createAccessControlConfiguration_description,
    createAccessControlConfiguration_hierarchicalAccessControlList,
    createAccessControlConfiguration_indexId,
    createAccessControlConfiguration_name,

    -- * Destructuring the Response
    CreateAccessControlConfigurationResponse (..),
    newCreateAccessControlConfigurationResponse,

    -- * Response Lenses
    createAccessControlConfigurationResponse_httpStatus,
    createAccessControlConfigurationResponse_id,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessControlConfiguration' smart constructor.
data CreateAccessControlConfiguration = CreateAccessControlConfiguration'
  { -- | Information on principals (users and\/or groups) and which documents
    -- they should have access to. This is useful for user context filtering,
    -- where search results are filtered based on the user or their group
    -- access to documents.
    accessControlList :: Prelude.Maybe [Principal],
    -- | A token that you provide to identify the request to create an access
    -- control configuration. Multiple calls to the
    -- @CreateAccessControlConfiguration@ API with the same client token will
    -- create only one access control configuration.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the access control configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
    -- lists that define the hierarchy for which documents users should have
    -- access to.
    hierarchicalAccessControlList :: Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal),
    -- | The identifier of the index to create an access control configuration
    -- for your documents.
    indexId :: Prelude.Text,
    -- | A name for the access control configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessControlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'createAccessControlConfiguration_accessControlList' - Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
--
-- 'clientToken', 'createAccessControlConfiguration_clientToken' - A token that you provide to identify the request to create an access
-- control configuration. Multiple calls to the
-- @CreateAccessControlConfiguration@ API with the same client token will
-- create only one access control configuration.
--
-- 'description', 'createAccessControlConfiguration_description' - A description for the access control configuration.
--
-- 'hierarchicalAccessControlList', 'createAccessControlConfiguration_hierarchicalAccessControlList' - The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
--
-- 'indexId', 'createAccessControlConfiguration_indexId' - The identifier of the index to create an access control configuration
-- for your documents.
--
-- 'name', 'createAccessControlConfiguration_name' - A name for the access control configuration.
newCreateAccessControlConfiguration ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateAccessControlConfiguration
newCreateAccessControlConfiguration pIndexId_ pName_ =
  CreateAccessControlConfiguration'
    { accessControlList =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      hierarchicalAccessControlList =
        Prelude.Nothing,
      indexId = pIndexId_,
      name = pName_
    }

-- | Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
createAccessControlConfiguration_accessControlList :: Lens.Lens' CreateAccessControlConfiguration (Prelude.Maybe [Principal])
createAccessControlConfiguration_accessControlList = Lens.lens (\CreateAccessControlConfiguration' {accessControlList} -> accessControlList) (\s@CreateAccessControlConfiguration' {} a -> s {accessControlList = a} :: CreateAccessControlConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A token that you provide to identify the request to create an access
-- control configuration. Multiple calls to the
-- @CreateAccessControlConfiguration@ API with the same client token will
-- create only one access control configuration.
createAccessControlConfiguration_clientToken :: Lens.Lens' CreateAccessControlConfiguration (Prelude.Maybe Prelude.Text)
createAccessControlConfiguration_clientToken = Lens.lens (\CreateAccessControlConfiguration' {clientToken} -> clientToken) (\s@CreateAccessControlConfiguration' {} a -> s {clientToken = a} :: CreateAccessControlConfiguration)

-- | A description for the access control configuration.
createAccessControlConfiguration_description :: Lens.Lens' CreateAccessControlConfiguration (Prelude.Maybe Prelude.Text)
createAccessControlConfiguration_description = Lens.lens (\CreateAccessControlConfiguration' {description} -> description) (\s@CreateAccessControlConfiguration' {} a -> s {description = a} :: CreateAccessControlConfiguration)

-- | The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
createAccessControlConfiguration_hierarchicalAccessControlList :: Lens.Lens' CreateAccessControlConfiguration (Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal))
createAccessControlConfiguration_hierarchicalAccessControlList = Lens.lens (\CreateAccessControlConfiguration' {hierarchicalAccessControlList} -> hierarchicalAccessControlList) (\s@CreateAccessControlConfiguration' {} a -> s {hierarchicalAccessControlList = a} :: CreateAccessControlConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index to create an access control configuration
-- for your documents.
createAccessControlConfiguration_indexId :: Lens.Lens' CreateAccessControlConfiguration Prelude.Text
createAccessControlConfiguration_indexId = Lens.lens (\CreateAccessControlConfiguration' {indexId} -> indexId) (\s@CreateAccessControlConfiguration' {} a -> s {indexId = a} :: CreateAccessControlConfiguration)

-- | A name for the access control configuration.
createAccessControlConfiguration_name :: Lens.Lens' CreateAccessControlConfiguration Prelude.Text
createAccessControlConfiguration_name = Lens.lens (\CreateAccessControlConfiguration' {name} -> name) (\s@CreateAccessControlConfiguration' {} a -> s {name = a} :: CreateAccessControlConfiguration)

instance
  Core.AWSRequest
    CreateAccessControlConfiguration
  where
  type
    AWSResponse CreateAccessControlConfiguration =
      CreateAccessControlConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessControlConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Id")
      )

instance
  Prelude.Hashable
    CreateAccessControlConfiguration
  where
  hashWithSalt
    _salt
    CreateAccessControlConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` accessControlList
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` hierarchicalAccessControlList
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    CreateAccessControlConfiguration
  where
  rnf CreateAccessControlConfiguration' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hierarchicalAccessControlList
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    CreateAccessControlConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateAccessControlConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessControlConfiguration where
  toJSON CreateAccessControlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlList" Data..=)
              Prelude.<$> accessControlList,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("HierarchicalAccessControlList" Data..=)
              Prelude.<$> hierarchicalAccessControlList,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateAccessControlConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateAccessControlConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessControlConfigurationResponse' smart constructor.
data CreateAccessControlConfigurationResponse = CreateAccessControlConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the access control configuration for your documents in
    -- an index.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessControlConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessControlConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createAccessControlConfigurationResponse_id' - The identifier of the access control configuration for your documents in
-- an index.
newCreateAccessControlConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  CreateAccessControlConfigurationResponse
newCreateAccessControlConfigurationResponse
  pHttpStatus_
  pId_ =
    CreateAccessControlConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        id = pId_
      }

-- | The response's http status code.
createAccessControlConfigurationResponse_httpStatus :: Lens.Lens' CreateAccessControlConfigurationResponse Prelude.Int
createAccessControlConfigurationResponse_httpStatus = Lens.lens (\CreateAccessControlConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateAccessControlConfigurationResponse' {} a -> s {httpStatus = a} :: CreateAccessControlConfigurationResponse)

-- | The identifier of the access control configuration for your documents in
-- an index.
createAccessControlConfigurationResponse_id :: Lens.Lens' CreateAccessControlConfigurationResponse Prelude.Text
createAccessControlConfigurationResponse_id = Lens.lens (\CreateAccessControlConfigurationResponse' {id} -> id) (\s@CreateAccessControlConfigurationResponse' {} a -> s {id = a} :: CreateAccessControlConfigurationResponse)

instance
  Prelude.NFData
    CreateAccessControlConfigurationResponse
  where
  rnf CreateAccessControlConfigurationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq` Prelude.rnf id
