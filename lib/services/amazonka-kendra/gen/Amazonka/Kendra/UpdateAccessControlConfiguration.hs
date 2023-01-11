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
-- Module      : Amazonka.Kendra.UpdateAccessControlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an access control configuration for your documents in an index.
-- This includes user and group access information for your documents. This
-- is useful for user context filtering, where search results are filtered
-- based on the user or their group access to documents.
--
-- You can update an access control configuration you created without
-- indexing all of your documents again. For example, your index contains
-- top-secret company documents that only certain employees or users should
-- access. You created an \'allow\' access control configuration for one
-- user who recently joined the \'top-secret\' team, switching from a team
-- with \'deny\' access to top-secret documents. However, the user suddenly
-- returns to their previous team and should no longer have access to top
-- secret documents. You can update the access control configuration to
-- re-configure access control for your documents as circumstances change.
--
-- You call the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchPutDocument.html BatchPutDocument>
-- API to apply the updated access control configuration, with the
-- @AccessControlConfigurationId@ included in the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Document.html Document>
-- object. If you use an S3 bucket as a data source, you synchronize your
-- data source to apply the @AccessControlConfigurationId@ in the
-- @.metadata.json@ file. Amazon Kendra currently only supports access
-- control configuration for S3 data sources and documents indexed using
-- the @BatchPutDocument@ API.
module Amazonka.Kendra.UpdateAccessControlConfiguration
  ( -- * Creating a Request
    UpdateAccessControlConfiguration (..),
    newUpdateAccessControlConfiguration,

    -- * Request Lenses
    updateAccessControlConfiguration_accessControlList,
    updateAccessControlConfiguration_description,
    updateAccessControlConfiguration_hierarchicalAccessControlList,
    updateAccessControlConfiguration_name,
    updateAccessControlConfiguration_indexId,
    updateAccessControlConfiguration_id,

    -- * Destructuring the Response
    UpdateAccessControlConfigurationResponse (..),
    newUpdateAccessControlConfigurationResponse,

    -- * Response Lenses
    updateAccessControlConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccessControlConfiguration' smart constructor.
data UpdateAccessControlConfiguration = UpdateAccessControlConfiguration'
  { -- | Information you want to update on principals (users and\/or groups) and
    -- which documents they should have access to. This is useful for user
    -- context filtering, where search results are filtered based on the user
    -- or their group access to documents.
    accessControlList :: Prelude.Maybe [Principal],
    -- | A new description for the access control configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated list of
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
    -- lists that define the hierarchy for which documents users should have
    -- access to.
    hierarchicalAccessControlList :: Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal),
    -- | A new name for the access control configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index for an access control configuration.
    indexId :: Prelude.Text,
    -- | The identifier of the access control configuration you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessControlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'updateAccessControlConfiguration_accessControlList' - Information you want to update on principals (users and\/or groups) and
-- which documents they should have access to. This is useful for user
-- context filtering, where search results are filtered based on the user
-- or their group access to documents.
--
-- 'description', 'updateAccessControlConfiguration_description' - A new description for the access control configuration.
--
-- 'hierarchicalAccessControlList', 'updateAccessControlConfiguration_hierarchicalAccessControlList' - The updated list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
--
-- 'name', 'updateAccessControlConfiguration_name' - A new name for the access control configuration.
--
-- 'indexId', 'updateAccessControlConfiguration_indexId' - The identifier of the index for an access control configuration.
--
-- 'id', 'updateAccessControlConfiguration_id' - The identifier of the access control configuration you want to update.
newUpdateAccessControlConfiguration ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  UpdateAccessControlConfiguration
newUpdateAccessControlConfiguration pIndexId_ pId_ =
  UpdateAccessControlConfiguration'
    { accessControlList =
        Prelude.Nothing,
      description = Prelude.Nothing,
      hierarchicalAccessControlList =
        Prelude.Nothing,
      name = Prelude.Nothing,
      indexId = pIndexId_,
      id = pId_
    }

-- | Information you want to update on principals (users and\/or groups) and
-- which documents they should have access to. This is useful for user
-- context filtering, where search results are filtered based on the user
-- or their group access to documents.
updateAccessControlConfiguration_accessControlList :: Lens.Lens' UpdateAccessControlConfiguration (Prelude.Maybe [Principal])
updateAccessControlConfiguration_accessControlList = Lens.lens (\UpdateAccessControlConfiguration' {accessControlList} -> accessControlList) (\s@UpdateAccessControlConfiguration' {} a -> s {accessControlList = a} :: UpdateAccessControlConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A new description for the access control configuration.
updateAccessControlConfiguration_description :: Lens.Lens' UpdateAccessControlConfiguration (Prelude.Maybe Prelude.Text)
updateAccessControlConfiguration_description = Lens.lens (\UpdateAccessControlConfiguration' {description} -> description) (\s@UpdateAccessControlConfiguration' {} a -> s {description = a} :: UpdateAccessControlConfiguration)

-- | The updated list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
updateAccessControlConfiguration_hierarchicalAccessControlList :: Lens.Lens' UpdateAccessControlConfiguration (Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal))
updateAccessControlConfiguration_hierarchicalAccessControlList = Lens.lens (\UpdateAccessControlConfiguration' {hierarchicalAccessControlList} -> hierarchicalAccessControlList) (\s@UpdateAccessControlConfiguration' {} a -> s {hierarchicalAccessControlList = a} :: UpdateAccessControlConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A new name for the access control configuration.
updateAccessControlConfiguration_name :: Lens.Lens' UpdateAccessControlConfiguration (Prelude.Maybe Prelude.Text)
updateAccessControlConfiguration_name = Lens.lens (\UpdateAccessControlConfiguration' {name} -> name) (\s@UpdateAccessControlConfiguration' {} a -> s {name = a} :: UpdateAccessControlConfiguration)

-- | The identifier of the index for an access control configuration.
updateAccessControlConfiguration_indexId :: Lens.Lens' UpdateAccessControlConfiguration Prelude.Text
updateAccessControlConfiguration_indexId = Lens.lens (\UpdateAccessControlConfiguration' {indexId} -> indexId) (\s@UpdateAccessControlConfiguration' {} a -> s {indexId = a} :: UpdateAccessControlConfiguration)

-- | The identifier of the access control configuration you want to update.
updateAccessControlConfiguration_id :: Lens.Lens' UpdateAccessControlConfiguration Prelude.Text
updateAccessControlConfiguration_id = Lens.lens (\UpdateAccessControlConfiguration' {id} -> id) (\s@UpdateAccessControlConfiguration' {} a -> s {id = a} :: UpdateAccessControlConfiguration)

instance
  Core.AWSRequest
    UpdateAccessControlConfiguration
  where
  type
    AWSResponse UpdateAccessControlConfiguration =
      UpdateAccessControlConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAccessControlConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAccessControlConfiguration
  where
  hashWithSalt
    _salt
    UpdateAccessControlConfiguration' {..} =
      _salt `Prelude.hashWithSalt` accessControlList
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` hierarchicalAccessControlList
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateAccessControlConfiguration
  where
  rnf UpdateAccessControlConfiguration' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hierarchicalAccessControlList
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateAccessControlConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateAccessControlConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAccessControlConfiguration where
  toJSON UpdateAccessControlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlList" Data..=)
              Prelude.<$> accessControlList,
            ("Description" Data..=) Prelude.<$> description,
            ("HierarchicalAccessControlList" Data..=)
              Prelude.<$> hierarchicalAccessControlList,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath UpdateAccessControlConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateAccessControlConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccessControlConfigurationResponse' smart constructor.
data UpdateAccessControlConfigurationResponse = UpdateAccessControlConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessControlConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccessControlConfigurationResponse_httpStatus' - The response's http status code.
newUpdateAccessControlConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccessControlConfigurationResponse
newUpdateAccessControlConfigurationResponse
  pHttpStatus_ =
    UpdateAccessControlConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateAccessControlConfigurationResponse_httpStatus :: Lens.Lens' UpdateAccessControlConfigurationResponse Prelude.Int
updateAccessControlConfigurationResponse_httpStatus = Lens.lens (\UpdateAccessControlConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateAccessControlConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateAccessControlConfigurationResponse)

instance
  Prelude.NFData
    UpdateAccessControlConfigurationResponse
  where
  rnf UpdateAccessControlConfigurationResponse' {..} =
    Prelude.rnf httpStatus
