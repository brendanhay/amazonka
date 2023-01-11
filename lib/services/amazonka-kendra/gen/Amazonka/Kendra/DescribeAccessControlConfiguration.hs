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
-- Module      : Amazonka.Kendra.DescribeAccessControlConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an access control configuration that you created
-- for your documents in an index. This includes user and group access
-- information for your documents. This is useful for user context
-- filtering, where search results are filtered based on the user or their
-- group access to documents.
module Amazonka.Kendra.DescribeAccessControlConfiguration
  ( -- * Creating a Request
    DescribeAccessControlConfiguration (..),
    newDescribeAccessControlConfiguration,

    -- * Request Lenses
    describeAccessControlConfiguration_indexId,
    describeAccessControlConfiguration_id,

    -- * Destructuring the Response
    DescribeAccessControlConfigurationResponse (..),
    newDescribeAccessControlConfigurationResponse,

    -- * Response Lenses
    describeAccessControlConfigurationResponse_accessControlList,
    describeAccessControlConfigurationResponse_description,
    describeAccessControlConfigurationResponse_errorMessage,
    describeAccessControlConfigurationResponse_hierarchicalAccessControlList,
    describeAccessControlConfigurationResponse_httpStatus,
    describeAccessControlConfigurationResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccessControlConfiguration' smart constructor.
data DescribeAccessControlConfiguration = DescribeAccessControlConfiguration'
  { -- | The identifier of the index for an access control configuration.
    indexId :: Prelude.Text,
    -- | The identifier of the access control configuration you want to get
    -- information on.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessControlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'describeAccessControlConfiguration_indexId' - The identifier of the index for an access control configuration.
--
-- 'id', 'describeAccessControlConfiguration_id' - The identifier of the access control configuration you want to get
-- information on.
newDescribeAccessControlConfiguration ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DescribeAccessControlConfiguration
newDescribeAccessControlConfiguration pIndexId_ pId_ =
  DescribeAccessControlConfiguration'
    { indexId =
        pIndexId_,
      id = pId_
    }

-- | The identifier of the index for an access control configuration.
describeAccessControlConfiguration_indexId :: Lens.Lens' DescribeAccessControlConfiguration Prelude.Text
describeAccessControlConfiguration_indexId = Lens.lens (\DescribeAccessControlConfiguration' {indexId} -> indexId) (\s@DescribeAccessControlConfiguration' {} a -> s {indexId = a} :: DescribeAccessControlConfiguration)

-- | The identifier of the access control configuration you want to get
-- information on.
describeAccessControlConfiguration_id :: Lens.Lens' DescribeAccessControlConfiguration Prelude.Text
describeAccessControlConfiguration_id = Lens.lens (\DescribeAccessControlConfiguration' {id} -> id) (\s@DescribeAccessControlConfiguration' {} a -> s {id = a} :: DescribeAccessControlConfiguration)

instance
  Core.AWSRequest
    DescribeAccessControlConfiguration
  where
  type
    AWSResponse DescribeAccessControlConfiguration =
      DescribeAccessControlConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccessControlConfigurationResponse'
            Prelude.<$> ( x Data..?> "AccessControlList"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "Description")
              Prelude.<*> (x Data..?> "ErrorMessage")
              Prelude.<*> (x Data..?> "HierarchicalAccessControlList")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "Name")
      )

instance
  Prelude.Hashable
    DescribeAccessControlConfiguration
  where
  hashWithSalt
    _salt
    DescribeAccessControlConfiguration' {..} =
      _salt `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DescribeAccessControlConfiguration
  where
  rnf DescribeAccessControlConfiguration' {..} =
    Prelude.rnf indexId `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DescribeAccessControlConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeAccessControlConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeAccessControlConfiguration
  where
  toJSON DescribeAccessControlConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance
  Data.ToPath
    DescribeAccessControlConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeAccessControlConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccessControlConfigurationResponse' smart constructor.
data DescribeAccessControlConfigurationResponse = DescribeAccessControlConfigurationResponse'
  { -- | Information on principals (users and\/or groups) and which documents
    -- they should have access to. This is useful for user context filtering,
    -- where search results are filtered based on the user or their group
    -- access to documents.
    accessControlList :: Prelude.Maybe [Principal],
    -- | The description for the access control configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The error message containing details if there are issues processing the
    -- access control configuration.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The list of
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
    -- lists that define the hierarchy for which documents users should have
    -- access to.
    hierarchicalAccessControlList :: Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name for the access control configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccessControlConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'describeAccessControlConfigurationResponse_accessControlList' - Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
--
-- 'description', 'describeAccessControlConfigurationResponse_description' - The description for the access control configuration.
--
-- 'errorMessage', 'describeAccessControlConfigurationResponse_errorMessage' - The error message containing details if there are issues processing the
-- access control configuration.
--
-- 'hierarchicalAccessControlList', 'describeAccessControlConfigurationResponse_hierarchicalAccessControlList' - The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
--
-- 'httpStatus', 'describeAccessControlConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeAccessControlConfigurationResponse_name' - The name for the access control configuration.
newDescribeAccessControlConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DescribeAccessControlConfigurationResponse
newDescribeAccessControlConfigurationResponse
  pHttpStatus_
  pName_ =
    DescribeAccessControlConfigurationResponse'
      { accessControlList =
          Prelude.Nothing,
        description = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        hierarchicalAccessControlList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_
      }

-- | Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
describeAccessControlConfigurationResponse_accessControlList :: Lens.Lens' DescribeAccessControlConfigurationResponse (Prelude.Maybe [Principal])
describeAccessControlConfigurationResponse_accessControlList = Lens.lens (\DescribeAccessControlConfigurationResponse' {accessControlList} -> accessControlList) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {accessControlList = a} :: DescribeAccessControlConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description for the access control configuration.
describeAccessControlConfigurationResponse_description :: Lens.Lens' DescribeAccessControlConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAccessControlConfigurationResponse_description = Lens.lens (\DescribeAccessControlConfigurationResponse' {description} -> description) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {description = a} :: DescribeAccessControlConfigurationResponse)

-- | The error message containing details if there are issues processing the
-- access control configuration.
describeAccessControlConfigurationResponse_errorMessage :: Lens.Lens' DescribeAccessControlConfigurationResponse (Prelude.Maybe Prelude.Text)
describeAccessControlConfigurationResponse_errorMessage = Lens.lens (\DescribeAccessControlConfigurationResponse' {errorMessage} -> errorMessage) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {errorMessage = a} :: DescribeAccessControlConfigurationResponse)

-- | The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
describeAccessControlConfigurationResponse_hierarchicalAccessControlList :: Lens.Lens' DescribeAccessControlConfigurationResponse (Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal))
describeAccessControlConfigurationResponse_hierarchicalAccessControlList = Lens.lens (\DescribeAccessControlConfigurationResponse' {hierarchicalAccessControlList} -> hierarchicalAccessControlList) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {hierarchicalAccessControlList = a} :: DescribeAccessControlConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccessControlConfigurationResponse_httpStatus :: Lens.Lens' DescribeAccessControlConfigurationResponse Prelude.Int
describeAccessControlConfigurationResponse_httpStatus = Lens.lens (\DescribeAccessControlConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeAccessControlConfigurationResponse)

-- | The name for the access control configuration.
describeAccessControlConfigurationResponse_name :: Lens.Lens' DescribeAccessControlConfigurationResponse Prelude.Text
describeAccessControlConfigurationResponse_name = Lens.lens (\DescribeAccessControlConfigurationResponse' {name} -> name) (\s@DescribeAccessControlConfigurationResponse' {} a -> s {name = a} :: DescribeAccessControlConfigurationResponse)

instance
  Prelude.NFData
    DescribeAccessControlConfigurationResponse
  where
  rnf DescribeAccessControlConfigurationResponse' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf hierarchicalAccessControlList
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
