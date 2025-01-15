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
-- Module      : Amazonka.AppFlow.DescribeConnectorEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details regarding the entity used with the connector, with a
-- description of the data model for each field in that entity.
module Amazonka.AppFlow.DescribeConnectorEntity
  ( -- * Creating a Request
    DescribeConnectorEntity (..),
    newDescribeConnectorEntity,

    -- * Request Lenses
    describeConnectorEntity_apiVersion,
    describeConnectorEntity_connectorProfileName,
    describeConnectorEntity_connectorType,
    describeConnectorEntity_connectorEntityName,

    -- * Destructuring the Response
    DescribeConnectorEntityResponse (..),
    newDescribeConnectorEntityResponse,

    -- * Response Lenses
    describeConnectorEntityResponse_httpStatus,
    describeConnectorEntityResponse_connectorEntityFields,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConnectorEntity' smart constructor.
data DescribeConnectorEntity = DescribeConnectorEntity'
  { -- | The version of the API that\'s used by the connector.
    apiVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The type of connector application, such as Salesforce, Amplitude, and so
    -- on.
    connectorType :: Prelude.Maybe ConnectorType,
    -- | The entity name for that connector.
    connectorEntityName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiVersion', 'describeConnectorEntity_apiVersion' - The version of the API that\'s used by the connector.
--
-- 'connectorProfileName', 'describeConnectorEntity_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
--
-- 'connectorType', 'describeConnectorEntity_connectorType' - The type of connector application, such as Salesforce, Amplitude, and so
-- on.
--
-- 'connectorEntityName', 'describeConnectorEntity_connectorEntityName' - The entity name for that connector.
newDescribeConnectorEntity ::
  -- | 'connectorEntityName'
  Prelude.Text ->
  DescribeConnectorEntity
newDescribeConnectorEntity pConnectorEntityName_ =
  DescribeConnectorEntity'
    { apiVersion =
        Prelude.Nothing,
      connectorProfileName = Prelude.Nothing,
      connectorType = Prelude.Nothing,
      connectorEntityName = pConnectorEntityName_
    }

-- | The version of the API that\'s used by the connector.
describeConnectorEntity_apiVersion :: Lens.Lens' DescribeConnectorEntity (Prelude.Maybe Prelude.Text)
describeConnectorEntity_apiVersion = Lens.lens (\DescribeConnectorEntity' {apiVersion} -> apiVersion) (\s@DescribeConnectorEntity' {} a -> s {apiVersion = a} :: DescribeConnectorEntity)

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
describeConnectorEntity_connectorProfileName :: Lens.Lens' DescribeConnectorEntity (Prelude.Maybe Prelude.Text)
describeConnectorEntity_connectorProfileName = Lens.lens (\DescribeConnectorEntity' {connectorProfileName} -> connectorProfileName) (\s@DescribeConnectorEntity' {} a -> s {connectorProfileName = a} :: DescribeConnectorEntity)

-- | The type of connector application, such as Salesforce, Amplitude, and so
-- on.
describeConnectorEntity_connectorType :: Lens.Lens' DescribeConnectorEntity (Prelude.Maybe ConnectorType)
describeConnectorEntity_connectorType = Lens.lens (\DescribeConnectorEntity' {connectorType} -> connectorType) (\s@DescribeConnectorEntity' {} a -> s {connectorType = a} :: DescribeConnectorEntity)

-- | The entity name for that connector.
describeConnectorEntity_connectorEntityName :: Lens.Lens' DescribeConnectorEntity Prelude.Text
describeConnectorEntity_connectorEntityName = Lens.lens (\DescribeConnectorEntity' {connectorEntityName} -> connectorEntityName) (\s@DescribeConnectorEntity' {} a -> s {connectorEntityName = a} :: DescribeConnectorEntity)

instance Core.AWSRequest DescribeConnectorEntity where
  type
    AWSResponse DescribeConnectorEntity =
      DescribeConnectorEntityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectorEntityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "connectorEntityFields"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable DescribeConnectorEntity where
  hashWithSalt _salt DescribeConnectorEntity' {..} =
    _salt
      `Prelude.hashWithSalt` apiVersion
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` connectorEntityName

instance Prelude.NFData DescribeConnectorEntity where
  rnf DescribeConnectorEntity' {..} =
    Prelude.rnf apiVersion `Prelude.seq`
      Prelude.rnf connectorProfileName `Prelude.seq`
        Prelude.rnf connectorType `Prelude.seq`
          Prelude.rnf connectorEntityName

instance Data.ToHeaders DescribeConnectorEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnectorEntity where
  toJSON DescribeConnectorEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiVersion" Data..=) Prelude.<$> apiVersion,
            ("connectorProfileName" Data..=)
              Prelude.<$> connectorProfileName,
            ("connectorType" Data..=) Prelude.<$> connectorType,
            Prelude.Just
              ("connectorEntityName" Data..= connectorEntityName)
          ]
      )

instance Data.ToPath DescribeConnectorEntity where
  toPath = Prelude.const "/describe-connector-entity"

instance Data.ToQuery DescribeConnectorEntity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectorEntityResponse' smart constructor.
data DescribeConnectorEntityResponse = DescribeConnectorEntityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the fields for that connector entity. For example, for an
    -- /account/ entity, the fields would be /account name/, /account ID/, and
    -- so on.
    connectorEntityFields :: [ConnectorEntityField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectorEntityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeConnectorEntityResponse_httpStatus' - The response's http status code.
--
-- 'connectorEntityFields', 'describeConnectorEntityResponse_connectorEntityFields' - Describes the fields for that connector entity. For example, for an
-- /account/ entity, the fields would be /account name/, /account ID/, and
-- so on.
newDescribeConnectorEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectorEntityResponse
newDescribeConnectorEntityResponse pHttpStatus_ =
  DescribeConnectorEntityResponse'
    { httpStatus =
        pHttpStatus_,
      connectorEntityFields = Prelude.mempty
    }

-- | The response's http status code.
describeConnectorEntityResponse_httpStatus :: Lens.Lens' DescribeConnectorEntityResponse Prelude.Int
describeConnectorEntityResponse_httpStatus = Lens.lens (\DescribeConnectorEntityResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectorEntityResponse' {} a -> s {httpStatus = a} :: DescribeConnectorEntityResponse)

-- | Describes the fields for that connector entity. For example, for an
-- /account/ entity, the fields would be /account name/, /account ID/, and
-- so on.
describeConnectorEntityResponse_connectorEntityFields :: Lens.Lens' DescribeConnectorEntityResponse [ConnectorEntityField]
describeConnectorEntityResponse_connectorEntityFields = Lens.lens (\DescribeConnectorEntityResponse' {connectorEntityFields} -> connectorEntityFields) (\s@DescribeConnectorEntityResponse' {} a -> s {connectorEntityFields = a} :: DescribeConnectorEntityResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeConnectorEntityResponse
  where
  rnf DescribeConnectorEntityResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf connectorEntityFields
