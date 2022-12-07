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
-- Module      : Amazonka.AppFlow.CreateConnectorProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new connector profile associated with your Amazon Web Services
-- account. There is a soft quota of 100 connector profiles per Amazon Web
-- Services account. If you need more connector profiles than this quota
-- allows, you can submit a request to the Amazon AppFlow team through the
-- Amazon AppFlow support channel. In each connector profile that you
-- create, you can provide the credentials and properties for only one
-- connector.
module Amazonka.AppFlow.CreateConnectorProfile
  ( -- * Creating a Request
    CreateConnectorProfile (..),
    newCreateConnectorProfile,

    -- * Request Lenses
    createConnectorProfile_kmsArn,
    createConnectorProfile_connectorLabel,
    createConnectorProfile_connectorProfileName,
    createConnectorProfile_connectorType,
    createConnectorProfile_connectionMode,
    createConnectorProfile_connectorProfileConfig,

    -- * Destructuring the Response
    CreateConnectorProfileResponse (..),
    newCreateConnectorProfileResponse,

    -- * Response Lenses
    createConnectorProfileResponse_connectorProfileArn,
    createConnectorProfileResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnectorProfile' smart constructor.
data CreateConnectorProfile = CreateConnectorProfile'
  { -- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
    -- you provide for encryption. This is required if you do not want to use
    -- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
    -- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
    kmsArn :: Prelude.Maybe Prelude.Text,
    -- | The label of the connector. The label is unique for each
    -- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
    -- if calling for CUSTOMCONNECTOR connector type\/.
    connectorLabel :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in your Amazon Web Services account.
    connectorProfileName :: Prelude.Text,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: ConnectorType,
    -- | Indicates the connection mode and specifies whether it is public or
    -- private. Private flows use Amazon Web Services PrivateLink to route data
    -- over Amazon Web Services infrastructure without exposing it to the
    -- public internet.
    connectionMode :: ConnectionMode,
    -- | Defines the connector-specific configuration and credentials.
    connectorProfileConfig :: ConnectorProfileConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsArn', 'createConnectorProfile_kmsArn' - The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
--
-- 'connectorLabel', 'createConnectorProfile_connectorLabel' - The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
--
-- 'connectorProfileName', 'createConnectorProfile_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in your Amazon Web Services account.
--
-- 'connectorType', 'createConnectorProfile_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'connectionMode', 'createConnectorProfile_connectionMode' - Indicates the connection mode and specifies whether it is public or
-- private. Private flows use Amazon Web Services PrivateLink to route data
-- over Amazon Web Services infrastructure without exposing it to the
-- public internet.
--
-- 'connectorProfileConfig', 'createConnectorProfile_connectorProfileConfig' - Defines the connector-specific configuration and credentials.
newCreateConnectorProfile ::
  -- | 'connectorProfileName'
  Prelude.Text ->
  -- | 'connectorType'
  ConnectorType ->
  -- | 'connectionMode'
  ConnectionMode ->
  -- | 'connectorProfileConfig'
  ConnectorProfileConfig ->
  CreateConnectorProfile
newCreateConnectorProfile
  pConnectorProfileName_
  pConnectorType_
  pConnectionMode_
  pConnectorProfileConfig_ =
    CreateConnectorProfile'
      { kmsArn = Prelude.Nothing,
        connectorLabel = Prelude.Nothing,
        connectorProfileName = pConnectorProfileName_,
        connectorType = pConnectorType_,
        connectionMode = pConnectionMode_,
        connectorProfileConfig = pConnectorProfileConfig_
      }

-- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
createConnectorProfile_kmsArn :: Lens.Lens' CreateConnectorProfile (Prelude.Maybe Prelude.Text)
createConnectorProfile_kmsArn = Lens.lens (\CreateConnectorProfile' {kmsArn} -> kmsArn) (\s@CreateConnectorProfile' {} a -> s {kmsArn = a} :: CreateConnectorProfile)

-- | The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account. Only needed
-- if calling for CUSTOMCONNECTOR connector type\/.
createConnectorProfile_connectorLabel :: Lens.Lens' CreateConnectorProfile (Prelude.Maybe Prelude.Text)
createConnectorProfile_connectorLabel = Lens.lens (\CreateConnectorProfile' {connectorLabel} -> connectorLabel) (\s@CreateConnectorProfile' {} a -> s {connectorLabel = a} :: CreateConnectorProfile)

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in your Amazon Web Services account.
createConnectorProfile_connectorProfileName :: Lens.Lens' CreateConnectorProfile Prelude.Text
createConnectorProfile_connectorProfileName = Lens.lens (\CreateConnectorProfile' {connectorProfileName} -> connectorProfileName) (\s@CreateConnectorProfile' {} a -> s {connectorProfileName = a} :: CreateConnectorProfile)

-- | The type of connector, such as Salesforce, Amplitude, and so on.
createConnectorProfile_connectorType :: Lens.Lens' CreateConnectorProfile ConnectorType
createConnectorProfile_connectorType = Lens.lens (\CreateConnectorProfile' {connectorType} -> connectorType) (\s@CreateConnectorProfile' {} a -> s {connectorType = a} :: CreateConnectorProfile)

-- | Indicates the connection mode and specifies whether it is public or
-- private. Private flows use Amazon Web Services PrivateLink to route data
-- over Amazon Web Services infrastructure without exposing it to the
-- public internet.
createConnectorProfile_connectionMode :: Lens.Lens' CreateConnectorProfile ConnectionMode
createConnectorProfile_connectionMode = Lens.lens (\CreateConnectorProfile' {connectionMode} -> connectionMode) (\s@CreateConnectorProfile' {} a -> s {connectionMode = a} :: CreateConnectorProfile)

-- | Defines the connector-specific configuration and credentials.
createConnectorProfile_connectorProfileConfig :: Lens.Lens' CreateConnectorProfile ConnectorProfileConfig
createConnectorProfile_connectorProfileConfig = Lens.lens (\CreateConnectorProfile' {connectorProfileConfig} -> connectorProfileConfig) (\s@CreateConnectorProfile' {} a -> s {connectorProfileConfig = a} :: CreateConnectorProfile)

instance Core.AWSRequest CreateConnectorProfile where
  type
    AWSResponse CreateConnectorProfile =
      CreateConnectorProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectorProfileResponse'
            Prelude.<$> (x Data..?> "connectorProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConnectorProfile where
  hashWithSalt _salt CreateConnectorProfile' {..} =
    _salt `Prelude.hashWithSalt` kmsArn
      `Prelude.hashWithSalt` connectorLabel
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` connectionMode
      `Prelude.hashWithSalt` connectorProfileConfig

instance Prelude.NFData CreateConnectorProfile where
  rnf CreateConnectorProfile' {..} =
    Prelude.rnf kmsArn
      `Prelude.seq` Prelude.rnf connectorLabel
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf connectionMode
      `Prelude.seq` Prelude.rnf connectorProfileConfig

instance Data.ToHeaders CreateConnectorProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnectorProfile where
  toJSON CreateConnectorProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsArn" Data..=) Prelude.<$> kmsArn,
            ("connectorLabel" Data..=)
              Prelude.<$> connectorLabel,
            Prelude.Just
              ( "connectorProfileName"
                  Data..= connectorProfileName
              ),
            Prelude.Just ("connectorType" Data..= connectorType),
            Prelude.Just
              ("connectionMode" Data..= connectionMode),
            Prelude.Just
              ( "connectorProfileConfig"
                  Data..= connectorProfileConfig
              )
          ]
      )

instance Data.ToPath CreateConnectorProfile where
  toPath = Prelude.const "/create-connector-profile"

instance Data.ToQuery CreateConnectorProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectorProfileResponse' smart constructor.
data CreateConnectorProfileResponse = CreateConnectorProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the connector profile.
    connectorProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectorProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileArn', 'createConnectorProfileResponse_connectorProfileArn' - The Amazon Resource Name (ARN) of the connector profile.
--
-- 'httpStatus', 'createConnectorProfileResponse_httpStatus' - The response's http status code.
newCreateConnectorProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConnectorProfileResponse
newCreateConnectorProfileResponse pHttpStatus_ =
  CreateConnectorProfileResponse'
    { connectorProfileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the connector profile.
createConnectorProfileResponse_connectorProfileArn :: Lens.Lens' CreateConnectorProfileResponse (Prelude.Maybe Prelude.Text)
createConnectorProfileResponse_connectorProfileArn = Lens.lens (\CreateConnectorProfileResponse' {connectorProfileArn} -> connectorProfileArn) (\s@CreateConnectorProfileResponse' {} a -> s {connectorProfileArn = a} :: CreateConnectorProfileResponse)

-- | The response's http status code.
createConnectorProfileResponse_httpStatus :: Lens.Lens' CreateConnectorProfileResponse Prelude.Int
createConnectorProfileResponse_httpStatus = Lens.lens (\CreateConnectorProfileResponse' {httpStatus} -> httpStatus) (\s@CreateConnectorProfileResponse' {} a -> s {httpStatus = a} :: CreateConnectorProfileResponse)

instance
  Prelude.NFData
    CreateConnectorProfileResponse
  where
  rnf CreateConnectorProfileResponse' {..} =
    Prelude.rnf connectorProfileArn
      `Prelude.seq` Prelude.rnf httpStatus
