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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessInstanceLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the logging configuration for the specified Amazon Web Services
-- Verified Access instance.
module Amazonka.EC2.ModifyVerifiedAccessInstanceLoggingConfiguration
  ( -- * Creating a Request
    ModifyVerifiedAccessInstanceLoggingConfiguration (..),
    newModifyVerifiedAccessInstanceLoggingConfiguration,

    -- * Request Lenses
    modifyVerifiedAccessInstanceLoggingConfiguration_clientToken,
    modifyVerifiedAccessInstanceLoggingConfiguration_dryRun,
    modifyVerifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId,
    modifyVerifiedAccessInstanceLoggingConfiguration_accessLogs,

    -- * Destructuring the Response
    ModifyVerifiedAccessInstanceLoggingConfigurationResponse (..),
    newModifyVerifiedAccessInstanceLoggingConfigurationResponse,

    -- * Response Lenses
    modifyVerifiedAccessInstanceLoggingConfigurationResponse_loggingConfiguration,
    modifyVerifiedAccessInstanceLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessInstanceLoggingConfiguration' smart constructor.
data ModifyVerifiedAccessInstanceLoggingConfiguration = ModifyVerifiedAccessInstanceLoggingConfiguration'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access instance.
    verifiedAccessInstanceId :: Prelude.Text,
    -- | The configuration options for Amazon Web Services Verified Access
    -- instances.
    accessLogs :: VerifiedAccessLogOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessInstanceLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessInstanceLoggingConfiguration_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'modifyVerifiedAccessInstanceLoggingConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessInstanceId', 'modifyVerifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId' - The ID of the Amazon Web Services Verified Access instance.
--
-- 'accessLogs', 'modifyVerifiedAccessInstanceLoggingConfiguration_accessLogs' - The configuration options for Amazon Web Services Verified Access
-- instances.
newModifyVerifiedAccessInstanceLoggingConfiguration ::
  -- | 'verifiedAccessInstanceId'
  Prelude.Text ->
  -- | 'accessLogs'
  VerifiedAccessLogOptions ->
  ModifyVerifiedAccessInstanceLoggingConfiguration
newModifyVerifiedAccessInstanceLoggingConfiguration
  pVerifiedAccessInstanceId_
  pAccessLogs_ =
    ModifyVerifiedAccessInstanceLoggingConfiguration'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        verifiedAccessInstanceId =
          pVerifiedAccessInstanceId_,
        accessLogs = pAccessLogs_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessInstanceLoggingConfiguration_clientToken :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfiguration (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessInstanceLoggingConfiguration_clientToken = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfiguration' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessInstanceLoggingConfiguration' {} a -> s {clientToken = a} :: ModifyVerifiedAccessInstanceLoggingConfiguration)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessInstanceLoggingConfiguration_dryRun :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfiguration (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessInstanceLoggingConfiguration_dryRun = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfiguration' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessInstanceLoggingConfiguration' {} a -> s {dryRun = a} :: ModifyVerifiedAccessInstanceLoggingConfiguration)

-- | The ID of the Amazon Web Services Verified Access instance.
modifyVerifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfiguration Prelude.Text
modifyVerifiedAccessInstanceLoggingConfiguration_verifiedAccessInstanceId = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfiguration' {verifiedAccessInstanceId} -> verifiedAccessInstanceId) (\s@ModifyVerifiedAccessInstanceLoggingConfiguration' {} a -> s {verifiedAccessInstanceId = a} :: ModifyVerifiedAccessInstanceLoggingConfiguration)

-- | The configuration options for Amazon Web Services Verified Access
-- instances.
modifyVerifiedAccessInstanceLoggingConfiguration_accessLogs :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfiguration VerifiedAccessLogOptions
modifyVerifiedAccessInstanceLoggingConfiguration_accessLogs = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfiguration' {accessLogs} -> accessLogs) (\s@ModifyVerifiedAccessInstanceLoggingConfiguration' {} a -> s {accessLogs = a} :: ModifyVerifiedAccessInstanceLoggingConfiguration)

instance
  Core.AWSRequest
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  type
    AWSResponse
      ModifyVerifiedAccessInstanceLoggingConfiguration =
      ModifyVerifiedAccessInstanceLoggingConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessInstanceLoggingConfigurationResponse'
            Prelude.<$> (x Data..@? "loggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessInstanceLoggingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` verifiedAccessInstanceId
        `Prelude.hashWithSalt` accessLogs

instance
  Prelude.NFData
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  rnf
    ModifyVerifiedAccessInstanceLoggingConfiguration' {..} =
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf dryRun `Prelude.seq`
          Prelude.rnf verifiedAccessInstanceId `Prelude.seq`
            Prelude.rnf accessLogs

instance
  Data.ToHeaders
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVerifiedAccessInstanceLoggingConfiguration
  where
  toQuery
    ModifyVerifiedAccessInstanceLoggingConfiguration' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "ModifyVerifiedAccessInstanceLoggingConfiguration" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "ClientToken" Data.=: clientToken,
          "DryRun" Data.=: dryRun,
          "VerifiedAccessInstanceId"
            Data.=: verifiedAccessInstanceId,
          "AccessLogs" Data.=: accessLogs
        ]

-- | /See:/ 'newModifyVerifiedAccessInstanceLoggingConfigurationResponse' smart constructor.
data ModifyVerifiedAccessInstanceLoggingConfigurationResponse = ModifyVerifiedAccessInstanceLoggingConfigurationResponse'
  { -- | The logging configuration for Amazon Web Services Verified Access
    -- instance.
    loggingConfiguration :: Prelude.Maybe VerifiedAccessInstanceLoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessInstanceLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'modifyVerifiedAccessInstanceLoggingConfigurationResponse_loggingConfiguration' - The logging configuration for Amazon Web Services Verified Access
-- instance.
--
-- 'httpStatus', 'modifyVerifiedAccessInstanceLoggingConfigurationResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessInstanceLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessInstanceLoggingConfigurationResponse
newModifyVerifiedAccessInstanceLoggingConfigurationResponse
  pHttpStatus_ =
    ModifyVerifiedAccessInstanceLoggingConfigurationResponse'
      { loggingConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The logging configuration for Amazon Web Services Verified Access
-- instance.
modifyVerifiedAccessInstanceLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfigurationResponse (Prelude.Maybe VerifiedAccessInstanceLoggingConfiguration)
modifyVerifiedAccessInstanceLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@ModifyVerifiedAccessInstanceLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: ModifyVerifiedAccessInstanceLoggingConfigurationResponse)

-- | The response's http status code.
modifyVerifiedAccessInstanceLoggingConfigurationResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessInstanceLoggingConfigurationResponse Prelude.Int
modifyVerifiedAccessInstanceLoggingConfigurationResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessInstanceLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessInstanceLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessInstanceLoggingConfigurationResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessInstanceLoggingConfigurationResponse
  where
  rnf
    ModifyVerifiedAccessInstanceLoggingConfigurationResponse' {..} =
      Prelude.rnf loggingConfiguration `Prelude.seq`
        Prelude.rnf httpStatus
