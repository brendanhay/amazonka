{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ModifyInstanceMetadataOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the instance metadata parameters on a running or stopped
-- instance. When you modify the parameters on a stopped instance, they are
-- applied when the instance is started. When you modify the parameters on
-- a running instance, the API responds with a state of “pending”. After
-- the parameter modifications are successfully applied to the instance,
-- the state of the modifications changes from “pending” to “applied” in
-- subsequent describe-instances API calls. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.ModifyInstanceMetadataOptions
  ( -- * Creating a Request
    ModifyInstanceMetadataOptions (..),
    newModifyInstanceMetadataOptions,

    -- * Request Lenses
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_instanceId,

    -- * Destructuring the Response
    ModifyInstanceMetadataOptionsResponse (..),
    newModifyInstanceMetadataOptionsResponse,

    -- * Response Lenses
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceMetadataOptions' smart constructor.
data ModifyInstanceMetadataOptions = ModifyInstanceMetadataOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | This parameter enables or disables the HTTP metadata endpoint on your
    -- instances. If the parameter is not specified, the existing state is
    -- maintained.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel. If no parameter is specified, the existing state is maintained.
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of token usage for your instance metadata requests. If the
    -- parameter is not specified in the request, the default state is
    -- @optional@.
    --
    -- If the state is @optional@, you can choose to retrieve instance metadata
    -- with or without a signed token header on your request. If you retrieve
    -- the IAM role credentials without a token, the version 1.0 role
    -- credentials are returned. If you retrieve the IAM role credentials using
    -- a valid signed token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a signed token header with any
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credential always returns the version 2.0 credentials; the version
    -- 1.0 credentials are not available.
    httpTokens :: Prelude.Maybe HttpTokensState,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyInstanceMetadataOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'httpEndpoint', 'modifyInstanceMetadataOptions_httpEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the existing state is
-- maintained.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
--
-- 'httpPutResponseHopLimit', 'modifyInstanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'modifyInstanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
--
-- 'instanceId', 'modifyInstanceMetadataOptions_instanceId' - The ID of the instance.
newModifyInstanceMetadataOptions ::
  -- | 'instanceId'
  Prelude.Text ->
  ModifyInstanceMetadataOptions
newModifyInstanceMetadataOptions pInstanceId_ =
  ModifyInstanceMetadataOptions'
    { dryRun =
        Prelude.Nothing,
      httpEndpoint = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceMetadataOptions_dryRun :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe Prelude.Bool)
modifyInstanceMetadataOptions_dryRun = Lens.lens (\ModifyInstanceMetadataOptions' {dryRun} -> dryRun) (\s@ModifyInstanceMetadataOptions' {} a -> s {dryRun = a} :: ModifyInstanceMetadataOptions)

-- | This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the existing state is
-- maintained.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
modifyInstanceMetadataOptions_httpEndpoint :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe InstanceMetadataEndpointState)
modifyInstanceMetadataOptions_httpEndpoint = Lens.lens (\ModifyInstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: ModifyInstanceMetadataOptions)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
modifyInstanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe Prelude.Int)
modifyInstanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\ModifyInstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: ModifyInstanceMetadataOptions)

-- | The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credential always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
modifyInstanceMetadataOptions_httpTokens :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe HttpTokensState)
modifyInstanceMetadataOptions_httpTokens = Lens.lens (\ModifyInstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpTokens = a} :: ModifyInstanceMetadataOptions)

-- | The ID of the instance.
modifyInstanceMetadataOptions_instanceId :: Lens.Lens' ModifyInstanceMetadataOptions Prelude.Text
modifyInstanceMetadataOptions_instanceId = Lens.lens (\ModifyInstanceMetadataOptions' {instanceId} -> instanceId) (\s@ModifyInstanceMetadataOptions' {} a -> s {instanceId = a} :: ModifyInstanceMetadataOptions)

instance
  Prelude.AWSRequest
    ModifyInstanceMetadataOptions
  where
  type
    Rs ModifyInstanceMetadataOptions =
      ModifyInstanceMetadataOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceMetadataOptionsResponse'
            Prelude.<$> (x Prelude..@? "instanceMetadataOptions")
            Prelude.<*> (x Prelude..@? "instanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceMetadataOptions

instance Prelude.NFData ModifyInstanceMetadataOptions

instance
  Prelude.ToHeaders
    ModifyInstanceMetadataOptions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyInstanceMetadataOptions where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyInstanceMetadataOptions
  where
  toQuery ModifyInstanceMetadataOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyInstanceMetadataOptions" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "HttpEndpoint" Prelude.=: httpEndpoint,
        "HttpPutResponseHopLimit"
          Prelude.=: httpPutResponseHopLimit,
        "HttpTokens" Prelude.=: httpTokens,
        "InstanceId" Prelude.=: instanceId
      ]

-- | /See:/ 'newModifyInstanceMetadataOptionsResponse' smart constructor.
data ModifyInstanceMetadataOptionsResponse = ModifyInstanceMetadataOptionsResponse'
  { -- | The metadata options for the instance.
    instanceMetadataOptions :: Prelude.Maybe InstanceMetadataOptionsResponse,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceMetadataOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceMetadataOptions', 'modifyInstanceMetadataOptionsResponse_instanceMetadataOptions' - The metadata options for the instance.
--
-- 'instanceId', 'modifyInstanceMetadataOptionsResponse_instanceId' - The ID of the instance.
--
-- 'httpStatus', 'modifyInstanceMetadataOptionsResponse_httpStatus' - The response's http status code.
newModifyInstanceMetadataOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceMetadataOptionsResponse
newModifyInstanceMetadataOptionsResponse pHttpStatus_ =
  ModifyInstanceMetadataOptionsResponse'
    { instanceMetadataOptions =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata options for the instance.
modifyInstanceMetadataOptionsResponse_instanceMetadataOptions :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataOptionsResponse)
modifyInstanceMetadataOptionsResponse_instanceMetadataOptions = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {instanceMetadataOptions} -> instanceMetadataOptions) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {instanceMetadataOptions = a} :: ModifyInstanceMetadataOptionsResponse)

-- | The ID of the instance.
modifyInstanceMetadataOptionsResponse_instanceId :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Prelude.Maybe Prelude.Text)
modifyInstanceMetadataOptionsResponse_instanceId = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {instanceId} -> instanceId) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {instanceId = a} :: ModifyInstanceMetadataOptionsResponse)

-- | The response's http status code.
modifyInstanceMetadataOptionsResponse_httpStatus :: Lens.Lens' ModifyInstanceMetadataOptionsResponse Prelude.Int
modifyInstanceMetadataOptionsResponse_httpStatus = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {httpStatus = a} :: ModifyInstanceMetadataOptionsResponse)

instance
  Prelude.NFData
    ModifyInstanceMetadataOptionsResponse
