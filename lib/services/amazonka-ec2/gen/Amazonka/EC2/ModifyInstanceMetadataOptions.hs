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
-- Module      : Amazonka.EC2.ModifyInstanceMetadataOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.EC2.ModifyInstanceMetadataOptions
  ( -- * Creating a Request
    ModifyInstanceMetadataOptions (..),
    newModifyInstanceMetadataOptions,

    -- * Request Lenses
    modifyInstanceMetadataOptions_dryRun,
    modifyInstanceMetadataOptions_httpEndpoint,
    modifyInstanceMetadataOptions_httpProtocolIpv6,
    modifyInstanceMetadataOptions_httpPutResponseHopLimit,
    modifyInstanceMetadataOptions_httpTokens,
    modifyInstanceMetadataOptions_instanceMetadataTags,
    modifyInstanceMetadataOptions_instanceId,

    -- * Destructuring the Response
    ModifyInstanceMetadataOptionsResponse (..),
    newModifyInstanceMetadataOptionsResponse,

    -- * Response Lenses
    modifyInstanceMetadataOptionsResponse_instanceId,
    modifyInstanceMetadataOptionsResponse_instanceMetadataOptions,
    modifyInstanceMetadataOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyInstanceMetadataOptions' smart constructor.
data ModifyInstanceMetadataOptions = ModifyInstanceMetadataOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Enables or disables the HTTP metadata endpoint on your instances. If
    -- this parameter is not specified, the existing state is maintained.
    --
    -- If you specify a value of @disabled@, you cannot access your instance
    -- metadata.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | Enables or disables the IPv6 endpoint for the instance metadata service.
    -- This setting applies only if you have enabled the HTTP metadata
    -- endpoint.
    httpProtocolIpv6 :: Prelude.Maybe InstanceMetadataProtocolState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel. If no parameter is specified, the existing state is maintained.
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
    -- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
    -- @required@ (in other words, set the use of IMDSv2 to @required@).
    --
    -- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
    --     instance metadata with or without a session token in your request.
    --     If you retrieve the IAM role credentials without a token, the IMDSv1
    --     role credentials are returned. If you retrieve the IAM role
    --     credentials using a valid session token, the IMDSv2 role credentials
    --     are returned.
    --
    -- -   @required@ - When IMDSv2 is required, you must send a session token
    --     with any instance metadata retrieval requests. In this state,
    --     retrieving the IAM role credentials always returns IMDSv2
    --     credentials; IMDSv1 credentials are not available.
    --
    -- Default: @optional@
    httpTokens :: Prelude.Maybe HttpTokensState,
    -- | Set to @enabled@ to allow access to instance tags from the instance
    -- metadata. Set to @disabled@ to turn off access to instance tags from the
    -- instance metadata. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
    --
    -- Default: @disabled@
    instanceMetadataTags :: Prelude.Maybe InstanceMetadataTagsState,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'httpEndpoint', 'modifyInstanceMetadataOptions_httpEndpoint' - Enables or disables the HTTP metadata endpoint on your instances. If
-- this parameter is not specified, the existing state is maintained.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
--
-- 'httpProtocolIpv6', 'modifyInstanceMetadataOptions_httpProtocolIpv6' - Enables or disables the IPv6 endpoint for the instance metadata service.
-- This setting applies only if you have enabled the HTTP metadata
-- endpoint.
--
-- 'httpPutResponseHopLimit', 'modifyInstanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'modifyInstanceMetadataOptions_httpTokens' - IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
-- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
-- @required@ (in other words, set the use of IMDSv2 to @required@).
--
-- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
--     instance metadata with or without a session token in your request.
--     If you retrieve the IAM role credentials without a token, the IMDSv1
--     role credentials are returned. If you retrieve the IAM role
--     credentials using a valid session token, the IMDSv2 role credentials
--     are returned.
--
-- -   @required@ - When IMDSv2 is required, you must send a session token
--     with any instance metadata retrieval requests. In this state,
--     retrieving the IAM role credentials always returns IMDSv2
--     credentials; IMDSv1 credentials are not available.
--
-- Default: @optional@
--
-- 'instanceMetadataTags', 'modifyInstanceMetadataOptions_instanceMetadataTags' - Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
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
      httpProtocolIpv6 = Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      instanceMetadataTags = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyInstanceMetadataOptions_dryRun :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe Prelude.Bool)
modifyInstanceMetadataOptions_dryRun = Lens.lens (\ModifyInstanceMetadataOptions' {dryRun} -> dryRun) (\s@ModifyInstanceMetadataOptions' {} a -> s {dryRun = a} :: ModifyInstanceMetadataOptions)

-- | Enables or disables the HTTP metadata endpoint on your instances. If
-- this parameter is not specified, the existing state is maintained.
--
-- If you specify a value of @disabled@, you cannot access your instance
-- metadata.
modifyInstanceMetadataOptions_httpEndpoint :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe InstanceMetadataEndpointState)
modifyInstanceMetadataOptions_httpEndpoint = Lens.lens (\ModifyInstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: ModifyInstanceMetadataOptions)

-- | Enables or disables the IPv6 endpoint for the instance metadata service.
-- This setting applies only if you have enabled the HTTP metadata
-- endpoint.
modifyInstanceMetadataOptions_httpProtocolIpv6 :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe InstanceMetadataProtocolState)
modifyInstanceMetadataOptions_httpProtocolIpv6 = Lens.lens (\ModifyInstanceMetadataOptions' {httpProtocolIpv6} -> httpProtocolIpv6) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpProtocolIpv6 = a} :: ModifyInstanceMetadataOptions)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
modifyInstanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe Prelude.Int)
modifyInstanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\ModifyInstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: ModifyInstanceMetadataOptions)

-- | IMDSv2 uses token-backed sessions. Set the use of HTTP tokens to
-- @optional@ (in other words, set the use of IMDSv2 to @optional@) or
-- @required@ (in other words, set the use of IMDSv2 to @required@).
--
-- -   @optional@ - When IMDSv2 is optional, you can choose to retrieve
--     instance metadata with or without a session token in your request.
--     If you retrieve the IAM role credentials without a token, the IMDSv1
--     role credentials are returned. If you retrieve the IAM role
--     credentials using a valid session token, the IMDSv2 role credentials
--     are returned.
--
-- -   @required@ - When IMDSv2 is required, you must send a session token
--     with any instance metadata retrieval requests. In this state,
--     retrieving the IAM role credentials always returns IMDSv2
--     credentials; IMDSv1 credentials are not available.
--
-- Default: @optional@
modifyInstanceMetadataOptions_httpTokens :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe HttpTokensState)
modifyInstanceMetadataOptions_httpTokens = Lens.lens (\ModifyInstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@ModifyInstanceMetadataOptions' {} a -> s {httpTokens = a} :: ModifyInstanceMetadataOptions)

-- | Set to @enabled@ to allow access to instance tags from the instance
-- metadata. Set to @disabled@ to turn off access to instance tags from the
-- instance metadata. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#work-with-tags-in-IMDS Work with instance tags using the instance metadata>.
--
-- Default: @disabled@
modifyInstanceMetadataOptions_instanceMetadataTags :: Lens.Lens' ModifyInstanceMetadataOptions (Prelude.Maybe InstanceMetadataTagsState)
modifyInstanceMetadataOptions_instanceMetadataTags = Lens.lens (\ModifyInstanceMetadataOptions' {instanceMetadataTags} -> instanceMetadataTags) (\s@ModifyInstanceMetadataOptions' {} a -> s {instanceMetadataTags = a} :: ModifyInstanceMetadataOptions)

-- | The ID of the instance.
modifyInstanceMetadataOptions_instanceId :: Lens.Lens' ModifyInstanceMetadataOptions Prelude.Text
modifyInstanceMetadataOptions_instanceId = Lens.lens (\ModifyInstanceMetadataOptions' {instanceId} -> instanceId) (\s@ModifyInstanceMetadataOptions' {} a -> s {instanceId = a} :: ModifyInstanceMetadataOptions)

instance
  Core.AWSRequest
    ModifyInstanceMetadataOptions
  where
  type
    AWSResponse ModifyInstanceMetadataOptions =
      ModifyInstanceMetadataOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstanceMetadataOptionsResponse'
            Prelude.<$> (x Data..@? "instanceId")
            Prelude.<*> (x Data..@? "instanceMetadataOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyInstanceMetadataOptions
  where
  hashWithSalt _salt ModifyInstanceMetadataOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` httpEndpoint
      `Prelude.hashWithSalt` httpProtocolIpv6
      `Prelude.hashWithSalt` httpPutResponseHopLimit
      `Prelude.hashWithSalt` httpTokens
      `Prelude.hashWithSalt` instanceMetadataTags
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ModifyInstanceMetadataOptions where
  rnf ModifyInstanceMetadataOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf httpEndpoint
      `Prelude.seq` Prelude.rnf httpProtocolIpv6
      `Prelude.seq` Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf instanceMetadataTags
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ModifyInstanceMetadataOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyInstanceMetadataOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyInstanceMetadataOptions where
  toQuery ModifyInstanceMetadataOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyInstanceMetadataOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "HttpEndpoint" Data.=: httpEndpoint,
        "HttpProtocolIpv6" Data.=: httpProtocolIpv6,
        "HttpPutResponseHopLimit"
          Data.=: httpPutResponseHopLimit,
        "HttpTokens" Data.=: httpTokens,
        "InstanceMetadataTags" Data.=: instanceMetadataTags,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newModifyInstanceMetadataOptionsResponse' smart constructor.
data ModifyInstanceMetadataOptionsResponse = ModifyInstanceMetadataOptionsResponse'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The metadata options for the instance.
    instanceMetadataOptions :: Prelude.Maybe InstanceMetadataOptionsResponse,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceMetadataOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'modifyInstanceMetadataOptionsResponse_instanceId' - The ID of the instance.
--
-- 'instanceMetadataOptions', 'modifyInstanceMetadataOptionsResponse_instanceMetadataOptions' - The metadata options for the instance.
--
-- 'httpStatus', 'modifyInstanceMetadataOptionsResponse_httpStatus' - The response's http status code.
newModifyInstanceMetadataOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyInstanceMetadataOptionsResponse
newModifyInstanceMetadataOptionsResponse pHttpStatus_ =
  ModifyInstanceMetadataOptionsResponse'
    { instanceId =
        Prelude.Nothing,
      instanceMetadataOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the instance.
modifyInstanceMetadataOptionsResponse_instanceId :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Prelude.Maybe Prelude.Text)
modifyInstanceMetadataOptionsResponse_instanceId = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {instanceId} -> instanceId) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {instanceId = a} :: ModifyInstanceMetadataOptionsResponse)

-- | The metadata options for the instance.
modifyInstanceMetadataOptionsResponse_instanceMetadataOptions :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Prelude.Maybe InstanceMetadataOptionsResponse)
modifyInstanceMetadataOptionsResponse_instanceMetadataOptions = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {instanceMetadataOptions} -> instanceMetadataOptions) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {instanceMetadataOptions = a} :: ModifyInstanceMetadataOptionsResponse)

-- | The response's http status code.
modifyInstanceMetadataOptionsResponse_httpStatus :: Lens.Lens' ModifyInstanceMetadataOptionsResponse Prelude.Int
modifyInstanceMetadataOptionsResponse_httpStatus = Lens.lens (\ModifyInstanceMetadataOptionsResponse' {httpStatus} -> httpStatus) (\s@ModifyInstanceMetadataOptionsResponse' {} a -> s {httpStatus = a} :: ModifyInstanceMetadataOptionsResponse)

instance
  Prelude.NFData
    ModifyInstanceMetadataOptionsResponse
  where
  rnf ModifyInstanceMetadataOptionsResponse' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceMetadataOptions
      `Prelude.seq` Prelude.rnf httpStatus
