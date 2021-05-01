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
-- Module      : Network.AWS.EC2.ModifyIdentityIdFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format of a resource for a specified IAM user, IAM role,
-- or the root user for an account; or all IAM users, IAM roles, and the
-- root user for an account. You can specify that resources should receive
-- longer IDs (17-character IDs) when they are created.
--
-- This request can only be used to modify longer ID settings for resource
-- types that are within the opt-in period. Resources currently in their
-- opt-in period include: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @route-table@ | @route-table-association@ | @security-group@ | @subnet@
-- | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@
-- | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ |
-- @vpn-gateway@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This setting applies to the principal specified in the request; it does
-- not apply to the principal that makes the request.
--
-- Resources created with longer IDs are visible to all IAM roles and
-- users, regardless of these settings and provided that they have
-- permission to use the relevant @Describe@ command for the resource type.
module Network.AWS.EC2.ModifyIdentityIdFormat
  ( -- * Creating a Request
    ModifyIdentityIdFormat (..),
    newModifyIdentityIdFormat,

    -- * Request Lenses
    modifyIdentityIdFormat_principalArn,
    modifyIdentityIdFormat_resource,
    modifyIdentityIdFormat_useLongIds,

    -- * Destructuring the Response
    ModifyIdentityIdFormatResponse (..),
    newModifyIdentityIdFormatResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyIdentityIdFormat' smart constructor.
data ModifyIdentityIdFormat = ModifyIdentityIdFormat'
  { -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. Specify @all@ to modify the ID format for all IAM users, IAM
    -- roles, and the root user of the account.
    principalArn :: Prelude.Text,
    -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
    -- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
    -- @export-task@ | @flow-log@ | @image@ | @import-task@ |
    -- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
    -- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
    -- @route-table@ | @route-table-association@ | @security-group@ | @subnet@
    -- | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@
    -- | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ |
    -- @vpn-gateway@.
    --
    -- Alternatively, use the @all-current@ option to include all resource
    -- types that are currently within their opt-in period for longer IDs.
    resource :: Prelude.Text,
    -- | Indicates whether the resource should use longer IDs (17-character IDs)
    useLongIds :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyIdentityIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalArn', 'modifyIdentityIdFormat_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. Specify @all@ to modify the ID format for all IAM users, IAM
-- roles, and the root user of the account.
--
-- 'resource', 'modifyIdentityIdFormat_resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @route-table@ | @route-table-association@ | @security-group@ | @subnet@
-- | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@
-- | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ |
-- @vpn-gateway@.
--
-- Alternatively, use the @all-current@ option to include all resource
-- types that are currently within their opt-in period for longer IDs.
--
-- 'useLongIds', 'modifyIdentityIdFormat_useLongIds' - Indicates whether the resource should use longer IDs (17-character IDs)
newModifyIdentityIdFormat ::
  -- | 'principalArn'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  -- | 'useLongIds'
  Prelude.Bool ->
  ModifyIdentityIdFormat
newModifyIdentityIdFormat
  pPrincipalArn_
  pResource_
  pUseLongIds_ =
    ModifyIdentityIdFormat'
      { principalArn =
          pPrincipalArn_,
        resource = pResource_,
        useLongIds = pUseLongIds_
      }

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. Specify @all@ to modify the ID format for all IAM users, IAM
-- roles, and the root user of the account.
modifyIdentityIdFormat_principalArn :: Lens.Lens' ModifyIdentityIdFormat Prelude.Text
modifyIdentityIdFormat_principalArn = Lens.lens (\ModifyIdentityIdFormat' {principalArn} -> principalArn) (\s@ModifyIdentityIdFormat' {} a -> s {principalArn = a} :: ModifyIdentityIdFormat)

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
-- | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ |
-- @export-task@ | @flow-log@ | @image@ | @import-task@ |
-- @internet-gateway@ | @network-acl@ | @network-acl-association@ |
-- @network-interface@ | @network-interface-attachment@ | @prefix-list@ |
-- @route-table@ | @route-table-association@ | @security-group@ | @subnet@
-- | @subnet-cidr-block-association@ | @vpc@ | @vpc-cidr-block-association@
-- | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ |
-- @vpn-gateway@.
--
-- Alternatively, use the @all-current@ option to include all resource
-- types that are currently within their opt-in period for longer IDs.
modifyIdentityIdFormat_resource :: Lens.Lens' ModifyIdentityIdFormat Prelude.Text
modifyIdentityIdFormat_resource = Lens.lens (\ModifyIdentityIdFormat' {resource} -> resource) (\s@ModifyIdentityIdFormat' {} a -> s {resource = a} :: ModifyIdentityIdFormat)

-- | Indicates whether the resource should use longer IDs (17-character IDs)
modifyIdentityIdFormat_useLongIds :: Lens.Lens' ModifyIdentityIdFormat Prelude.Bool
modifyIdentityIdFormat_useLongIds = Lens.lens (\ModifyIdentityIdFormat' {useLongIds} -> useLongIds) (\s@ModifyIdentityIdFormat' {} a -> s {useLongIds = a} :: ModifyIdentityIdFormat)

instance Prelude.AWSRequest ModifyIdentityIdFormat where
  type
    Rs ModifyIdentityIdFormat =
      ModifyIdentityIdFormatResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifyIdentityIdFormatResponse'

instance Prelude.Hashable ModifyIdentityIdFormat

instance Prelude.NFData ModifyIdentityIdFormat

instance Prelude.ToHeaders ModifyIdentityIdFormat where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyIdentityIdFormat where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyIdentityIdFormat where
  toQuery ModifyIdentityIdFormat' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyIdentityIdFormat" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "PrincipalArn" Prelude.=: principalArn,
        "Resource" Prelude.=: resource,
        "UseLongIds" Prelude.=: useLongIds
      ]

-- | /See:/ 'newModifyIdentityIdFormatResponse' smart constructor.
data ModifyIdentityIdFormatResponse = ModifyIdentityIdFormatResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyIdentityIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyIdentityIdFormatResponse ::
  ModifyIdentityIdFormatResponse
newModifyIdentityIdFormatResponse =
  ModifyIdentityIdFormatResponse'

instance
  Prelude.NFData
    ModifyIdentityIdFormatResponse
