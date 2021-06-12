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
-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified network interface attribute. You can specify only
-- one attribute at a time. You can use this action to attach and detach
-- security groups from an existing EC2 instance.
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
  ( -- * Creating a Request
    ModifyNetworkInterfaceAttribute (..),
    newModifyNetworkInterfaceAttribute,

    -- * Request Lenses
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- * Destructuring the Response
    ModifyNetworkInterfaceAttributeResponse (..),
    newModifyNetworkInterfaceAttributeResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
-- /See:/ 'newModifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { -- | Changes the security groups for the network interface. The new set of
    -- groups you specify replaces the current set. You must specify at least
    -- one group, even if it\'s just the default security group in the VPC. You
    -- must specify the ID of the security group, not the name.
    groups :: Core.Maybe [Core.Text],
    -- | Information about the interface attachment. If modifying the \'delete on
    -- termination\' attribute, you must specify the ID of the interface
    -- attachment.
    attachment :: Core.Maybe NetworkInterfaceAttachmentChanges,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether source\/destination checking is enabled. A value of
    -- @true@ means checking is enabled, and @false@ means checking is
    -- disabled. This value must be @false@ for a NAT instance to perform NAT.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    sourceDestCheck :: Core.Maybe AttributeBooleanValue,
    -- | A description for the network interface.
    description :: Core.Maybe AttributeValue,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyNetworkInterfaceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'modifyNetworkInterfaceAttribute_groups' - Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
--
-- 'attachment', 'modifyNetworkInterfaceAttribute_attachment' - Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
--
-- 'dryRun', 'modifyNetworkInterfaceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'sourceDestCheck', 'modifyNetworkInterfaceAttribute_sourceDestCheck' - Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- 'description', 'modifyNetworkInterfaceAttribute_description' - A description for the network interface.
--
-- 'networkInterfaceId', 'modifyNetworkInterfaceAttribute_networkInterfaceId' - The ID of the network interface.
newModifyNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Core.Text ->
  ModifyNetworkInterfaceAttribute
newModifyNetworkInterfaceAttribute
  pNetworkInterfaceId_ =
    ModifyNetworkInterfaceAttribute'
      { groups =
          Core.Nothing,
        attachment = Core.Nothing,
        dryRun = Core.Nothing,
        sourceDestCheck = Core.Nothing,
        description = Core.Nothing,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
modifyNetworkInterfaceAttribute_groups :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe [Core.Text])
modifyNetworkInterfaceAttribute_groups = Lens.lens (\ModifyNetworkInterfaceAttribute' {groups} -> groups) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {groups = a} :: ModifyNetworkInterfaceAttribute) Core.. Lens.mapping Lens._Coerce

-- | Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
modifyNetworkInterfaceAttribute_attachment :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe NetworkInterfaceAttachmentChanges)
modifyNetworkInterfaceAttribute_attachment = Lens.lens (\ModifyNetworkInterfaceAttribute' {attachment} -> attachment) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {attachment = a} :: ModifyNetworkInterfaceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyNetworkInterfaceAttribute_dryRun :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Core.Bool)
modifyNetworkInterfaceAttribute_dryRun = Lens.lens (\ModifyNetworkInterfaceAttribute' {dryRun} -> dryRun) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {dryRun = a} :: ModifyNetworkInterfaceAttribute)

-- | Indicates whether source\/destination checking is enabled. A value of
-- @true@ means checking is enabled, and @false@ means checking is
-- disabled. This value must be @false@ for a NAT instance to perform NAT.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon Virtual Private Cloud User Guide/.
modifyNetworkInterfaceAttribute_sourceDestCheck :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe AttributeBooleanValue)
modifyNetworkInterfaceAttribute_sourceDestCheck = Lens.lens (\ModifyNetworkInterfaceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {sourceDestCheck = a} :: ModifyNetworkInterfaceAttribute)

-- | A description for the network interface.
modifyNetworkInterfaceAttribute_description :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe AttributeValue)
modifyNetworkInterfaceAttribute_description = Lens.lens (\ModifyNetworkInterfaceAttribute' {description} -> description) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {description = a} :: ModifyNetworkInterfaceAttribute)

-- | The ID of the network interface.
modifyNetworkInterfaceAttribute_networkInterfaceId :: Lens.Lens' ModifyNetworkInterfaceAttribute Core.Text
modifyNetworkInterfaceAttribute_networkInterfaceId = Lens.lens (\ModifyNetworkInterfaceAttribute' {networkInterfaceId} -> networkInterfaceId) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {networkInterfaceId = a} :: ModifyNetworkInterfaceAttribute)

instance
  Core.AWSRequest
    ModifyNetworkInterfaceAttribute
  where
  type
    AWSResponse ModifyNetworkInterfaceAttribute =
      ModifyNetworkInterfaceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ModifyNetworkInterfaceAttributeResponse'

instance
  Core.Hashable
    ModifyNetworkInterfaceAttribute

instance Core.NFData ModifyNetworkInterfaceAttribute

instance
  Core.ToHeaders
    ModifyNetworkInterfaceAttribute
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyNetworkInterfaceAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ModifyNetworkInterfaceAttribute where
  toQuery ModifyNetworkInterfaceAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifyNetworkInterfaceAttribute" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          (Core.toQueryList "SecurityGroupId" Core.<$> groups),
        "Attachment" Core.=: attachment,
        "DryRun" Core.=: dryRun,
        "SourceDestCheck" Core.=: sourceDestCheck,
        "Description" Core.=: description,
        "NetworkInterfaceId" Core.=: networkInterfaceId
      ]

-- | /See:/ 'newModifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyNetworkInterfaceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyNetworkInterfaceAttributeResponse ::
  ModifyNetworkInterfaceAttributeResponse
newModifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'

instance
  Core.NFData
    ModifyNetworkInterfaceAttributeResponse
