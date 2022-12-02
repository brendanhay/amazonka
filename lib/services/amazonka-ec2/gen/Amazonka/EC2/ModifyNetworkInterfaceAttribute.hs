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
-- Module      : Amazonka.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified network interface attribute. You can specify only
-- one attribute at a time. You can use this action to attach and detach
-- security groups from an existing EC2 instance.
module Amazonka.EC2.ModifyNetworkInterfaceAttribute
  ( -- * Creating a Request
    ModifyNetworkInterfaceAttribute (..),
    newModifyNetworkInterfaceAttribute,

    -- * Request Lenses
    modifyNetworkInterfaceAttribute_attachment,
    modifyNetworkInterfaceAttribute_sourceDestCheck,
    modifyNetworkInterfaceAttribute_description,
    modifyNetworkInterfaceAttribute_dryRun,
    modifyNetworkInterfaceAttribute_groups,
    modifyNetworkInterfaceAttribute_networkInterfaceId,

    -- * Destructuring the Response
    ModifyNetworkInterfaceAttributeResponse (..),
    newModifyNetworkInterfaceAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
-- /See:/ 'newModifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { -- | Information about the interface attachment. If modifying the \'delete on
    -- termination\' attribute, you must specify the ID of the interface
    -- attachment.
    attachment :: Prelude.Maybe NetworkInterfaceAttachmentChanges,
    -- | Enable or disable source\/destination checks, which ensure that the
    -- instance is either the source or the destination of any traffic that it
    -- receives. If the value is @true@, source\/destination checks are
    -- enabled; otherwise, they are disabled. The default value is @true@. You
    -- must disable source\/destination checks if the instance runs services
    -- such as network address translation, routing, or firewalls.
    sourceDestCheck :: Prelude.Maybe AttributeBooleanValue,
    -- | A description for the network interface.
    description :: Prelude.Maybe AttributeValue,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Changes the security groups for the network interface. The new set of
    -- groups you specify replaces the current set. You must specify at least
    -- one group, even if it\'s just the default security group in the VPC. You
    -- must specify the ID of the security group, not the name.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyNetworkInterfaceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'modifyNetworkInterfaceAttribute_attachment' - Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
--
-- 'sourceDestCheck', 'modifyNetworkInterfaceAttribute_sourceDestCheck' - Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
--
-- 'description', 'modifyNetworkInterfaceAttribute_description' - A description for the network interface.
--
-- 'dryRun', 'modifyNetworkInterfaceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groups', 'modifyNetworkInterfaceAttribute_groups' - Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
--
-- 'networkInterfaceId', 'modifyNetworkInterfaceAttribute_networkInterfaceId' - The ID of the network interface.
newModifyNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  ModifyNetworkInterfaceAttribute
newModifyNetworkInterfaceAttribute
  pNetworkInterfaceId_ =
    ModifyNetworkInterfaceAttribute'
      { attachment =
          Prelude.Nothing,
        sourceDestCheck = Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        groups = Prelude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | Information about the interface attachment. If modifying the \'delete on
-- termination\' attribute, you must specify the ID of the interface
-- attachment.
modifyNetworkInterfaceAttribute_attachment :: Lens.Lens' ModifyNetworkInterfaceAttribute (Prelude.Maybe NetworkInterfaceAttachmentChanges)
modifyNetworkInterfaceAttribute_attachment = Lens.lens (\ModifyNetworkInterfaceAttribute' {attachment} -> attachment) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {attachment = a} :: ModifyNetworkInterfaceAttribute)

-- | Enable or disable source\/destination checks, which ensure that the
-- instance is either the source or the destination of any traffic that it
-- receives. If the value is @true@, source\/destination checks are
-- enabled; otherwise, they are disabled. The default value is @true@. You
-- must disable source\/destination checks if the instance runs services
-- such as network address translation, routing, or firewalls.
modifyNetworkInterfaceAttribute_sourceDestCheck :: Lens.Lens' ModifyNetworkInterfaceAttribute (Prelude.Maybe AttributeBooleanValue)
modifyNetworkInterfaceAttribute_sourceDestCheck = Lens.lens (\ModifyNetworkInterfaceAttribute' {sourceDestCheck} -> sourceDestCheck) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {sourceDestCheck = a} :: ModifyNetworkInterfaceAttribute)

-- | A description for the network interface.
modifyNetworkInterfaceAttribute_description :: Lens.Lens' ModifyNetworkInterfaceAttribute (Prelude.Maybe AttributeValue)
modifyNetworkInterfaceAttribute_description = Lens.lens (\ModifyNetworkInterfaceAttribute' {description} -> description) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {description = a} :: ModifyNetworkInterfaceAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyNetworkInterfaceAttribute_dryRun :: Lens.Lens' ModifyNetworkInterfaceAttribute (Prelude.Maybe Prelude.Bool)
modifyNetworkInterfaceAttribute_dryRun = Lens.lens (\ModifyNetworkInterfaceAttribute' {dryRun} -> dryRun) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {dryRun = a} :: ModifyNetworkInterfaceAttribute)

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least
-- one group, even if it\'s just the default security group in the VPC. You
-- must specify the ID of the security group, not the name.
modifyNetworkInterfaceAttribute_groups :: Lens.Lens' ModifyNetworkInterfaceAttribute (Prelude.Maybe [Prelude.Text])
modifyNetworkInterfaceAttribute_groups = Lens.lens (\ModifyNetworkInterfaceAttribute' {groups} -> groups) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {groups = a} :: ModifyNetworkInterfaceAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
modifyNetworkInterfaceAttribute_networkInterfaceId :: Lens.Lens' ModifyNetworkInterfaceAttribute Prelude.Text
modifyNetworkInterfaceAttribute_networkInterfaceId = Lens.lens (\ModifyNetworkInterfaceAttribute' {networkInterfaceId} -> networkInterfaceId) (\s@ModifyNetworkInterfaceAttribute' {} a -> s {networkInterfaceId = a} :: ModifyNetworkInterfaceAttribute)

instance
  Core.AWSRequest
    ModifyNetworkInterfaceAttribute
  where
  type
    AWSResponse ModifyNetworkInterfaceAttribute =
      ModifyNetworkInterfaceAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ModifyNetworkInterfaceAttributeResponse'

instance
  Prelude.Hashable
    ModifyNetworkInterfaceAttribute
  where
  hashWithSalt
    _salt
    ModifyNetworkInterfaceAttribute' {..} =
      _salt `Prelude.hashWithSalt` attachment
        `Prelude.hashWithSalt` sourceDestCheck
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` groups
        `Prelude.hashWithSalt` networkInterfaceId

instance
  Prelude.NFData
    ModifyNetworkInterfaceAttribute
  where
  rnf ModifyNetworkInterfaceAttribute' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf sourceDestCheck
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance
  Data.ToHeaders
    ModifyNetworkInterfaceAttribute
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyNetworkInterfaceAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyNetworkInterfaceAttribute where
  toQuery ModifyNetworkInterfaceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyNetworkInterfaceAttribute" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attachment" Data.=: attachment,
        "SourceDestCheck" Data.=: sourceDestCheck,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> groups
          ),
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newModifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyNetworkInterfaceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyNetworkInterfaceAttributeResponse ::
  ModifyNetworkInterfaceAttributeResponse
newModifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'

instance
  Prelude.NFData
    ModifyNetworkInterfaceAttributeResponse
  where
  rnf _ = ()
