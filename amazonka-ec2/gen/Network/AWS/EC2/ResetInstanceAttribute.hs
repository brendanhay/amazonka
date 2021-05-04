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
-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an instance to its default value. To reset the
-- @kernel@ or @ramdisk@, the instance must be in a stopped state. To reset
-- the @sourceDestCheck@, the instance can be either running or stopped.
--
-- The @sourceDestCheck@ attribute controls whether source\/destination
-- checking is enabled. The default value is @true@, which means checking
-- is enabled. This value must be @false@ for a NAT instance to perform
-- NAT. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances>
-- in the /Amazon VPC User Guide/.
module Network.AWS.EC2.ResetInstanceAttribute
  ( -- * Creating a Request
    ResetInstanceAttribute (..),
    newResetInstanceAttribute,

    -- * Request Lenses
    resetInstanceAttribute_dryRun,
    resetInstanceAttribute_attribute,
    resetInstanceAttribute_instanceId,

    -- * Destructuring the Response
    ResetInstanceAttributeResponse (..),
    newResetInstanceAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetInstanceAttribute' smart constructor.
data ResetInstanceAttribute = ResetInstanceAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The attribute to reset.
    --
    -- You can only reset the following attributes: @kernel@ | @ramdisk@ |
    -- @sourceDestCheck@. To change an instance attribute, use
    -- ModifyInstanceAttribute.
    attribute :: InstanceAttributeName,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetInstanceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetInstanceAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'resetInstanceAttribute_attribute' - The attribute to reset.
--
-- You can only reset the following attributes: @kernel@ | @ramdisk@ |
-- @sourceDestCheck@. To change an instance attribute, use
-- ModifyInstanceAttribute.
--
-- 'instanceId', 'resetInstanceAttribute_instanceId' - The ID of the instance.
newResetInstanceAttribute ::
  -- | 'attribute'
  InstanceAttributeName ->
  -- | 'instanceId'
  Prelude.Text ->
  ResetInstanceAttribute
newResetInstanceAttribute pAttribute_ pInstanceId_ =
  ResetInstanceAttribute'
    { dryRun = Prelude.Nothing,
      attribute = pAttribute_,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetInstanceAttribute_dryRun :: Lens.Lens' ResetInstanceAttribute (Prelude.Maybe Prelude.Bool)
resetInstanceAttribute_dryRun = Lens.lens (\ResetInstanceAttribute' {dryRun} -> dryRun) (\s@ResetInstanceAttribute' {} a -> s {dryRun = a} :: ResetInstanceAttribute)

-- | The attribute to reset.
--
-- You can only reset the following attributes: @kernel@ | @ramdisk@ |
-- @sourceDestCheck@. To change an instance attribute, use
-- ModifyInstanceAttribute.
resetInstanceAttribute_attribute :: Lens.Lens' ResetInstanceAttribute InstanceAttributeName
resetInstanceAttribute_attribute = Lens.lens (\ResetInstanceAttribute' {attribute} -> attribute) (\s@ResetInstanceAttribute' {} a -> s {attribute = a} :: ResetInstanceAttribute)

-- | The ID of the instance.
resetInstanceAttribute_instanceId :: Lens.Lens' ResetInstanceAttribute Prelude.Text
resetInstanceAttribute_instanceId = Lens.lens (\ResetInstanceAttribute' {instanceId} -> instanceId) (\s@ResetInstanceAttribute' {} a -> s {instanceId = a} :: ResetInstanceAttribute)

instance Prelude.AWSRequest ResetInstanceAttribute where
  type
    Rs ResetInstanceAttribute =
      ResetInstanceAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      ResetInstanceAttributeResponse'

instance Prelude.Hashable ResetInstanceAttribute

instance Prelude.NFData ResetInstanceAttribute

instance Prelude.ToHeaders ResetInstanceAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResetInstanceAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetInstanceAttribute where
  toQuery ResetInstanceAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResetInstanceAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Attribute" Prelude.=: attribute,
        "InstanceId" Prelude.=: instanceId
      ]

-- | /See:/ 'newResetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetInstanceAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResetInstanceAttributeResponse ::
  ResetInstanceAttributeResponse
newResetInstanceAttributeResponse =
  ResetInstanceAttributeResponse'

instance
  Prelude.NFData
    ResetInstanceAttributeResponse
