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
-- Module      : Amazonka.EC2.ModifyIdFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ID format for the specified resource on a per-Region basis.
-- You can specify that resources should receive longer IDs (17-character
-- IDs) when they are created.
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
-- This setting applies to the IAM user who makes the request; it does not
-- apply to the entire Amazon Web Services account. By default, an IAM user
-- defaults to the same settings as the root user. If you\'re using this
-- action as the root user, then these settings apply to the entire
-- account, unless an IAM user explicitly overrides these settings for
-- themselves. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Resources created with longer IDs are visible to all IAM roles and
-- users, regardless of these settings and provided that they have
-- permission to use the relevant @Describe@ command for the resource type.
module Amazonka.EC2.ModifyIdFormat
  ( -- * Creating a Request
    ModifyIdFormat (..),
    newModifyIdFormat,

    -- * Request Lenses
    modifyIdFormat_resource,
    modifyIdFormat_useLongIds,

    -- * Destructuring the Response
    ModifyIdFormatResponse (..),
    newModifyIdFormatResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyIdFormat' smart constructor.
data ModifyIdFormat = ModifyIdFormat'
  { -- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
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
    -- | Indicate whether the resource should use longer IDs (17-character IDs).
    useLongIds :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'modifyIdFormat_resource' - The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@
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
-- 'useLongIds', 'modifyIdFormat_useLongIds' - Indicate whether the resource should use longer IDs (17-character IDs).
newModifyIdFormat ::
  -- | 'resource'
  Prelude.Text ->
  -- | 'useLongIds'
  Prelude.Bool ->
  ModifyIdFormat
newModifyIdFormat pResource_ pUseLongIds_ =
  ModifyIdFormat'
    { resource = pResource_,
      useLongIds = pUseLongIds_
    }

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
modifyIdFormat_resource :: Lens.Lens' ModifyIdFormat Prelude.Text
modifyIdFormat_resource = Lens.lens (\ModifyIdFormat' {resource} -> resource) (\s@ModifyIdFormat' {} a -> s {resource = a} :: ModifyIdFormat)

-- | Indicate whether the resource should use longer IDs (17-character IDs).
modifyIdFormat_useLongIds :: Lens.Lens' ModifyIdFormat Prelude.Bool
modifyIdFormat_useLongIds = Lens.lens (\ModifyIdFormat' {useLongIds} -> useLongIds) (\s@ModifyIdFormat' {} a -> s {useLongIds = a} :: ModifyIdFormat)

instance Core.AWSRequest ModifyIdFormat where
  type
    AWSResponse ModifyIdFormat =
      ModifyIdFormatResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ModifyIdFormatResponse'

instance Prelude.Hashable ModifyIdFormat where
  hashWithSalt _salt ModifyIdFormat' {..} =
    _salt
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` useLongIds

instance Prelude.NFData ModifyIdFormat where
  rnf ModifyIdFormat' {..} =
    Prelude.rnf resource `Prelude.seq`
      Prelude.rnf useLongIds

instance Data.ToHeaders ModifyIdFormat where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyIdFormat where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyIdFormat where
  toQuery ModifyIdFormat' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyIdFormat" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Resource" Data.=: resource,
        "UseLongIds" Data.=: useLongIds
      ]

-- | /See:/ 'newModifyIdFormatResponse' smart constructor.
data ModifyIdFormatResponse = ModifyIdFormatResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyIdFormatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyIdFormatResponse ::
  ModifyIdFormatResponse
newModifyIdFormatResponse = ModifyIdFormatResponse'

instance Prelude.NFData ModifyIdFormatResponse where
  rnf _ = ()
