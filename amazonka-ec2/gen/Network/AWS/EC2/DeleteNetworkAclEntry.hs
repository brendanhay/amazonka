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
-- Module      : Network.AWS.EC2.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL.
module Network.AWS.EC2.DeleteNetworkAclEntry
  ( -- * Creating a Request
    DeleteNetworkAclEntry (..),
    newDeleteNetworkAclEntry,

    -- * Request Lenses
    deleteNetworkAclEntry_dryRun,
    deleteNetworkAclEntry_egress,
    deleteNetworkAclEntry_networkAclId,
    deleteNetworkAclEntry_ruleNumber,

    -- * Destructuring the Response
    DeleteNetworkAclEntryResponse (..),
    newDeleteNetworkAclEntryResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkAclEntry' smart constructor.
data DeleteNetworkAclEntry = DeleteNetworkAclEntry'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Indicates whether the rule is an egress rule.
    egress :: Core.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Core.Text,
    -- | The rule number of the entry to delete.
    ruleNumber :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkAclEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkAclEntry_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'egress', 'deleteNetworkAclEntry_egress' - Indicates whether the rule is an egress rule.
--
-- 'networkAclId', 'deleteNetworkAclEntry_networkAclId' - The ID of the network ACL.
--
-- 'ruleNumber', 'deleteNetworkAclEntry_ruleNumber' - The rule number of the entry to delete.
newDeleteNetworkAclEntry ::
  -- | 'egress'
  Core.Bool ->
  -- | 'networkAclId'
  Core.Text ->
  -- | 'ruleNumber'
  Core.Int ->
  DeleteNetworkAclEntry
newDeleteNetworkAclEntry
  pEgress_
  pNetworkAclId_
  pRuleNumber_ =
    DeleteNetworkAclEntry'
      { dryRun = Core.Nothing,
        egress = pEgress_,
        networkAclId = pNetworkAclId_,
        ruleNumber = pRuleNumber_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkAclEntry_dryRun :: Lens.Lens' DeleteNetworkAclEntry (Core.Maybe Core.Bool)
deleteNetworkAclEntry_dryRun = Lens.lens (\DeleteNetworkAclEntry' {dryRun} -> dryRun) (\s@DeleteNetworkAclEntry' {} a -> s {dryRun = a} :: DeleteNetworkAclEntry)

-- | Indicates whether the rule is an egress rule.
deleteNetworkAclEntry_egress :: Lens.Lens' DeleteNetworkAclEntry Core.Bool
deleteNetworkAclEntry_egress = Lens.lens (\DeleteNetworkAclEntry' {egress} -> egress) (\s@DeleteNetworkAclEntry' {} a -> s {egress = a} :: DeleteNetworkAclEntry)

-- | The ID of the network ACL.
deleteNetworkAclEntry_networkAclId :: Lens.Lens' DeleteNetworkAclEntry Core.Text
deleteNetworkAclEntry_networkAclId = Lens.lens (\DeleteNetworkAclEntry' {networkAclId} -> networkAclId) (\s@DeleteNetworkAclEntry' {} a -> s {networkAclId = a} :: DeleteNetworkAclEntry)

-- | The rule number of the entry to delete.
deleteNetworkAclEntry_ruleNumber :: Lens.Lens' DeleteNetworkAclEntry Core.Int
deleteNetworkAclEntry_ruleNumber = Lens.lens (\DeleteNetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@DeleteNetworkAclEntry' {} a -> s {ruleNumber = a} :: DeleteNetworkAclEntry)

instance Core.AWSRequest DeleteNetworkAclEntry where
  type
    AWSResponse DeleteNetworkAclEntry =
      DeleteNetworkAclEntryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteNetworkAclEntryResponse'

instance Core.Hashable DeleteNetworkAclEntry

instance Core.NFData DeleteNetworkAclEntry

instance Core.ToHeaders DeleteNetworkAclEntry where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNetworkAclEntry where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkAclEntry where
  toQuery DeleteNetworkAclEntry' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkAclEntry" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Egress" Core.=: egress,
        "NetworkAclId" Core.=: networkAclId,
        "RuleNumber" Core.=: ruleNumber
      ]

-- | /See:/ 'newDeleteNetworkAclEntryResponse' smart constructor.
data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkAclEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkAclEntryResponse ::
  DeleteNetworkAclEntryResponse
newDeleteNetworkAclEntryResponse =
  DeleteNetworkAclEntryResponse'

instance Core.NFData DeleteNetworkAclEntryResponse
