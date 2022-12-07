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
-- Module      : Amazonka.EC2.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL.
module Amazonka.EC2.DeleteNetworkAclEntry
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkAclEntry' smart constructor.
data DeleteNetworkAclEntry = DeleteNetworkAclEntry'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the rule is an egress rule.
    egress :: Prelude.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Text,
    -- | The rule number of the entry to delete.
    ruleNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Bool ->
  -- | 'networkAclId'
  Prelude.Text ->
  -- | 'ruleNumber'
  Prelude.Int ->
  DeleteNetworkAclEntry
newDeleteNetworkAclEntry
  pEgress_
  pNetworkAclId_
  pRuleNumber_ =
    DeleteNetworkAclEntry'
      { dryRun = Prelude.Nothing,
        egress = pEgress_,
        networkAclId = pNetworkAclId_,
        ruleNumber = pRuleNumber_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkAclEntry_dryRun :: Lens.Lens' DeleteNetworkAclEntry (Prelude.Maybe Prelude.Bool)
deleteNetworkAclEntry_dryRun = Lens.lens (\DeleteNetworkAclEntry' {dryRun} -> dryRun) (\s@DeleteNetworkAclEntry' {} a -> s {dryRun = a} :: DeleteNetworkAclEntry)

-- | Indicates whether the rule is an egress rule.
deleteNetworkAclEntry_egress :: Lens.Lens' DeleteNetworkAclEntry Prelude.Bool
deleteNetworkAclEntry_egress = Lens.lens (\DeleteNetworkAclEntry' {egress} -> egress) (\s@DeleteNetworkAclEntry' {} a -> s {egress = a} :: DeleteNetworkAclEntry)

-- | The ID of the network ACL.
deleteNetworkAclEntry_networkAclId :: Lens.Lens' DeleteNetworkAclEntry Prelude.Text
deleteNetworkAclEntry_networkAclId = Lens.lens (\DeleteNetworkAclEntry' {networkAclId} -> networkAclId) (\s@DeleteNetworkAclEntry' {} a -> s {networkAclId = a} :: DeleteNetworkAclEntry)

-- | The rule number of the entry to delete.
deleteNetworkAclEntry_ruleNumber :: Lens.Lens' DeleteNetworkAclEntry Prelude.Int
deleteNetworkAclEntry_ruleNumber = Lens.lens (\DeleteNetworkAclEntry' {ruleNumber} -> ruleNumber) (\s@DeleteNetworkAclEntry' {} a -> s {ruleNumber = a} :: DeleteNetworkAclEntry)

instance Core.AWSRequest DeleteNetworkAclEntry where
  type
    AWSResponse DeleteNetworkAclEntry =
      DeleteNetworkAclEntryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteNetworkAclEntryResponse'

instance Prelude.Hashable DeleteNetworkAclEntry where
  hashWithSalt _salt DeleteNetworkAclEntry' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` egress
      `Prelude.hashWithSalt` networkAclId
      `Prelude.hashWithSalt` ruleNumber

instance Prelude.NFData DeleteNetworkAclEntry where
  rnf DeleteNetworkAclEntry' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf egress
      `Prelude.seq` Prelude.rnf networkAclId
      `Prelude.seq` Prelude.rnf ruleNumber

instance Data.ToHeaders DeleteNetworkAclEntry where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkAclEntry where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNetworkAclEntry where
  toQuery DeleteNetworkAclEntry' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteNetworkAclEntry" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Egress" Data.=: egress,
        "NetworkAclId" Data.=: networkAclId,
        "RuleNumber" Data.=: ruleNumber
      ]

-- | /See:/ 'newDeleteNetworkAclEntryResponse' smart constructor.
data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkAclEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkAclEntryResponse ::
  DeleteNetworkAclEntryResponse
newDeleteNetworkAclEntryResponse =
  DeleteNetworkAclEntryResponse'

instance Prelude.NFData DeleteNetworkAclEntryResponse where
  rnf _ = ()
