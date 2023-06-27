{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CleanRooms.Types.ProtectedQuerySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQuerySummary where

import Amazonka.CleanRooms.Types.ProtectedQueryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The protected query summary for the objects listed by the request.
--
-- /See:/ 'newProtectedQuerySummary' smart constructor.
data ProtectedQuerySummary = ProtectedQuerySummary'
  { -- | The unique ID of the protected query.
    id :: Prelude.Text,
    -- | The unique ID for the membership that initiated the protected query.
    membershipId :: Prelude.Text,
    -- | The unique ARN for the membership that initiated the protected query.
    membershipArn :: Prelude.Text,
    -- | The time the protected query was created.
    createTime :: Data.POSIX,
    -- | The status of the protected query. Value values are \`SUBMITTED\`,
    -- \`STARTED\`, \`CANCELLED\`, \`CANCELLING\`, \`FAILED\`, \`SUCCESS\`,
    -- \`TIMED_OUT\`.
    status :: ProtectedQueryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQuerySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'protectedQuerySummary_id' - The unique ID of the protected query.
--
-- 'membershipId', 'protectedQuerySummary_membershipId' - The unique ID for the membership that initiated the protected query.
--
-- 'membershipArn', 'protectedQuerySummary_membershipArn' - The unique ARN for the membership that initiated the protected query.
--
-- 'createTime', 'protectedQuerySummary_createTime' - The time the protected query was created.
--
-- 'status', 'protectedQuerySummary_status' - The status of the protected query. Value values are \`SUBMITTED\`,
-- \`STARTED\`, \`CANCELLED\`, \`CANCELLING\`, \`FAILED\`, \`SUCCESS\`,
-- \`TIMED_OUT\`.
newProtectedQuerySummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'membershipArn'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'status'
  ProtectedQueryStatus ->
  ProtectedQuerySummary
newProtectedQuerySummary
  pId_
  pMembershipId_
  pMembershipArn_
  pCreateTime_
  pStatus_ =
    ProtectedQuerySummary'
      { id = pId_,
        membershipId = pMembershipId_,
        membershipArn = pMembershipArn_,
        createTime = Data._Time Lens.# pCreateTime_,
        status = pStatus_
      }

-- | The unique ID of the protected query.
protectedQuerySummary_id :: Lens.Lens' ProtectedQuerySummary Prelude.Text
protectedQuerySummary_id = Lens.lens (\ProtectedQuerySummary' {id} -> id) (\s@ProtectedQuerySummary' {} a -> s {id = a} :: ProtectedQuerySummary)

-- | The unique ID for the membership that initiated the protected query.
protectedQuerySummary_membershipId :: Lens.Lens' ProtectedQuerySummary Prelude.Text
protectedQuerySummary_membershipId = Lens.lens (\ProtectedQuerySummary' {membershipId} -> membershipId) (\s@ProtectedQuerySummary' {} a -> s {membershipId = a} :: ProtectedQuerySummary)

-- | The unique ARN for the membership that initiated the protected query.
protectedQuerySummary_membershipArn :: Lens.Lens' ProtectedQuerySummary Prelude.Text
protectedQuerySummary_membershipArn = Lens.lens (\ProtectedQuerySummary' {membershipArn} -> membershipArn) (\s@ProtectedQuerySummary' {} a -> s {membershipArn = a} :: ProtectedQuerySummary)

-- | The time the protected query was created.
protectedQuerySummary_createTime :: Lens.Lens' ProtectedQuerySummary Prelude.UTCTime
protectedQuerySummary_createTime = Lens.lens (\ProtectedQuerySummary' {createTime} -> createTime) (\s@ProtectedQuerySummary' {} a -> s {createTime = a} :: ProtectedQuerySummary) Prelude.. Data._Time

-- | The status of the protected query. Value values are \`SUBMITTED\`,
-- \`STARTED\`, \`CANCELLED\`, \`CANCELLING\`, \`FAILED\`, \`SUCCESS\`,
-- \`TIMED_OUT\`.
protectedQuerySummary_status :: Lens.Lens' ProtectedQuerySummary ProtectedQueryStatus
protectedQuerySummary_status = Lens.lens (\ProtectedQuerySummary' {status} -> status) (\s@ProtectedQuerySummary' {} a -> s {status = a} :: ProtectedQuerySummary)

instance Data.FromJSON ProtectedQuerySummary where
  parseJSON =
    Data.withObject
      "ProtectedQuerySummary"
      ( \x ->
          ProtectedQuerySummary'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "membershipId")
            Prelude.<*> (x Data..: "membershipArn")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ProtectedQuerySummary where
  hashWithSalt _salt ProtectedQuerySummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` membershipArn
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ProtectedQuerySummary where
  rnf ProtectedQuerySummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf status
