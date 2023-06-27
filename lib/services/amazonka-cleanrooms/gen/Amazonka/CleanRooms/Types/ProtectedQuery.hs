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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQuery where

import Amazonka.CleanRooms.Types.ProtectedQueryError
import Amazonka.CleanRooms.Types.ProtectedQueryResult
import Amazonka.CleanRooms.Types.ProtectedQueryResultConfiguration
import Amazonka.CleanRooms.Types.ProtectedQuerySQLParameters
import Amazonka.CleanRooms.Types.ProtectedQueryStatistics
import Amazonka.CleanRooms.Types.ProtectedQueryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for an AWS Clean Rooms protected query.
--
-- /See:/ 'newProtectedQuery' smart constructor.
data ProtectedQuery = ProtectedQuery'
  { -- | An error thrown by the protected query.
    error :: Prelude.Maybe ProtectedQueryError,
    -- | The result of the protected query.
    result :: Prelude.Maybe ProtectedQueryResult,
    -- | Statistics about protected query execution.
    statistics :: Prelude.Maybe ProtectedQueryStatistics,
    -- | The identifier for a protected query instance.
    id :: Prelude.Text,
    -- | The identifier for the membership.
    membershipId :: Prelude.Text,
    -- | The ARN of the membership.
    membershipArn :: Prelude.Text,
    -- | The time at which the protected query was created.
    createTime :: Data.POSIX,
    -- | The protected query SQL parameters.
    sqlParameters :: Data.Sensitive ProtectedQuerySQLParameters,
    -- | The status of the query.
    status :: ProtectedQueryStatus,
    -- | Contains any details needed to write the query results.
    resultConfiguration :: ProtectedQueryResultConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'protectedQuery_error' - An error thrown by the protected query.
--
-- 'result', 'protectedQuery_result' - The result of the protected query.
--
-- 'statistics', 'protectedQuery_statistics' - Statistics about protected query execution.
--
-- 'id', 'protectedQuery_id' - The identifier for a protected query instance.
--
-- 'membershipId', 'protectedQuery_membershipId' - The identifier for the membership.
--
-- 'membershipArn', 'protectedQuery_membershipArn' - The ARN of the membership.
--
-- 'createTime', 'protectedQuery_createTime' - The time at which the protected query was created.
--
-- 'sqlParameters', 'protectedQuery_sqlParameters' - The protected query SQL parameters.
--
-- 'status', 'protectedQuery_status' - The status of the query.
--
-- 'resultConfiguration', 'protectedQuery_resultConfiguration' - Contains any details needed to write the query results.
newProtectedQuery ::
  -- | 'id'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'membershipArn'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'sqlParameters'
  ProtectedQuerySQLParameters ->
  -- | 'status'
  ProtectedQueryStatus ->
  -- | 'resultConfiguration'
  ProtectedQueryResultConfiguration ->
  ProtectedQuery
newProtectedQuery
  pId_
  pMembershipId_
  pMembershipArn_
  pCreateTime_
  pSqlParameters_
  pStatus_
  pResultConfiguration_ =
    ProtectedQuery'
      { error = Prelude.Nothing,
        result = Prelude.Nothing,
        statistics = Prelude.Nothing,
        id = pId_,
        membershipId = pMembershipId_,
        membershipArn = pMembershipArn_,
        createTime = Data._Time Lens.# pCreateTime_,
        sqlParameters =
          Data._Sensitive Lens.# pSqlParameters_,
        status = pStatus_,
        resultConfiguration = pResultConfiguration_
      }

-- | An error thrown by the protected query.
protectedQuery_error :: Lens.Lens' ProtectedQuery (Prelude.Maybe ProtectedQueryError)
protectedQuery_error = Lens.lens (\ProtectedQuery' {error} -> error) (\s@ProtectedQuery' {} a -> s {error = a} :: ProtectedQuery)

-- | The result of the protected query.
protectedQuery_result :: Lens.Lens' ProtectedQuery (Prelude.Maybe ProtectedQueryResult)
protectedQuery_result = Lens.lens (\ProtectedQuery' {result} -> result) (\s@ProtectedQuery' {} a -> s {result = a} :: ProtectedQuery)

-- | Statistics about protected query execution.
protectedQuery_statistics :: Lens.Lens' ProtectedQuery (Prelude.Maybe ProtectedQueryStatistics)
protectedQuery_statistics = Lens.lens (\ProtectedQuery' {statistics} -> statistics) (\s@ProtectedQuery' {} a -> s {statistics = a} :: ProtectedQuery)

-- | The identifier for a protected query instance.
protectedQuery_id :: Lens.Lens' ProtectedQuery Prelude.Text
protectedQuery_id = Lens.lens (\ProtectedQuery' {id} -> id) (\s@ProtectedQuery' {} a -> s {id = a} :: ProtectedQuery)

-- | The identifier for the membership.
protectedQuery_membershipId :: Lens.Lens' ProtectedQuery Prelude.Text
protectedQuery_membershipId = Lens.lens (\ProtectedQuery' {membershipId} -> membershipId) (\s@ProtectedQuery' {} a -> s {membershipId = a} :: ProtectedQuery)

-- | The ARN of the membership.
protectedQuery_membershipArn :: Lens.Lens' ProtectedQuery Prelude.Text
protectedQuery_membershipArn = Lens.lens (\ProtectedQuery' {membershipArn} -> membershipArn) (\s@ProtectedQuery' {} a -> s {membershipArn = a} :: ProtectedQuery)

-- | The time at which the protected query was created.
protectedQuery_createTime :: Lens.Lens' ProtectedQuery Prelude.UTCTime
protectedQuery_createTime = Lens.lens (\ProtectedQuery' {createTime} -> createTime) (\s@ProtectedQuery' {} a -> s {createTime = a} :: ProtectedQuery) Prelude.. Data._Time

-- | The protected query SQL parameters.
protectedQuery_sqlParameters :: Lens.Lens' ProtectedQuery ProtectedQuerySQLParameters
protectedQuery_sqlParameters = Lens.lens (\ProtectedQuery' {sqlParameters} -> sqlParameters) (\s@ProtectedQuery' {} a -> s {sqlParameters = a} :: ProtectedQuery) Prelude.. Data._Sensitive

-- | The status of the query.
protectedQuery_status :: Lens.Lens' ProtectedQuery ProtectedQueryStatus
protectedQuery_status = Lens.lens (\ProtectedQuery' {status} -> status) (\s@ProtectedQuery' {} a -> s {status = a} :: ProtectedQuery)

-- | Contains any details needed to write the query results.
protectedQuery_resultConfiguration :: Lens.Lens' ProtectedQuery ProtectedQueryResultConfiguration
protectedQuery_resultConfiguration = Lens.lens (\ProtectedQuery' {resultConfiguration} -> resultConfiguration) (\s@ProtectedQuery' {} a -> s {resultConfiguration = a} :: ProtectedQuery)

instance Data.FromJSON ProtectedQuery where
  parseJSON =
    Data.withObject
      "ProtectedQuery"
      ( \x ->
          ProtectedQuery'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..:? "result")
            Prelude.<*> (x Data..:? "statistics")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "membershipId")
            Prelude.<*> (x Data..: "membershipArn")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "sqlParameters")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "resultConfiguration")
      )

instance Prelude.Hashable ProtectedQuery where
  hashWithSalt _salt ProtectedQuery' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` membershipArn
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` sqlParameters
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resultConfiguration

instance Prelude.NFData ProtectedQuery where
  rnf ProtectedQuery' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf sqlParameters
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resultConfiguration
