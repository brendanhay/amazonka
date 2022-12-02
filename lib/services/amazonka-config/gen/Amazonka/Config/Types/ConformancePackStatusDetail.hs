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
-- Module      : Amazonka.Config.Types.ConformancePackStatusDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackStatusDetail where

import Amazonka.Config.Types.ConformancePackState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status details of a conformance pack.
--
-- /See:/ 'newConformancePackStatusDetail' smart constructor.
data ConformancePackStatusDetail = ConformancePackStatusDetail'
  { -- | The reason of conformance pack creation failure.
    conformancePackStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Last time when conformation pack creation and update was successful.
    lastUpdateCompletedTime :: Prelude.Maybe Data.POSIX,
    -- | Name of the conformance pack.
    conformancePackName :: Prelude.Text,
    -- | ID of the conformance pack.
    conformancePackId :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of comformance pack.
    conformancePackArn :: Prelude.Text,
    -- | Indicates deployment status of conformance pack.
    --
    -- Config sets the state of the conformance pack to:
    --
    -- -   CREATE_IN_PROGRESS when a conformance pack creation is in progress
    --     for an account.
    --
    -- -   CREATE_COMPLETE when a conformance pack has been successfully
    --     created in your account.
    --
    -- -   CREATE_FAILED when a conformance pack creation failed in your
    --     account.
    --
    -- -   DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
    --
    -- -   DELETE_FAILED when a conformance pack deletion failed in your
    --     account.
    conformancePackState :: ConformancePackState,
    -- | Amazon Resource Name (ARN) of CloudFormation stack.
    stackArn :: Prelude.Text,
    -- | Last time when conformation pack creation and update was requested.
    lastUpdateRequestedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackStatusDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackStatusReason', 'conformancePackStatusDetail_conformancePackStatusReason' - The reason of conformance pack creation failure.
--
-- 'lastUpdateCompletedTime', 'conformancePackStatusDetail_lastUpdateCompletedTime' - Last time when conformation pack creation and update was successful.
--
-- 'conformancePackName', 'conformancePackStatusDetail_conformancePackName' - Name of the conformance pack.
--
-- 'conformancePackId', 'conformancePackStatusDetail_conformancePackId' - ID of the conformance pack.
--
-- 'conformancePackArn', 'conformancePackStatusDetail_conformancePackArn' - Amazon Resource Name (ARN) of comformance pack.
--
-- 'conformancePackState', 'conformancePackStatusDetail_conformancePackState' - Indicates deployment status of conformance pack.
--
-- Config sets the state of the conformance pack to:
--
-- -   CREATE_IN_PROGRESS when a conformance pack creation is in progress
--     for an account.
--
-- -   CREATE_COMPLETE when a conformance pack has been successfully
--     created in your account.
--
-- -   CREATE_FAILED when a conformance pack creation failed in your
--     account.
--
-- -   DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
--
-- -   DELETE_FAILED when a conformance pack deletion failed in your
--     account.
--
-- 'stackArn', 'conformancePackStatusDetail_stackArn' - Amazon Resource Name (ARN) of CloudFormation stack.
--
-- 'lastUpdateRequestedTime', 'conformancePackStatusDetail_lastUpdateRequestedTime' - Last time when conformation pack creation and update was requested.
newConformancePackStatusDetail ::
  -- | 'conformancePackName'
  Prelude.Text ->
  -- | 'conformancePackId'
  Prelude.Text ->
  -- | 'conformancePackArn'
  Prelude.Text ->
  -- | 'conformancePackState'
  ConformancePackState ->
  -- | 'stackArn'
  Prelude.Text ->
  -- | 'lastUpdateRequestedTime'
  Prelude.UTCTime ->
  ConformancePackStatusDetail
newConformancePackStatusDetail
  pConformancePackName_
  pConformancePackId_
  pConformancePackArn_
  pConformancePackState_
  pStackArn_
  pLastUpdateRequestedTime_ =
    ConformancePackStatusDetail'
      { conformancePackStatusReason =
          Prelude.Nothing,
        lastUpdateCompletedTime = Prelude.Nothing,
        conformancePackName = pConformancePackName_,
        conformancePackId = pConformancePackId_,
        conformancePackArn = pConformancePackArn_,
        conformancePackState = pConformancePackState_,
        stackArn = pStackArn_,
        lastUpdateRequestedTime =
          Data._Time Lens.# pLastUpdateRequestedTime_
      }

-- | The reason of conformance pack creation failure.
conformancePackStatusDetail_conformancePackStatusReason :: Lens.Lens' ConformancePackStatusDetail (Prelude.Maybe Prelude.Text)
conformancePackStatusDetail_conformancePackStatusReason = Lens.lens (\ConformancePackStatusDetail' {conformancePackStatusReason} -> conformancePackStatusReason) (\s@ConformancePackStatusDetail' {} a -> s {conformancePackStatusReason = a} :: ConformancePackStatusDetail)

-- | Last time when conformation pack creation and update was successful.
conformancePackStatusDetail_lastUpdateCompletedTime :: Lens.Lens' ConformancePackStatusDetail (Prelude.Maybe Prelude.UTCTime)
conformancePackStatusDetail_lastUpdateCompletedTime = Lens.lens (\ConformancePackStatusDetail' {lastUpdateCompletedTime} -> lastUpdateCompletedTime) (\s@ConformancePackStatusDetail' {} a -> s {lastUpdateCompletedTime = a} :: ConformancePackStatusDetail) Prelude.. Lens.mapping Data._Time

-- | Name of the conformance pack.
conformancePackStatusDetail_conformancePackName :: Lens.Lens' ConformancePackStatusDetail Prelude.Text
conformancePackStatusDetail_conformancePackName = Lens.lens (\ConformancePackStatusDetail' {conformancePackName} -> conformancePackName) (\s@ConformancePackStatusDetail' {} a -> s {conformancePackName = a} :: ConformancePackStatusDetail)

-- | ID of the conformance pack.
conformancePackStatusDetail_conformancePackId :: Lens.Lens' ConformancePackStatusDetail Prelude.Text
conformancePackStatusDetail_conformancePackId = Lens.lens (\ConformancePackStatusDetail' {conformancePackId} -> conformancePackId) (\s@ConformancePackStatusDetail' {} a -> s {conformancePackId = a} :: ConformancePackStatusDetail)

-- | Amazon Resource Name (ARN) of comformance pack.
conformancePackStatusDetail_conformancePackArn :: Lens.Lens' ConformancePackStatusDetail Prelude.Text
conformancePackStatusDetail_conformancePackArn = Lens.lens (\ConformancePackStatusDetail' {conformancePackArn} -> conformancePackArn) (\s@ConformancePackStatusDetail' {} a -> s {conformancePackArn = a} :: ConformancePackStatusDetail)

-- | Indicates deployment status of conformance pack.
--
-- Config sets the state of the conformance pack to:
--
-- -   CREATE_IN_PROGRESS when a conformance pack creation is in progress
--     for an account.
--
-- -   CREATE_COMPLETE when a conformance pack has been successfully
--     created in your account.
--
-- -   CREATE_FAILED when a conformance pack creation failed in your
--     account.
--
-- -   DELETE_IN_PROGRESS when a conformance pack deletion is in progress.
--
-- -   DELETE_FAILED when a conformance pack deletion failed in your
--     account.
conformancePackStatusDetail_conformancePackState :: Lens.Lens' ConformancePackStatusDetail ConformancePackState
conformancePackStatusDetail_conformancePackState = Lens.lens (\ConformancePackStatusDetail' {conformancePackState} -> conformancePackState) (\s@ConformancePackStatusDetail' {} a -> s {conformancePackState = a} :: ConformancePackStatusDetail)

-- | Amazon Resource Name (ARN) of CloudFormation stack.
conformancePackStatusDetail_stackArn :: Lens.Lens' ConformancePackStatusDetail Prelude.Text
conformancePackStatusDetail_stackArn = Lens.lens (\ConformancePackStatusDetail' {stackArn} -> stackArn) (\s@ConformancePackStatusDetail' {} a -> s {stackArn = a} :: ConformancePackStatusDetail)

-- | Last time when conformation pack creation and update was requested.
conformancePackStatusDetail_lastUpdateRequestedTime :: Lens.Lens' ConformancePackStatusDetail Prelude.UTCTime
conformancePackStatusDetail_lastUpdateRequestedTime = Lens.lens (\ConformancePackStatusDetail' {lastUpdateRequestedTime} -> lastUpdateRequestedTime) (\s@ConformancePackStatusDetail' {} a -> s {lastUpdateRequestedTime = a} :: ConformancePackStatusDetail) Prelude.. Data._Time

instance Data.FromJSON ConformancePackStatusDetail where
  parseJSON =
    Data.withObject
      "ConformancePackStatusDetail"
      ( \x ->
          ConformancePackStatusDetail'
            Prelude.<$> (x Data..:? "ConformancePackStatusReason")
            Prelude.<*> (x Data..:? "LastUpdateCompletedTime")
            Prelude.<*> (x Data..: "ConformancePackName")
            Prelude.<*> (x Data..: "ConformancePackId")
            Prelude.<*> (x Data..: "ConformancePackArn")
            Prelude.<*> (x Data..: "ConformancePackState")
            Prelude.<*> (x Data..: "StackArn")
            Prelude.<*> (x Data..: "LastUpdateRequestedTime")
      )

instance Prelude.Hashable ConformancePackStatusDetail where
  hashWithSalt _salt ConformancePackStatusDetail' {..} =
    _salt
      `Prelude.hashWithSalt` conformancePackStatusReason
      `Prelude.hashWithSalt` lastUpdateCompletedTime
      `Prelude.hashWithSalt` conformancePackName
      `Prelude.hashWithSalt` conformancePackId
      `Prelude.hashWithSalt` conformancePackArn
      `Prelude.hashWithSalt` conformancePackState
      `Prelude.hashWithSalt` stackArn
      `Prelude.hashWithSalt` lastUpdateRequestedTime

instance Prelude.NFData ConformancePackStatusDetail where
  rnf ConformancePackStatusDetail' {..} =
    Prelude.rnf conformancePackStatusReason
      `Prelude.seq` Prelude.rnf lastUpdateCompletedTime
      `Prelude.seq` Prelude.rnf conformancePackName
      `Prelude.seq` Prelude.rnf conformancePackId
      `Prelude.seq` Prelude.rnf conformancePackArn
      `Prelude.seq` Prelude.rnf conformancePackState
      `Prelude.seq` Prelude.rnf stackArn
      `Prelude.seq` Prelude.rnf lastUpdateRequestedTime
