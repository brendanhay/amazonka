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
-- Module      : Amazonka.EMRServerless.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.ApplicationState
import Amazonka.EMRServerless.Types.Architecture
import qualified Amazonka.Prelude as Prelude

-- | The summary of attributes associated with an application.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | The CPU architecture of an application.
    architecture :: Prelude.Maybe Architecture,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state details of the application.
    stateDetails :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    id :: Prelude.Text,
    -- | The ARN of the application.
    arn :: Prelude.Text,
    -- | The EMR release associated with the application.
    releaseLabel :: Prelude.Text,
    -- | The type of application, such as Spark or Hive.
    type' :: Prelude.Text,
    -- | The state of the application.
    state :: ApplicationState,
    -- | The date and time when the application was created.
    createdAt :: Data.POSIX,
    -- | The date and time when the application was last updated.
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'applicationSummary_architecture' - The CPU architecture of an application.
--
-- 'name', 'applicationSummary_name' - The name of the application.
--
-- 'stateDetails', 'applicationSummary_stateDetails' - The state details of the application.
--
-- 'id', 'applicationSummary_id' - The ID of the application.
--
-- 'arn', 'applicationSummary_arn' - The ARN of the application.
--
-- 'releaseLabel', 'applicationSummary_releaseLabel' - The EMR release associated with the application.
--
-- 'type'', 'applicationSummary_type' - The type of application, such as Spark or Hive.
--
-- 'state', 'applicationSummary_state' - The state of the application.
--
-- 'createdAt', 'applicationSummary_createdAt' - The date and time when the application was created.
--
-- 'updatedAt', 'applicationSummary_updatedAt' - The date and time when the application was last updated.
newApplicationSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'state'
  ApplicationState ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  ApplicationSummary
newApplicationSummary
  pId_
  pArn_
  pReleaseLabel_
  pType_
  pState_
  pCreatedAt_
  pUpdatedAt_ =
    ApplicationSummary'
      { architecture = Prelude.Nothing,
        name = Prelude.Nothing,
        stateDetails = Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        releaseLabel = pReleaseLabel_,
        type' = pType_,
        state = pState_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | The CPU architecture of an application.
applicationSummary_architecture :: Lens.Lens' ApplicationSummary (Prelude.Maybe Architecture)
applicationSummary_architecture = Lens.lens (\ApplicationSummary' {architecture} -> architecture) (\s@ApplicationSummary' {} a -> s {architecture = a} :: ApplicationSummary)

-- | The name of the application.
applicationSummary_name :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_name = Lens.lens (\ApplicationSummary' {name} -> name) (\s@ApplicationSummary' {} a -> s {name = a} :: ApplicationSummary)

-- | The state details of the application.
applicationSummary_stateDetails :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_stateDetails = Lens.lens (\ApplicationSummary' {stateDetails} -> stateDetails) (\s@ApplicationSummary' {} a -> s {stateDetails = a} :: ApplicationSummary)

-- | The ID of the application.
applicationSummary_id :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_id = Lens.lens (\ApplicationSummary' {id} -> id) (\s@ApplicationSummary' {} a -> s {id = a} :: ApplicationSummary)

-- | The ARN of the application.
applicationSummary_arn :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_arn = Lens.lens (\ApplicationSummary' {arn} -> arn) (\s@ApplicationSummary' {} a -> s {arn = a} :: ApplicationSummary)

-- | The EMR release associated with the application.
applicationSummary_releaseLabel :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_releaseLabel = Lens.lens (\ApplicationSummary' {releaseLabel} -> releaseLabel) (\s@ApplicationSummary' {} a -> s {releaseLabel = a} :: ApplicationSummary)

-- | The type of application, such as Spark or Hive.
applicationSummary_type :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_type = Lens.lens (\ApplicationSummary' {type'} -> type') (\s@ApplicationSummary' {} a -> s {type' = a} :: ApplicationSummary)

-- | The state of the application.
applicationSummary_state :: Lens.Lens' ApplicationSummary ApplicationState
applicationSummary_state = Lens.lens (\ApplicationSummary' {state} -> state) (\s@ApplicationSummary' {} a -> s {state = a} :: ApplicationSummary)

-- | The date and time when the application was created.
applicationSummary_createdAt :: Lens.Lens' ApplicationSummary Prelude.UTCTime
applicationSummary_createdAt = Lens.lens (\ApplicationSummary' {createdAt} -> createdAt) (\s@ApplicationSummary' {} a -> s {createdAt = a} :: ApplicationSummary) Prelude.. Data._Time

-- | The date and time when the application was last updated.
applicationSummary_updatedAt :: Lens.Lens' ApplicationSummary Prelude.UTCTime
applicationSummary_updatedAt = Lens.lens (\ApplicationSummary' {updatedAt} -> updatedAt) (\s@ApplicationSummary' {} a -> s {updatedAt = a} :: ApplicationSummary) Prelude.. Data._Time

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "architecture")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "stateDetails")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "releaseLabel")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "state")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stateDetails
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stateDetails
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
