{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Athena.Types.WorkGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroup where

import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A workgroup, which contains a name, description, creation time, state,
-- and other configuration, listed under WorkGroup$Configuration. Each
-- workgroup enables you to isolate queries for you or your group of users
-- from other queries in the same account, to configure the query results
-- location and the encryption configuration (known as workgroup settings),
-- to enable sending query metrics to Amazon CloudWatch, and to establish
-- per-query data usage control limits for all queries in a workgroup. The
-- workgroup settings override is specified in
-- EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- /See:/ 'newWorkGroup' smart constructor.
data WorkGroup = WorkGroup'
  { -- | The date and time the workgroup was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The configuration of the workgroup, which includes the location in
    -- Amazon S3 where query results are stored, the encryption configuration,
    -- if any, used for query results; whether the Amazon CloudWatch Metrics
    -- are enabled for the workgroup; whether workgroup settings override
    -- client-side settings; and the data usage limits for the amount of data
    -- scanned per query or per workgroup. The workgroup settings override is
    -- specified in EnforceWorkGroupConfiguration (true\/false) in the
    -- WorkGroupConfiguration. See
    -- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
    configuration :: Prelude.Maybe WorkGroupConfiguration,
    -- | The state of the workgroup: ENABLED or DISABLED.
    state :: Prelude.Maybe WorkGroupState,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workgroup name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'workGroup_creationTime' - The date and time the workgroup was created.
--
-- 'configuration', 'workGroup_configuration' - The configuration of the workgroup, which includes the location in
-- Amazon S3 where query results are stored, the encryption configuration,
-- if any, used for query results; whether the Amazon CloudWatch Metrics
-- are enabled for the workgroup; whether workgroup settings override
-- client-side settings; and the data usage limits for the amount of data
-- scanned per query or per workgroup. The workgroup settings override is
-- specified in EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
--
-- 'state', 'workGroup_state' - The state of the workgroup: ENABLED or DISABLED.
--
-- 'description', 'workGroup_description' - The workgroup description.
--
-- 'name', 'workGroup_name' - The workgroup name.
newWorkGroup ::
  -- | 'name'
  Prelude.Text ->
  WorkGroup
newWorkGroup pName_ =
  WorkGroup'
    { creationTime = Prelude.Nothing,
      configuration = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | The date and time the workgroup was created.
workGroup_creationTime :: Lens.Lens' WorkGroup (Prelude.Maybe Prelude.UTCTime)
workGroup_creationTime = Lens.lens (\WorkGroup' {creationTime} -> creationTime) (\s@WorkGroup' {} a -> s {creationTime = a} :: WorkGroup) Prelude.. Lens.mapping Prelude._Time

-- | The configuration of the workgroup, which includes the location in
-- Amazon S3 where query results are stored, the encryption configuration,
-- if any, used for query results; whether the Amazon CloudWatch Metrics
-- are enabled for the workgroup; whether workgroup settings override
-- client-side settings; and the data usage limits for the amount of data
-- scanned per query or per workgroup. The workgroup settings override is
-- specified in EnforceWorkGroupConfiguration (true\/false) in the
-- WorkGroupConfiguration. See
-- WorkGroupConfiguration$EnforceWorkGroupConfiguration.
workGroup_configuration :: Lens.Lens' WorkGroup (Prelude.Maybe WorkGroupConfiguration)
workGroup_configuration = Lens.lens (\WorkGroup' {configuration} -> configuration) (\s@WorkGroup' {} a -> s {configuration = a} :: WorkGroup)

-- | The state of the workgroup: ENABLED or DISABLED.
workGroup_state :: Lens.Lens' WorkGroup (Prelude.Maybe WorkGroupState)
workGroup_state = Lens.lens (\WorkGroup' {state} -> state) (\s@WorkGroup' {} a -> s {state = a} :: WorkGroup)

-- | The workgroup description.
workGroup_description :: Lens.Lens' WorkGroup (Prelude.Maybe Prelude.Text)
workGroup_description = Lens.lens (\WorkGroup' {description} -> description) (\s@WorkGroup' {} a -> s {description = a} :: WorkGroup)

-- | The workgroup name.
workGroup_name :: Lens.Lens' WorkGroup Prelude.Text
workGroup_name = Lens.lens (\WorkGroup' {name} -> name) (\s@WorkGroup' {} a -> s {name = a} :: WorkGroup)

instance Prelude.FromJSON WorkGroup where
  parseJSON =
    Prelude.withObject
      "WorkGroup"
      ( \x ->
          WorkGroup'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Configuration")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable WorkGroup

instance Prelude.NFData WorkGroup
