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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.GroupResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.GroupResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
import qualified Amazonka.Prelude as Prelude

-- | Show Group Result.
--
-- /See:/ 'newGroupResult' smart constructor.
data GroupResult = GroupResult'
  { -- | Group result ID.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | Group Result Name.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | Tests under Group Result.
    tests :: Prelude.Maybe [TestCaseRun]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'groupResult_groupId' - Group result ID.
--
-- 'groupName', 'groupResult_groupName' - Group Result Name.
--
-- 'tests', 'groupResult_tests' - Tests under Group Result.
newGroupResult ::
  GroupResult
newGroupResult =
  GroupResult'
    { groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      tests = Prelude.Nothing
    }

-- | Group result ID.
groupResult_groupId :: Lens.Lens' GroupResult (Prelude.Maybe Prelude.Text)
groupResult_groupId = Lens.lens (\GroupResult' {groupId} -> groupId) (\s@GroupResult' {} a -> s {groupId = a} :: GroupResult)

-- | Group Result Name.
groupResult_groupName :: Lens.Lens' GroupResult (Prelude.Maybe Prelude.Text)
groupResult_groupName = Lens.lens (\GroupResult' {groupName} -> groupName) (\s@GroupResult' {} a -> s {groupName = a} :: GroupResult)

-- | Tests under Group Result.
groupResult_tests :: Lens.Lens' GroupResult (Prelude.Maybe [TestCaseRun])
groupResult_tests = Lens.lens (\GroupResult' {tests} -> tests) (\s@GroupResult' {} a -> s {tests = a} :: GroupResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GroupResult where
  parseJSON =
    Data.withObject
      "GroupResult"
      ( \x ->
          GroupResult'
            Prelude.<$> (x Data..:? "groupId")
            Prelude.<*> (x Data..:? "groupName")
            Prelude.<*> (x Data..:? "tests" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GroupResult where
  hashWithSalt _salt GroupResult' {..} =
    _salt `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` tests

instance Prelude.NFData GroupResult where
  rnf GroupResult' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf tests
