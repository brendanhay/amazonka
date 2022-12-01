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
import Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
import qualified Amazonka.Prelude as Prelude

-- | Show Group Result.
--
-- /See:/ 'newGroupResult' smart constructor.
data GroupResult = GroupResult'
  { -- | Tests under Group Result.
    tests :: Prelude.Maybe [TestCaseRun],
    -- | Group Result Name.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | Group result ID.
    groupId :: Prelude.Maybe Prelude.Text
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
-- 'tests', 'groupResult_tests' - Tests under Group Result.
--
-- 'groupName', 'groupResult_groupName' - Group Result Name.
--
-- 'groupId', 'groupResult_groupId' - Group result ID.
newGroupResult ::
  GroupResult
newGroupResult =
  GroupResult'
    { tests = Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | Tests under Group Result.
groupResult_tests :: Lens.Lens' GroupResult (Prelude.Maybe [TestCaseRun])
groupResult_tests = Lens.lens (\GroupResult' {tests} -> tests) (\s@GroupResult' {} a -> s {tests = a} :: GroupResult) Prelude.. Lens.mapping Lens.coerced

-- | Group Result Name.
groupResult_groupName :: Lens.Lens' GroupResult (Prelude.Maybe Prelude.Text)
groupResult_groupName = Lens.lens (\GroupResult' {groupName} -> groupName) (\s@GroupResult' {} a -> s {groupName = a} :: GroupResult)

-- | Group result ID.
groupResult_groupId :: Lens.Lens' GroupResult (Prelude.Maybe Prelude.Text)
groupResult_groupId = Lens.lens (\GroupResult' {groupId} -> groupId) (\s@GroupResult' {} a -> s {groupId = a} :: GroupResult)

instance Core.FromJSON GroupResult where
  parseJSON =
    Core.withObject
      "GroupResult"
      ( \x ->
          GroupResult'
            Prelude.<$> (x Core..:? "tests" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "groupName")
            Prelude.<*> (x Core..:? "groupId")
      )

instance Prelude.Hashable GroupResult where
  hashWithSalt _salt GroupResult' {..} =
    _salt `Prelude.hashWithSalt` tests
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData GroupResult where
  rnf GroupResult' {..} =
    Prelude.rnf tests
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupId
