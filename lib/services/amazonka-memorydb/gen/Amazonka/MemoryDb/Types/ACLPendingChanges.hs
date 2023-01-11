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
-- Module      : Amazonka.MemoryDb.Types.ACLPendingChanges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.ACLPendingChanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the updates being applied to the ACL.
--
-- /See:/ 'newACLPendingChanges' smart constructor.
data ACLPendingChanges = ACLPendingChanges'
  { -- | A list of users being added to the ACL
    userNamesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | A list of user names being removed from the ACL
    userNamesToRemove :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ACLPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userNamesToAdd', 'aCLPendingChanges_userNamesToAdd' - A list of users being added to the ACL
--
-- 'userNamesToRemove', 'aCLPendingChanges_userNamesToRemove' - A list of user names being removed from the ACL
newACLPendingChanges ::
  ACLPendingChanges
newACLPendingChanges =
  ACLPendingChanges'
    { userNamesToAdd =
        Prelude.Nothing,
      userNamesToRemove = Prelude.Nothing
    }

-- | A list of users being added to the ACL
aCLPendingChanges_userNamesToAdd :: Lens.Lens' ACLPendingChanges (Prelude.Maybe [Prelude.Text])
aCLPendingChanges_userNamesToAdd = Lens.lens (\ACLPendingChanges' {userNamesToAdd} -> userNamesToAdd) (\s@ACLPendingChanges' {} a -> s {userNamesToAdd = a} :: ACLPendingChanges) Prelude.. Lens.mapping Lens.coerced

-- | A list of user names being removed from the ACL
aCLPendingChanges_userNamesToRemove :: Lens.Lens' ACLPendingChanges (Prelude.Maybe [Prelude.Text])
aCLPendingChanges_userNamesToRemove = Lens.lens (\ACLPendingChanges' {userNamesToRemove} -> userNamesToRemove) (\s@ACLPendingChanges' {} a -> s {userNamesToRemove = a} :: ACLPendingChanges) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ACLPendingChanges where
  parseJSON =
    Data.withObject
      "ACLPendingChanges"
      ( \x ->
          ACLPendingChanges'
            Prelude.<$> (x Data..:? "UserNamesToAdd" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "UserNamesToRemove"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ACLPendingChanges where
  hashWithSalt _salt ACLPendingChanges' {..} =
    _salt `Prelude.hashWithSalt` userNamesToAdd
      `Prelude.hashWithSalt` userNamesToRemove

instance Prelude.NFData ACLPendingChanges where
  rnf ACLPendingChanges' {..} =
    Prelude.rnf userNamesToAdd
      `Prelude.seq` Prelude.rnf userNamesToRemove
