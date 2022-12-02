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
-- Module      : Amazonka.Support.Types.RecentCaseCommunications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.RecentCaseCommunications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Support.Types.Communication

-- | The five most recent communications associated with the case.
--
-- /See:/ 'newRecentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
  { -- | A resumption point for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The five most recent communications associated with the case.
    communications :: Prelude.Maybe [Communication]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecentCaseCommunications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'recentCaseCommunications_nextToken' - A resumption point for pagination.
--
-- 'communications', 'recentCaseCommunications_communications' - The five most recent communications associated with the case.
newRecentCaseCommunications ::
  RecentCaseCommunications
newRecentCaseCommunications =
  RecentCaseCommunications'
    { nextToken =
        Prelude.Nothing,
      communications = Prelude.Nothing
    }

-- | A resumption point for pagination.
recentCaseCommunications_nextToken :: Lens.Lens' RecentCaseCommunications (Prelude.Maybe Prelude.Text)
recentCaseCommunications_nextToken = Lens.lens (\RecentCaseCommunications' {nextToken} -> nextToken) (\s@RecentCaseCommunications' {} a -> s {nextToken = a} :: RecentCaseCommunications)

-- | The five most recent communications associated with the case.
recentCaseCommunications_communications :: Lens.Lens' RecentCaseCommunications (Prelude.Maybe [Communication])
recentCaseCommunications_communications = Lens.lens (\RecentCaseCommunications' {communications} -> communications) (\s@RecentCaseCommunications' {} a -> s {communications = a} :: RecentCaseCommunications) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecentCaseCommunications where
  parseJSON =
    Data.withObject
      "RecentCaseCommunications"
      ( \x ->
          RecentCaseCommunications'
            Prelude.<$> (x Data..:? "nextToken")
            Prelude.<*> ( x Data..:? "communications"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RecentCaseCommunications where
  hashWithSalt _salt RecentCaseCommunications' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` communications

instance Prelude.NFData RecentCaseCommunications where
  rnf RecentCaseCommunications' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf communications
