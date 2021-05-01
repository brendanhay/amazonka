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
-- Module      : Network.AWS.Support.Types.RecentCaseCommunications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.RecentCaseCommunications where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Support.Types.Communication

-- | The five most recent communications associated with the case.
--
-- /See:/ 'newRecentCaseCommunications' smart constructor.
data RecentCaseCommunications = RecentCaseCommunications'
  { -- | A resumption point for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The five most recent communications associated with the case.
    communications :: Prelude.Maybe [Communication]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
recentCaseCommunications_communications = Lens.lens (\RecentCaseCommunications' {communications} -> communications) (\s@RecentCaseCommunications' {} a -> s {communications = a} :: RecentCaseCommunications) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON RecentCaseCommunications where
  parseJSON =
    Prelude.withObject
      "RecentCaseCommunications"
      ( \x ->
          RecentCaseCommunications'
            Prelude.<$> (x Prelude..:? "nextToken")
            Prelude.<*> ( x Prelude..:? "communications"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RecentCaseCommunications

instance Prelude.NFData RecentCaseCommunications
