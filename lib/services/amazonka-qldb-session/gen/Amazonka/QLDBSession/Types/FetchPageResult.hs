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
-- Module      : Amazonka.QLDBSession.Types.FetchPageResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.FetchPageResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.Page
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the page that was fetched.
--
-- /See:/ 'newFetchPageResult' smart constructor.
data FetchPageResult = FetchPageResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | Contains metrics about the number of I\/O requests that were consumed.
    consumedIOs :: Prelude.Maybe IOUsage,
    -- | Contains details of the fetched page.
    page :: Prelude.Maybe Page
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FetchPageResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timingInformation', 'fetchPageResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'consumedIOs', 'fetchPageResult_consumedIOs' - Contains metrics about the number of I\/O requests that were consumed.
--
-- 'page', 'fetchPageResult_page' - Contains details of the fetched page.
newFetchPageResult ::
  FetchPageResult
newFetchPageResult =
  FetchPageResult'
    { timingInformation =
        Prelude.Nothing,
      consumedIOs = Prelude.Nothing,
      page = Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
fetchPageResult_timingInformation :: Lens.Lens' FetchPageResult (Prelude.Maybe TimingInformation)
fetchPageResult_timingInformation = Lens.lens (\FetchPageResult' {timingInformation} -> timingInformation) (\s@FetchPageResult' {} a -> s {timingInformation = a} :: FetchPageResult)

-- | Contains metrics about the number of I\/O requests that were consumed.
fetchPageResult_consumedIOs :: Lens.Lens' FetchPageResult (Prelude.Maybe IOUsage)
fetchPageResult_consumedIOs = Lens.lens (\FetchPageResult' {consumedIOs} -> consumedIOs) (\s@FetchPageResult' {} a -> s {consumedIOs = a} :: FetchPageResult)

-- | Contains details of the fetched page.
fetchPageResult_page :: Lens.Lens' FetchPageResult (Prelude.Maybe Page)
fetchPageResult_page = Lens.lens (\FetchPageResult' {page} -> page) (\s@FetchPageResult' {} a -> s {page = a} :: FetchPageResult)

instance Core.FromJSON FetchPageResult where
  parseJSON =
    Core.withObject
      "FetchPageResult"
      ( \x ->
          FetchPageResult'
            Prelude.<$> (x Core..:? "TimingInformation")
            Prelude.<*> (x Core..:? "ConsumedIOs")
            Prelude.<*> (x Core..:? "Page")
      )

instance Prelude.Hashable FetchPageResult where
  hashWithSalt _salt FetchPageResult' {..} =
    _salt `Prelude.hashWithSalt` timingInformation
      `Prelude.hashWithSalt` consumedIOs
      `Prelude.hashWithSalt` page

instance Prelude.NFData FetchPageResult where
  rnf FetchPageResult' {..} =
    Prelude.rnf timingInformation
      `Prelude.seq` Prelude.rnf consumedIOs
      `Prelude.seq` Prelude.rnf page
