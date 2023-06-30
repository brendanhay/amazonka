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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.FetchPageResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.Page
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the page that was fetched.
--
-- /See:/ 'newFetchPageResult' smart constructor.
data FetchPageResult = FetchPageResult'
  { -- | Contains metrics about the number of I\/O requests that were consumed.
    consumedIOs :: Prelude.Maybe IOUsage,
    -- | Contains details of the fetched page.
    page :: Prelude.Maybe Page,
    -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation
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
-- 'consumedIOs', 'fetchPageResult_consumedIOs' - Contains metrics about the number of I\/O requests that were consumed.
--
-- 'page', 'fetchPageResult_page' - Contains details of the fetched page.
--
-- 'timingInformation', 'fetchPageResult_timingInformation' - Contains server-side performance information for the command.
newFetchPageResult ::
  FetchPageResult
newFetchPageResult =
  FetchPageResult'
    { consumedIOs = Prelude.Nothing,
      page = Prelude.Nothing,
      timingInformation = Prelude.Nothing
    }

-- | Contains metrics about the number of I\/O requests that were consumed.
fetchPageResult_consumedIOs :: Lens.Lens' FetchPageResult (Prelude.Maybe IOUsage)
fetchPageResult_consumedIOs = Lens.lens (\FetchPageResult' {consumedIOs} -> consumedIOs) (\s@FetchPageResult' {} a -> s {consumedIOs = a} :: FetchPageResult)

-- | Contains details of the fetched page.
fetchPageResult_page :: Lens.Lens' FetchPageResult (Prelude.Maybe Page)
fetchPageResult_page = Lens.lens (\FetchPageResult' {page} -> page) (\s@FetchPageResult' {} a -> s {page = a} :: FetchPageResult)

-- | Contains server-side performance information for the command.
fetchPageResult_timingInformation :: Lens.Lens' FetchPageResult (Prelude.Maybe TimingInformation)
fetchPageResult_timingInformation = Lens.lens (\FetchPageResult' {timingInformation} -> timingInformation) (\s@FetchPageResult' {} a -> s {timingInformation = a} :: FetchPageResult)

instance Data.FromJSON FetchPageResult where
  parseJSON =
    Data.withObject
      "FetchPageResult"
      ( \x ->
          FetchPageResult'
            Prelude.<$> (x Data..:? "ConsumedIOs")
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "TimingInformation")
      )

instance Prelude.Hashable FetchPageResult where
  hashWithSalt _salt FetchPageResult' {..} =
    _salt
      `Prelude.hashWithSalt` consumedIOs
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` timingInformation

instance Prelude.NFData FetchPageResult where
  rnf FetchPageResult' {..} =
    Prelude.rnf consumedIOs
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf timingInformation
