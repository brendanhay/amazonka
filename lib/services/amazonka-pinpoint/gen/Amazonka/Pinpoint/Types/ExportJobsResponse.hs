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
-- Module      : Amazonka.Pinpoint.Types.ExportJobsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ExportJobsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ExportJobResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the export jobs that are associated with
-- an application or segment. An export job is a job that exports endpoint
-- definitions to a file.
--
-- /See:/ 'newExportJobsResponse' smart constructor.
data ExportJobsResponse = ExportJobsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each export job that\'s associated with
    -- the application (Export Jobs resource) or segment (Segment Export Jobs
    -- resource).
    item :: [ExportJobResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'exportJobsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'exportJobsResponse_item' - An array of responses, one for each export job that\'s associated with
-- the application (Export Jobs resource) or segment (Segment Export Jobs
-- resource).
newExportJobsResponse ::
  ExportJobsResponse
newExportJobsResponse =
  ExportJobsResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
exportJobsResponse_nextToken :: Lens.Lens' ExportJobsResponse (Prelude.Maybe Prelude.Text)
exportJobsResponse_nextToken = Lens.lens (\ExportJobsResponse' {nextToken} -> nextToken) (\s@ExportJobsResponse' {} a -> s {nextToken = a} :: ExportJobsResponse)

-- | An array of responses, one for each export job that\'s associated with
-- the application (Export Jobs resource) or segment (Segment Export Jobs
-- resource).
exportJobsResponse_item :: Lens.Lens' ExportJobsResponse [ExportJobResponse]
exportJobsResponse_item = Lens.lens (\ExportJobsResponse' {item} -> item) (\s@ExportJobsResponse' {} a -> s {item = a} :: ExportJobsResponse) Prelude.. Lens.coerced

instance Data.FromJSON ExportJobsResponse where
  parseJSON =
    Data.withObject
      "ExportJobsResponse"
      ( \x ->
          ExportJobsResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExportJobsResponse where
  hashWithSalt _salt ExportJobsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData ExportJobsResponse where
  rnf ExportJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf item
