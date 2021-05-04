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
-- Module      : Network.AWS.Pinpoint.Types.ExportJobsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ExportJobsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ExportJobResponse
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
exportJobsResponse_item = Lens.lens (\ExportJobsResponse' {item} -> item) (\s@ExportJobsResponse' {} a -> s {item = a} :: ExportJobsResponse) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ExportJobsResponse where
  parseJSON =
    Prelude.withObject
      "ExportJobsResponse"
      ( \x ->
          ExportJobsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ExportJobsResponse

instance Prelude.NFData ExportJobsResponse
