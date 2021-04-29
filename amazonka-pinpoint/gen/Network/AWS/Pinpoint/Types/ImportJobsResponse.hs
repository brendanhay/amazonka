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
-- Module      : Network.AWS.Pinpoint.Types.ImportJobsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ImportJobsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ImportJobResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the status and settings of all the import
-- jobs that are associated with an application or segment. An import job
-- is a job that imports endpoint definitions from one or more files.
--
-- /See:/ 'newImportJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each import job that\'s associated with
    -- the application (Import Jobs resource) or segment (Segment Import Jobs
    -- resource).
    item :: [ImportJobResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'importJobsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'importJobsResponse_item' - An array of responses, one for each import job that\'s associated with
-- the application (Import Jobs resource) or segment (Segment Import Jobs
-- resource).
newImportJobsResponse ::
  ImportJobsResponse
newImportJobsResponse =
  ImportJobsResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
importJobsResponse_nextToken :: Lens.Lens' ImportJobsResponse (Prelude.Maybe Prelude.Text)
importJobsResponse_nextToken = Lens.lens (\ImportJobsResponse' {nextToken} -> nextToken) (\s@ImportJobsResponse' {} a -> s {nextToken = a} :: ImportJobsResponse)

-- | An array of responses, one for each import job that\'s associated with
-- the application (Import Jobs resource) or segment (Segment Import Jobs
-- resource).
importJobsResponse_item :: Lens.Lens' ImportJobsResponse [ImportJobResponse]
importJobsResponse_item = Lens.lens (\ImportJobsResponse' {item} -> item) (\s@ImportJobsResponse' {} a -> s {item = a} :: ImportJobsResponse) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ImportJobsResponse where
  parseJSON =
    Prelude.withObject
      "ImportJobsResponse"
      ( \x ->
          ImportJobsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ImportJobsResponse

instance Prelude.NFData ImportJobsResponse
