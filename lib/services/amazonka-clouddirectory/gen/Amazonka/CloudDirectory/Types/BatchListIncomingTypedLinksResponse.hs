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
-- Module      : Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinksResponse where

import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListIncomingTypedLinks response operation.
--
-- /See:/ 'newBatchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Prelude.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListIncomingTypedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkSpecifiers', 'batchListIncomingTypedLinksResponse_linkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- 'nextToken', 'batchListIncomingTypedLinksResponse_nextToken' - The pagination token.
newBatchListIncomingTypedLinksResponse ::
  BatchListIncomingTypedLinksResponse
newBatchListIncomingTypedLinksResponse =
  BatchListIncomingTypedLinksResponse'
    { linkSpecifiers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Returns one or more typed link specifiers as output.
batchListIncomingTypedLinksResponse_linkSpecifiers :: Lens.Lens' BatchListIncomingTypedLinksResponse (Prelude.Maybe [TypedLinkSpecifier])
batchListIncomingTypedLinksResponse_linkSpecifiers = Lens.lens (\BatchListIncomingTypedLinksResponse' {linkSpecifiers} -> linkSpecifiers) (\s@BatchListIncomingTypedLinksResponse' {} a -> s {linkSpecifiers = a} :: BatchListIncomingTypedLinksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token.
batchListIncomingTypedLinksResponse_nextToken :: Lens.Lens' BatchListIncomingTypedLinksResponse (Prelude.Maybe Prelude.Text)
batchListIncomingTypedLinksResponse_nextToken = Lens.lens (\BatchListIncomingTypedLinksResponse' {nextToken} -> nextToken) (\s@BatchListIncomingTypedLinksResponse' {} a -> s {nextToken = a} :: BatchListIncomingTypedLinksResponse)

instance
  Data.FromJSON
    BatchListIncomingTypedLinksResponse
  where
  parseJSON =
    Data.withObject
      "BatchListIncomingTypedLinksResponse"
      ( \x ->
          BatchListIncomingTypedLinksResponse'
            Prelude.<$> (x Data..:? "LinkSpecifiers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NextToken")
      )

instance
  Prelude.Hashable
    BatchListIncomingTypedLinksResponse
  where
  hashWithSalt
    _salt
    BatchListIncomingTypedLinksResponse' {..} =
      _salt `Prelude.hashWithSalt` linkSpecifiers
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    BatchListIncomingTypedLinksResponse
  where
  rnf BatchListIncomingTypedLinksResponse' {..} =
    Prelude.rnf linkSpecifiers
      `Prelude.seq` Prelude.rnf nextToken
