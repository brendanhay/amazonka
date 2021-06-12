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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListIncomingTypedLinks response operation.
--
-- /See:/ 'newBatchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Core.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Returns one or more typed link specifiers as output.
batchListIncomingTypedLinksResponse_linkSpecifiers :: Lens.Lens' BatchListIncomingTypedLinksResponse (Core.Maybe [TypedLinkSpecifier])
batchListIncomingTypedLinksResponse_linkSpecifiers = Lens.lens (\BatchListIncomingTypedLinksResponse' {linkSpecifiers} -> linkSpecifiers) (\s@BatchListIncomingTypedLinksResponse' {} a -> s {linkSpecifiers = a} :: BatchListIncomingTypedLinksResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token.
batchListIncomingTypedLinksResponse_nextToken :: Lens.Lens' BatchListIncomingTypedLinksResponse (Core.Maybe Core.Text)
batchListIncomingTypedLinksResponse_nextToken = Lens.lens (\BatchListIncomingTypedLinksResponse' {nextToken} -> nextToken) (\s@BatchListIncomingTypedLinksResponse' {} a -> s {nextToken = a} :: BatchListIncomingTypedLinksResponse)

instance
  Core.FromJSON
    BatchListIncomingTypedLinksResponse
  where
  parseJSON =
    Core.withObject
      "BatchListIncomingTypedLinksResponse"
      ( \x ->
          BatchListIncomingTypedLinksResponse'
            Core.<$> (x Core..:? "LinkSpecifiers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
      )

instance
  Core.Hashable
    BatchListIncomingTypedLinksResponse

instance
  Core.NFData
    BatchListIncomingTypedLinksResponse
