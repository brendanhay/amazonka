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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse where

import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newBatchListObjectParentsResponse' smart constructor.
data BatchListObjectParentsResponse = BatchListObjectParentsResponse'
  { parentLinks :: Core.Maybe [ObjectIdentifierAndLinkNameTuple],
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListObjectParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentLinks', 'batchListObjectParentsResponse_parentLinks' - Undocumented member.
--
-- 'nextToken', 'batchListObjectParentsResponse_nextToken' - Undocumented member.
newBatchListObjectParentsResponse ::
  BatchListObjectParentsResponse
newBatchListObjectParentsResponse =
  BatchListObjectParentsResponse'
    { parentLinks =
        Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Undocumented member.
batchListObjectParentsResponse_parentLinks :: Lens.Lens' BatchListObjectParentsResponse (Core.Maybe [ObjectIdentifierAndLinkNameTuple])
batchListObjectParentsResponse_parentLinks = Lens.lens (\BatchListObjectParentsResponse' {parentLinks} -> parentLinks) (\s@BatchListObjectParentsResponse' {} a -> s {parentLinks = a} :: BatchListObjectParentsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
batchListObjectParentsResponse_nextToken :: Lens.Lens' BatchListObjectParentsResponse (Core.Maybe Core.Text)
batchListObjectParentsResponse_nextToken = Lens.lens (\BatchListObjectParentsResponse' {nextToken} -> nextToken) (\s@BatchListObjectParentsResponse' {} a -> s {nextToken = a} :: BatchListObjectParentsResponse)

instance Core.FromJSON BatchListObjectParentsResponse where
  parseJSON =
    Core.withObject
      "BatchListObjectParentsResponse"
      ( \x ->
          BatchListObjectParentsResponse'
            Core.<$> (x Core..:? "ParentLinks" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
      )

instance Core.Hashable BatchListObjectParentsResponse

instance Core.NFData BatchListObjectParentsResponse
