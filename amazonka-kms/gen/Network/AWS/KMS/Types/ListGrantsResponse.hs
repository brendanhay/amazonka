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
-- Module      : Network.AWS.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ListGrantsResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types.GrantListEntry
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newListGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Core.Maybe Core.Text,
    -- | A list of grants.
    grants :: Core.Maybe [GrantListEntry],
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listGrantsResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'grants', 'listGrantsResponse_grants' - A list of grants.
--
-- 'truncated', 'listGrantsResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
newListGrantsResponse ::
  ListGrantsResponse
newListGrantsResponse =
  ListGrantsResponse'
    { nextMarker = Core.Nothing,
      grants = Core.Nothing,
      truncated = Core.Nothing
    }

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listGrantsResponse_nextMarker :: Lens.Lens' ListGrantsResponse (Core.Maybe Core.Text)
listGrantsResponse_nextMarker = Lens.lens (\ListGrantsResponse' {nextMarker} -> nextMarker) (\s@ListGrantsResponse' {} a -> s {nextMarker = a} :: ListGrantsResponse)

-- | A list of grants.
listGrantsResponse_grants :: Lens.Lens' ListGrantsResponse (Core.Maybe [GrantListEntry])
listGrantsResponse_grants = Lens.lens (\ListGrantsResponse' {grants} -> grants) (\s@ListGrantsResponse' {} a -> s {grants = a} :: ListGrantsResponse) Core.. Lens.mapping Lens._Coerce

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listGrantsResponse_truncated :: Lens.Lens' ListGrantsResponse (Core.Maybe Core.Bool)
listGrantsResponse_truncated = Lens.lens (\ListGrantsResponse' {truncated} -> truncated) (\s@ListGrantsResponse' {} a -> s {truncated = a} :: ListGrantsResponse)

instance Core.FromJSON ListGrantsResponse where
  parseJSON =
    Core.withObject
      "ListGrantsResponse"
      ( \x ->
          ListGrantsResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "Grants" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Truncated")
      )

instance Core.Hashable ListGrantsResponse

instance Core.NFData ListGrantsResponse
