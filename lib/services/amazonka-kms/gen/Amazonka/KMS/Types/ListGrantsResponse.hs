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
-- Module      : Amazonka.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.ListGrantsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types.GrantListEntry
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newListGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { -- | A list of grants.
    grants :: Prelude.Maybe [GrantListEntry],
    -- | When @Truncated@ is true, this element is present and contains the value
    -- to use for the @Marker@ parameter in a subsequent request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items in the list. When
    -- this value is true, the list in this response is truncated. To get more
    -- items, pass the value of the @NextMarker@ element in thisresponse to the
    -- @Marker@ parameter in a subsequent request.
    truncated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'listGrantsResponse_grants' - A list of grants.
--
-- 'nextMarker', 'listGrantsResponse_nextMarker' - When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
--
-- 'truncated', 'listGrantsResponse_truncated' - A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
newListGrantsResponse ::
  ListGrantsResponse
newListGrantsResponse =
  ListGrantsResponse'
    { grants = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      truncated = Prelude.Nothing
    }

-- | A list of grants.
listGrantsResponse_grants :: Lens.Lens' ListGrantsResponse (Prelude.Maybe [GrantListEntry])
listGrantsResponse_grants = Lens.lens (\ListGrantsResponse' {grants} -> grants) (\s@ListGrantsResponse' {} a -> s {grants = a} :: ListGrantsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @Truncated@ is true, this element is present and contains the value
-- to use for the @Marker@ parameter in a subsequent request.
listGrantsResponse_nextMarker :: Lens.Lens' ListGrantsResponse (Prelude.Maybe Prelude.Text)
listGrantsResponse_nextMarker = Lens.lens (\ListGrantsResponse' {nextMarker} -> nextMarker) (\s@ListGrantsResponse' {} a -> s {nextMarker = a} :: ListGrantsResponse)

-- | A flag that indicates whether there are more items in the list. When
-- this value is true, the list in this response is truncated. To get more
-- items, pass the value of the @NextMarker@ element in thisresponse to the
-- @Marker@ parameter in a subsequent request.
listGrantsResponse_truncated :: Lens.Lens' ListGrantsResponse (Prelude.Maybe Prelude.Bool)
listGrantsResponse_truncated = Lens.lens (\ListGrantsResponse' {truncated} -> truncated) (\s@ListGrantsResponse' {} a -> s {truncated = a} :: ListGrantsResponse)

instance Data.FromJSON ListGrantsResponse where
  parseJSON =
    Data.withObject
      "ListGrantsResponse"
      ( \x ->
          ListGrantsResponse'
            Prelude.<$> (x Data..:? "Grants" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NextMarker")
            Prelude.<*> (x Data..:? "Truncated")
      )

instance Prelude.Hashable ListGrantsResponse where
  hashWithSalt _salt ListGrantsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` grants
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` truncated

instance Prelude.NFData ListGrantsResponse where
  rnf ListGrantsResponse' {..} =
    Prelude.rnf grants `Prelude.seq`
      Prelude.rnf nextMarker `Prelude.seq`
        Prelude.rnf truncated
