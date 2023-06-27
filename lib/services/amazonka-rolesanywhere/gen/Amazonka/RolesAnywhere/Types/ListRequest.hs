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
-- Module      : Amazonka.RolesAnywhere.Types.ListRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ListRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newListRequest' smart constructor.
data ListRequest = ListRequest'
  { -- | A token that indicates where the output should continue from, if a
    -- previous request did not show all results. To get the next results, make
    -- the request again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of resources in the paginated list.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRequest_nextToken' - A token that indicates where the output should continue from, if a
-- previous request did not show all results. To get the next results, make
-- the request again with this value.
--
-- 'pageSize', 'listRequest_pageSize' - The number of resources in the paginated list.
newListRequest ::
  ListRequest
newListRequest =
  ListRequest'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token that indicates where the output should continue from, if a
-- previous request did not show all results. To get the next results, make
-- the request again with this value.
listRequest_nextToken :: Lens.Lens' ListRequest (Prelude.Maybe Prelude.Text)
listRequest_nextToken = Lens.lens (\ListRequest' {nextToken} -> nextToken) (\s@ListRequest' {} a -> s {nextToken = a} :: ListRequest)

-- | The number of resources in the paginated list.
listRequest_pageSize :: Lens.Lens' ListRequest (Prelude.Maybe Prelude.Int)
listRequest_pageSize = Lens.lens (\ListRequest' {pageSize} -> pageSize) (\s@ListRequest' {} a -> s {pageSize = a} :: ListRequest)

instance Prelude.Hashable ListRequest where
  hashWithSalt _salt ListRequest' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListRequest where
  rnf ListRequest' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToJSON ListRequest where
  toJSON ListRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("pageSize" Data..=) Prelude.<$> pageSize
          ]
      )
