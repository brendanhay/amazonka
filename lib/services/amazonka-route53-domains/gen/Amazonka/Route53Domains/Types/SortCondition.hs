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
-- Module      : Amazonka.Route53Domains.Types.SortCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.SortCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.ListDomainsAttributeName
import Amazonka.Route53Domains.Types.SortOrder

-- | Information for sorting a list of domains.
--
-- /See:/ 'newSortCondition' smart constructor.
data SortCondition = SortCondition'
  { -- | Field to be used for sorting the list of domains. It can be either the
    -- name or the expiration for a domain. Note that if @filterCondition@ is
    -- used in the same
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains__ListDomains.html ListDomains>
    -- call, the field used for sorting has to be the same as the field used
    -- for filtering.
    name :: ListDomainsAttributeName,
    -- | The sort order for a list of domains. Either ascending (ASC) or
    -- descending (DES).
    sortOrder :: SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sortCondition_name' - Field to be used for sorting the list of domains. It can be either the
-- name or the expiration for a domain. Note that if @filterCondition@ is
-- used in the same
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains__ListDomains.html ListDomains>
-- call, the field used for sorting has to be the same as the field used
-- for filtering.
--
-- 'sortOrder', 'sortCondition_sortOrder' - The sort order for a list of domains. Either ascending (ASC) or
-- descending (DES).
newSortCondition ::
  -- | 'name'
  ListDomainsAttributeName ->
  -- | 'sortOrder'
  SortOrder ->
  SortCondition
newSortCondition pName_ pSortOrder_ =
  SortCondition'
    { name = pName_,
      sortOrder = pSortOrder_
    }

-- | Field to be used for sorting the list of domains. It can be either the
-- name or the expiration for a domain. Note that if @filterCondition@ is
-- used in the same
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains__ListDomains.html ListDomains>
-- call, the field used for sorting has to be the same as the field used
-- for filtering.
sortCondition_name :: Lens.Lens' SortCondition ListDomainsAttributeName
sortCondition_name = Lens.lens (\SortCondition' {name} -> name) (\s@SortCondition' {} a -> s {name = a} :: SortCondition)

-- | The sort order for a list of domains. Either ascending (ASC) or
-- descending (DES).
sortCondition_sortOrder :: Lens.Lens' SortCondition SortOrder
sortCondition_sortOrder = Lens.lens (\SortCondition' {sortOrder} -> sortOrder) (\s@SortCondition' {} a -> s {sortOrder = a} :: SortCondition)

instance Prelude.Hashable SortCondition where
  hashWithSalt _salt SortCondition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData SortCondition where
  rnf SortCondition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON SortCondition where
  toJSON SortCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SortOrder" Data..= sortOrder)
          ]
      )
