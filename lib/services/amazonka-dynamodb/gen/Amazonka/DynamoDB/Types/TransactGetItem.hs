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
-- Module      : Amazonka.DynamoDB.Types.TransactGetItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TransactGetItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.Get
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Specifies an item to be retrieved as part of the transaction.
--
-- /See:/ 'newTransactGetItem' smart constructor.
data TransactGetItem = TransactGetItem'
  { -- | Contains the primary key that identifies the item to get, together with
    -- the name of the table that contains the item, and optionally the
    -- specific attributes of the item to retrieve.
    get' :: Get
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransactGetItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'get'', 'transactGetItem_get' - Contains the primary key that identifies the item to get, together with
-- the name of the table that contains the item, and optionally the
-- specific attributes of the item to retrieve.
newTransactGetItem ::
  -- | 'get''
  Get ->
  TransactGetItem
newTransactGetItem pGet_ =
  TransactGetItem' {get' = pGet_}

-- | Contains the primary key that identifies the item to get, together with
-- the name of the table that contains the item, and optionally the
-- specific attributes of the item to retrieve.
transactGetItem_get :: Lens.Lens' TransactGetItem Get
transactGetItem_get = Lens.lens (\TransactGetItem' {get'} -> get') (\s@TransactGetItem' {} a -> s {get' = a} :: TransactGetItem)

instance Prelude.Hashable TransactGetItem where
  hashWithSalt _salt TransactGetItem' {..} =
    _salt `Prelude.hashWithSalt` get'

instance Prelude.NFData TransactGetItem where
  rnf TransactGetItem' {..} = Prelude.rnf get'

instance Data.ToJSON TransactGetItem where
  toJSON TransactGetItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Get" Data..= get')]
      )
