{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PersonalizeEvents.PutItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more items to an Items dataset. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/importing-items.html Importing Items Incrementally>.
module Amazonka.PersonalizeEvents.PutItems
  ( -- * Creating a Request
    PutItems (..),
    newPutItems,

    -- * Request Lenses
    putItems_datasetArn,
    putItems_items,

    -- * Destructuring the Response
    PutItemsResponse (..),
    newPutItemsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PersonalizeEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutItems' smart constructor.
data PutItems = PutItems'
  { -- | The Amazon Resource Name (ARN) of the Items dataset you are adding the
    -- item or items to.
    datasetArn :: Prelude.Text,
    -- | A list of item data.
    items :: Prelude.NonEmpty Item
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'putItems_datasetArn' - The Amazon Resource Name (ARN) of the Items dataset you are adding the
-- item or items to.
--
-- 'items', 'putItems_items' - A list of item data.
newPutItems ::
  -- | 'datasetArn'
  Prelude.Text ->
  -- | 'items'
  Prelude.NonEmpty Item ->
  PutItems
newPutItems pDatasetArn_ pItems_ =
  PutItems'
    { datasetArn = pDatasetArn_,
      items = Lens.coerced Lens.# pItems_
    }

-- | The Amazon Resource Name (ARN) of the Items dataset you are adding the
-- item or items to.
putItems_datasetArn :: Lens.Lens' PutItems Prelude.Text
putItems_datasetArn = Lens.lens (\PutItems' {datasetArn} -> datasetArn) (\s@PutItems' {} a -> s {datasetArn = a} :: PutItems)

-- | A list of item data.
putItems_items :: Lens.Lens' PutItems (Prelude.NonEmpty Item)
putItems_items = Lens.lens (\PutItems' {items} -> items) (\s@PutItems' {} a -> s {items = a} :: PutItems) Prelude.. Lens.coerced

instance Core.AWSRequest PutItems where
  type AWSResponse PutItems = PutItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull PutItemsResponse'

instance Prelude.Hashable PutItems where
  hashWithSalt _salt PutItems' {..} =
    _salt `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` items

instance Prelude.NFData PutItems where
  rnf PutItems' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf items

instance Data.ToHeaders PutItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutItems where
  toJSON PutItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("datasetArn" Data..= datasetArn),
            Prelude.Just ("items" Data..= items)
          ]
      )

instance Data.ToPath PutItems where
  toPath = Prelude.const "/items"

instance Data.ToQuery PutItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutItemsResponse' smart constructor.
data PutItemsResponse = PutItemsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutItemsResponse ::
  PutItemsResponse
newPutItemsResponse = PutItemsResponse'

instance Prelude.NFData PutItemsResponse where
  rnf _ = ()
