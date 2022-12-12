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
-- Module      : Amazonka.OpenSearchServerless.Types.CollectionErrorDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CollectionErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error information for an OpenSearch Serverless request.
--
-- /See:/ 'newCollectionErrorDetail' smart constructor.
data CollectionErrorDetail = CollectionErrorDetail'
  { -- | The error code for the request. For example, @NOT_FOUND@.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | A description of the error. For example,
    -- @The specified Collection is not found.@
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | If the request contains collection IDs, the response includes the IDs
    -- provided in the request.
    id :: Prelude.Maybe Prelude.Text,
    -- | If the request contains collection names, the response includes the
    -- names provided in the request.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'collectionErrorDetail_errorCode' - The error code for the request. For example, @NOT_FOUND@.
--
-- 'errorMessage', 'collectionErrorDetail_errorMessage' - A description of the error. For example,
-- @The specified Collection is not found.@
--
-- 'id', 'collectionErrorDetail_id' - If the request contains collection IDs, the response includes the IDs
-- provided in the request.
--
-- 'name', 'collectionErrorDetail_name' - If the request contains collection names, the response includes the
-- names provided in the request.
newCollectionErrorDetail ::
  CollectionErrorDetail
newCollectionErrorDetail =
  CollectionErrorDetail'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The error code for the request. For example, @NOT_FOUND@.
collectionErrorDetail_errorCode :: Lens.Lens' CollectionErrorDetail (Prelude.Maybe Prelude.Text)
collectionErrorDetail_errorCode = Lens.lens (\CollectionErrorDetail' {errorCode} -> errorCode) (\s@CollectionErrorDetail' {} a -> s {errorCode = a} :: CollectionErrorDetail)

-- | A description of the error. For example,
-- @The specified Collection is not found.@
collectionErrorDetail_errorMessage :: Lens.Lens' CollectionErrorDetail (Prelude.Maybe Prelude.Text)
collectionErrorDetail_errorMessage = Lens.lens (\CollectionErrorDetail' {errorMessage} -> errorMessage) (\s@CollectionErrorDetail' {} a -> s {errorMessage = a} :: CollectionErrorDetail)

-- | If the request contains collection IDs, the response includes the IDs
-- provided in the request.
collectionErrorDetail_id :: Lens.Lens' CollectionErrorDetail (Prelude.Maybe Prelude.Text)
collectionErrorDetail_id = Lens.lens (\CollectionErrorDetail' {id} -> id) (\s@CollectionErrorDetail' {} a -> s {id = a} :: CollectionErrorDetail)

-- | If the request contains collection names, the response includes the
-- names provided in the request.
collectionErrorDetail_name :: Lens.Lens' CollectionErrorDetail (Prelude.Maybe Prelude.Text)
collectionErrorDetail_name = Lens.lens (\CollectionErrorDetail' {name} -> name) (\s@CollectionErrorDetail' {} a -> s {name = a} :: CollectionErrorDetail)

instance Data.FromJSON CollectionErrorDetail where
  parseJSON =
    Data.withObject
      "CollectionErrorDetail"
      ( \x ->
          CollectionErrorDetail'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable CollectionErrorDetail where
  hashWithSalt _salt CollectionErrorDetail' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData CollectionErrorDetail where
  rnf CollectionErrorDetail' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
