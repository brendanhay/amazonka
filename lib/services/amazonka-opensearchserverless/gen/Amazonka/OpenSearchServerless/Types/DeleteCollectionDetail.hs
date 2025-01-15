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
-- Module      : Amazonka.OpenSearchServerless.Types.DeleteCollectionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.DeleteCollectionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about a deleted OpenSearch Serverless collection.
--
-- /See:/ 'newDeleteCollectionDetail' smart constructor.
data DeleteCollectionDetail = DeleteCollectionDetail'
  { -- | The unique identifier of the collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the collection.
    status :: Prelude.Maybe CollectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCollectionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteCollectionDetail_id' - The unique identifier of the collection.
--
-- 'name', 'deleteCollectionDetail_name' - The name of the collection.
--
-- 'status', 'deleteCollectionDetail_status' - The current status of the collection.
newDeleteCollectionDetail ::
  DeleteCollectionDetail
newDeleteCollectionDetail =
  DeleteCollectionDetail'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique identifier of the collection.
deleteCollectionDetail_id :: Lens.Lens' DeleteCollectionDetail (Prelude.Maybe Prelude.Text)
deleteCollectionDetail_id = Lens.lens (\DeleteCollectionDetail' {id} -> id) (\s@DeleteCollectionDetail' {} a -> s {id = a} :: DeleteCollectionDetail)

-- | The name of the collection.
deleteCollectionDetail_name :: Lens.Lens' DeleteCollectionDetail (Prelude.Maybe Prelude.Text)
deleteCollectionDetail_name = Lens.lens (\DeleteCollectionDetail' {name} -> name) (\s@DeleteCollectionDetail' {} a -> s {name = a} :: DeleteCollectionDetail)

-- | The current status of the collection.
deleteCollectionDetail_status :: Lens.Lens' DeleteCollectionDetail (Prelude.Maybe CollectionStatus)
deleteCollectionDetail_status = Lens.lens (\DeleteCollectionDetail' {status} -> status) (\s@DeleteCollectionDetail' {} a -> s {status = a} :: DeleteCollectionDetail)

instance Data.FromJSON DeleteCollectionDetail where
  parseJSON =
    Data.withObject
      "DeleteCollectionDetail"
      ( \x ->
          DeleteCollectionDetail'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DeleteCollectionDetail where
  hashWithSalt _salt DeleteCollectionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DeleteCollectionDetail where
  rnf DeleteCollectionDetail' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf status
