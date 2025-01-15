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
-- Module      : Amazonka.IotTwinMaker.BatchPutPropertyValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets values for multiple time series properties.
module Amazonka.IotTwinMaker.BatchPutPropertyValues
  ( -- * Creating a Request
    BatchPutPropertyValues (..),
    newBatchPutPropertyValues,

    -- * Request Lenses
    batchPutPropertyValues_workspaceId,
    batchPutPropertyValues_entries,

    -- * Destructuring the Response
    BatchPutPropertyValuesResponse (..),
    newBatchPutPropertyValuesResponse,

    -- * Response Lenses
    batchPutPropertyValuesResponse_httpStatus,
    batchPutPropertyValuesResponse_errorEntries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutPropertyValues' smart constructor.
data BatchPutPropertyValues = BatchPutPropertyValues'
  { -- | The ID of the workspace that contains the properties to set.
    workspaceId :: Prelude.Text,
    -- | An object that maps strings to the property value entries to set. Each
    -- string in the mapping must be unique to this object.
    entries :: Prelude.NonEmpty PropertyValueEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutPropertyValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'batchPutPropertyValues_workspaceId' - The ID of the workspace that contains the properties to set.
--
-- 'entries', 'batchPutPropertyValues_entries' - An object that maps strings to the property value entries to set. Each
-- string in the mapping must be unique to this object.
newBatchPutPropertyValues ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'entries'
  Prelude.NonEmpty PropertyValueEntry ->
  BatchPutPropertyValues
newBatchPutPropertyValues pWorkspaceId_ pEntries_ =
  BatchPutPropertyValues'
    { workspaceId =
        pWorkspaceId_,
      entries = Lens.coerced Lens.# pEntries_
    }

-- | The ID of the workspace that contains the properties to set.
batchPutPropertyValues_workspaceId :: Lens.Lens' BatchPutPropertyValues Prelude.Text
batchPutPropertyValues_workspaceId = Lens.lens (\BatchPutPropertyValues' {workspaceId} -> workspaceId) (\s@BatchPutPropertyValues' {} a -> s {workspaceId = a} :: BatchPutPropertyValues)

-- | An object that maps strings to the property value entries to set. Each
-- string in the mapping must be unique to this object.
batchPutPropertyValues_entries :: Lens.Lens' BatchPutPropertyValues (Prelude.NonEmpty PropertyValueEntry)
batchPutPropertyValues_entries = Lens.lens (\BatchPutPropertyValues' {entries} -> entries) (\s@BatchPutPropertyValues' {} a -> s {entries = a} :: BatchPutPropertyValues) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutPropertyValues where
  type
    AWSResponse BatchPutPropertyValues =
      BatchPutPropertyValuesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutPropertyValuesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "errorEntries")
      )

instance Prelude.Hashable BatchPutPropertyValues where
  hashWithSalt _salt BatchPutPropertyValues' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchPutPropertyValues where
  rnf BatchPutPropertyValues' {..} =
    Prelude.rnf workspaceId `Prelude.seq`
      Prelude.rnf entries

instance Data.ToHeaders BatchPutPropertyValues where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchPutPropertyValues where
  toJSON BatchPutPropertyValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("entries" Data..= entries)]
      )

instance Data.ToPath BatchPutPropertyValues where
  toPath BatchPutPropertyValues' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entity-properties"
      ]

instance Data.ToQuery BatchPutPropertyValues where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutPropertyValuesResponse' smart constructor.
data BatchPutPropertyValuesResponse = BatchPutPropertyValuesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Entries that caused errors in the batch put operation.
    errorEntries :: Prelude.NonEmpty BatchPutPropertyErrorEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutPropertyValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchPutPropertyValuesResponse_httpStatus' - The response's http status code.
--
-- 'errorEntries', 'batchPutPropertyValuesResponse_errorEntries' - Entries that caused errors in the batch put operation.
newBatchPutPropertyValuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'errorEntries'
  Prelude.NonEmpty BatchPutPropertyErrorEntry ->
  BatchPutPropertyValuesResponse
newBatchPutPropertyValuesResponse
  pHttpStatus_
  pErrorEntries_ =
    BatchPutPropertyValuesResponse'
      { httpStatus =
          pHttpStatus_,
        errorEntries =
          Lens.coerced Lens.# pErrorEntries_
      }

-- | The response's http status code.
batchPutPropertyValuesResponse_httpStatus :: Lens.Lens' BatchPutPropertyValuesResponse Prelude.Int
batchPutPropertyValuesResponse_httpStatus = Lens.lens (\BatchPutPropertyValuesResponse' {httpStatus} -> httpStatus) (\s@BatchPutPropertyValuesResponse' {} a -> s {httpStatus = a} :: BatchPutPropertyValuesResponse)

-- | Entries that caused errors in the batch put operation.
batchPutPropertyValuesResponse_errorEntries :: Lens.Lens' BatchPutPropertyValuesResponse (Prelude.NonEmpty BatchPutPropertyErrorEntry)
batchPutPropertyValuesResponse_errorEntries = Lens.lens (\BatchPutPropertyValuesResponse' {errorEntries} -> errorEntries) (\s@BatchPutPropertyValuesResponse' {} a -> s {errorEntries = a} :: BatchPutPropertyValuesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchPutPropertyValuesResponse
  where
  rnf BatchPutPropertyValuesResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf errorEntries
