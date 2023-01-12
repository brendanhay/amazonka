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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.DeleteRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Record@ from a @FeatureGroup@. When the @DeleteRecord@ API is
-- called a new record will be added to the @OfflineStore@ and the @Record@
-- will be removed from the @OnlineStore@. This record will have a value of
-- @True@ in the @is_deleted@ column.
module Amazonka.SageMakerFeatureStoreRuntime.DeleteRecord
  ( -- * Creating a Request
    DeleteRecord (..),
    newDeleteRecord,

    -- * Request Lenses
    deleteRecord_targetStores,
    deleteRecord_featureGroupName,
    deleteRecord_recordIdentifierValueAsString,
    deleteRecord_eventTime,

    -- * Destructuring the Response
    DeleteRecordResponse (..),
    newDeleteRecordResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerFeatureStoreRuntime.Types

-- | /See:/ 'newDeleteRecord' smart constructor.
data DeleteRecord = DeleteRecord'
  { -- | A list of stores from which you\'re deleting the record. By default,
    -- Feature Store deletes the record from all of the stores that you\'re
    -- using for the @FeatureGroup@.
    targetStores :: Prelude.Maybe (Prelude.NonEmpty TargetStore),
    -- | The name of the feature group to delete the record from.
    featureGroupName :: Prelude.Text,
    -- | The value for the @RecordIdentifier@ that uniquely identifies the
    -- record, in string format.
    recordIdentifierValueAsString :: Prelude.Text,
    -- | Timestamp indicating when the deletion event occurred. @EventTime@ can
    -- be used to query data at a certain point in time.
    eventTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetStores', 'deleteRecord_targetStores' - A list of stores from which you\'re deleting the record. By default,
-- Feature Store deletes the record from all of the stores that you\'re
-- using for the @FeatureGroup@.
--
-- 'featureGroupName', 'deleteRecord_featureGroupName' - The name of the feature group to delete the record from.
--
-- 'recordIdentifierValueAsString', 'deleteRecord_recordIdentifierValueAsString' - The value for the @RecordIdentifier@ that uniquely identifies the
-- record, in string format.
--
-- 'eventTime', 'deleteRecord_eventTime' - Timestamp indicating when the deletion event occurred. @EventTime@ can
-- be used to query data at a certain point in time.
newDeleteRecord ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'recordIdentifierValueAsString'
  Prelude.Text ->
  -- | 'eventTime'
  Prelude.Text ->
  DeleteRecord
newDeleteRecord
  pFeatureGroupName_
  pRecordIdentifierValueAsString_
  pEventTime_ =
    DeleteRecord'
      { targetStores = Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        recordIdentifierValueAsString =
          pRecordIdentifierValueAsString_,
        eventTime = pEventTime_
      }

-- | A list of stores from which you\'re deleting the record. By default,
-- Feature Store deletes the record from all of the stores that you\'re
-- using for the @FeatureGroup@.
deleteRecord_targetStores :: Lens.Lens' DeleteRecord (Prelude.Maybe (Prelude.NonEmpty TargetStore))
deleteRecord_targetStores = Lens.lens (\DeleteRecord' {targetStores} -> targetStores) (\s@DeleteRecord' {} a -> s {targetStores = a} :: DeleteRecord) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature group to delete the record from.
deleteRecord_featureGroupName :: Lens.Lens' DeleteRecord Prelude.Text
deleteRecord_featureGroupName = Lens.lens (\DeleteRecord' {featureGroupName} -> featureGroupName) (\s@DeleteRecord' {} a -> s {featureGroupName = a} :: DeleteRecord)

-- | The value for the @RecordIdentifier@ that uniquely identifies the
-- record, in string format.
deleteRecord_recordIdentifierValueAsString :: Lens.Lens' DeleteRecord Prelude.Text
deleteRecord_recordIdentifierValueAsString = Lens.lens (\DeleteRecord' {recordIdentifierValueAsString} -> recordIdentifierValueAsString) (\s@DeleteRecord' {} a -> s {recordIdentifierValueAsString = a} :: DeleteRecord)

-- | Timestamp indicating when the deletion event occurred. @EventTime@ can
-- be used to query data at a certain point in time.
deleteRecord_eventTime :: Lens.Lens' DeleteRecord Prelude.Text
deleteRecord_eventTime = Lens.lens (\DeleteRecord' {eventTime} -> eventTime) (\s@DeleteRecord' {} a -> s {eventTime = a} :: DeleteRecord)

instance Core.AWSRequest DeleteRecord where
  type AWSResponse DeleteRecord = DeleteRecordResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteRecordResponse'

instance Prelude.Hashable DeleteRecord where
  hashWithSalt _salt DeleteRecord' {..} =
    _salt `Prelude.hashWithSalt` targetStores
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` recordIdentifierValueAsString
      `Prelude.hashWithSalt` eventTime

instance Prelude.NFData DeleteRecord where
  rnf DeleteRecord' {..} =
    Prelude.rnf targetStores
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf recordIdentifierValueAsString
      `Prelude.seq` Prelude.rnf eventTime

instance Data.ToHeaders DeleteRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRecord where
  toPath DeleteRecord' {..} =
    Prelude.mconcat
      ["/FeatureGroup/", Data.toBS featureGroupName]

instance Data.ToQuery DeleteRecord where
  toQuery DeleteRecord' {..} =
    Prelude.mconcat
      [ "TargetStores"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> targetStores),
        "RecordIdentifierValueAsString"
          Data.=: recordIdentifierValueAsString,
        "EventTime" Data.=: eventTime
      ]

-- | /See:/ 'newDeleteRecordResponse' smart constructor.
data DeleteRecordResponse = DeleteRecordResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRecordResponse ::
  DeleteRecordResponse
newDeleteRecordResponse = DeleteRecordResponse'

instance Prelude.NFData DeleteRecordResponse where
  rnf _ = ()
