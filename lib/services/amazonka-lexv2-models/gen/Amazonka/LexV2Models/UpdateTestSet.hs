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
-- Module      : Amazonka.LexV2Models.UpdateTestSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The action to update the test set.
module Amazonka.LexV2Models.UpdateTestSet
  ( -- * Creating a Request
    UpdateTestSet (..),
    newUpdateTestSet,

    -- * Request Lenses
    updateTestSet_description,
    updateTestSet_testSetId,
    updateTestSet_testSetName,

    -- * Destructuring the Response
    UpdateTestSetResponse (..),
    newUpdateTestSetResponse,

    -- * Response Lenses
    updateTestSetResponse_creationDateTime,
    updateTestSetResponse_description,
    updateTestSetResponse_lastUpdatedDateTime,
    updateTestSetResponse_modality,
    updateTestSetResponse_numTurns,
    updateTestSetResponse_roleArn,
    updateTestSetResponse_status,
    updateTestSetResponse_storageLocation,
    updateTestSetResponse_testSetId,
    updateTestSetResponse_testSetName,
    updateTestSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTestSet' smart constructor.
data UpdateTestSet = UpdateTestSet'
  { -- | The new test set description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The test set Id for which update test operation to be performed.
    testSetId :: Prelude.Text,
    -- | The new test set name.
    testSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTestSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateTestSet_description' - The new test set description.
--
-- 'testSetId', 'updateTestSet_testSetId' - The test set Id for which update test operation to be performed.
--
-- 'testSetName', 'updateTestSet_testSetName' - The new test set name.
newUpdateTestSet ::
  -- | 'testSetId'
  Prelude.Text ->
  -- | 'testSetName'
  Prelude.Text ->
  UpdateTestSet
newUpdateTestSet pTestSetId_ pTestSetName_ =
  UpdateTestSet'
    { description = Prelude.Nothing,
      testSetId = pTestSetId_,
      testSetName = pTestSetName_
    }

-- | The new test set description.
updateTestSet_description :: Lens.Lens' UpdateTestSet (Prelude.Maybe Prelude.Text)
updateTestSet_description = Lens.lens (\UpdateTestSet' {description} -> description) (\s@UpdateTestSet' {} a -> s {description = a} :: UpdateTestSet)

-- | The test set Id for which update test operation to be performed.
updateTestSet_testSetId :: Lens.Lens' UpdateTestSet Prelude.Text
updateTestSet_testSetId = Lens.lens (\UpdateTestSet' {testSetId} -> testSetId) (\s@UpdateTestSet' {} a -> s {testSetId = a} :: UpdateTestSet)

-- | The new test set name.
updateTestSet_testSetName :: Lens.Lens' UpdateTestSet Prelude.Text
updateTestSet_testSetName = Lens.lens (\UpdateTestSet' {testSetName} -> testSetName) (\s@UpdateTestSet' {} a -> s {testSetName = a} :: UpdateTestSet)

instance Core.AWSRequest UpdateTestSet where
  type
    AWSResponse UpdateTestSet =
      UpdateTestSetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTestSetResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "modality")
            Prelude.<*> (x Data..?> "numTurns")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "storageLocation")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (x Data..?> "testSetName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTestSet where
  hashWithSalt _salt UpdateTestSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` testSetId
      `Prelude.hashWithSalt` testSetName

instance Prelude.NFData UpdateTestSet where
  rnf UpdateTestSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName

instance Data.ToHeaders UpdateTestSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTestSet where
  toJSON UpdateTestSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("testSetName" Data..= testSetName)
          ]
      )

instance Data.ToPath UpdateTestSet where
  toPath UpdateTestSet' {..} =
    Prelude.mconcat ["/testsets/", Data.toBS testSetId]

instance Data.ToQuery UpdateTestSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTestSetResponse' smart constructor.
data UpdateTestSetResponse = UpdateTestSetResponse'
  { -- | The creation date and time for the updated test set.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The test set description for the updated test set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the last update for the updated test set.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether audio or text is used for the updated test set.
    modality :: Prelude.Maybe TestSetModality,
    -- | The number of conversation turns from the updated test set.
    numTurns :: Prelude.Maybe Prelude.Int,
    -- | The roleARN used for any operation in the test set to access resources
    -- in the Amazon Web Services account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The status for the updated test set.
    status :: Prelude.Maybe TestSetStatus,
    -- | The Amazon S3 storage location for the updated test set.
    storageLocation :: Prelude.Maybe TestSetStorageLocation,
    -- | The test set Id for which update test operation to be performed.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The test set name for the updated test set.
    testSetName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTestSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'updateTestSetResponse_creationDateTime' - The creation date and time for the updated test set.
--
-- 'description', 'updateTestSetResponse_description' - The test set description for the updated test set.
--
-- 'lastUpdatedDateTime', 'updateTestSetResponse_lastUpdatedDateTime' - The date and time of the last update for the updated test set.
--
-- 'modality', 'updateTestSetResponse_modality' - Indicates whether audio or text is used for the updated test set.
--
-- 'numTurns', 'updateTestSetResponse_numTurns' - The number of conversation turns from the updated test set.
--
-- 'roleArn', 'updateTestSetResponse_roleArn' - The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
--
-- 'status', 'updateTestSetResponse_status' - The status for the updated test set.
--
-- 'storageLocation', 'updateTestSetResponse_storageLocation' - The Amazon S3 storage location for the updated test set.
--
-- 'testSetId', 'updateTestSetResponse_testSetId' - The test set Id for which update test operation to be performed.
--
-- 'testSetName', 'updateTestSetResponse_testSetName' - The test set name for the updated test set.
--
-- 'httpStatus', 'updateTestSetResponse_httpStatus' - The response's http status code.
newUpdateTestSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTestSetResponse
newUpdateTestSetResponse pHttpStatus_ =
  UpdateTestSetResponse'
    { creationDateTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      modality = Prelude.Nothing,
      numTurns = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      testSetId = Prelude.Nothing,
      testSetName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation date and time for the updated test set.
updateTestSetResponse_creationDateTime :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.UTCTime)
updateTestSetResponse_creationDateTime = Lens.lens (\UpdateTestSetResponse' {creationDateTime} -> creationDateTime) (\s@UpdateTestSetResponse' {} a -> s {creationDateTime = a} :: UpdateTestSetResponse) Prelude.. Lens.mapping Data._Time

-- | The test set description for the updated test set.
updateTestSetResponse_description :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.Text)
updateTestSetResponse_description = Lens.lens (\UpdateTestSetResponse' {description} -> description) (\s@UpdateTestSetResponse' {} a -> s {description = a} :: UpdateTestSetResponse)

-- | The date and time of the last update for the updated test set.
updateTestSetResponse_lastUpdatedDateTime :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.UTCTime)
updateTestSetResponse_lastUpdatedDateTime = Lens.lens (\UpdateTestSetResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateTestSetResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateTestSetResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates whether audio or text is used for the updated test set.
updateTestSetResponse_modality :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe TestSetModality)
updateTestSetResponse_modality = Lens.lens (\UpdateTestSetResponse' {modality} -> modality) (\s@UpdateTestSetResponse' {} a -> s {modality = a} :: UpdateTestSetResponse)

-- | The number of conversation turns from the updated test set.
updateTestSetResponse_numTurns :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.Int)
updateTestSetResponse_numTurns = Lens.lens (\UpdateTestSetResponse' {numTurns} -> numTurns) (\s@UpdateTestSetResponse' {} a -> s {numTurns = a} :: UpdateTestSetResponse)

-- | The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
updateTestSetResponse_roleArn :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.Text)
updateTestSetResponse_roleArn = Lens.lens (\UpdateTestSetResponse' {roleArn} -> roleArn) (\s@UpdateTestSetResponse' {} a -> s {roleArn = a} :: UpdateTestSetResponse)

-- | The status for the updated test set.
updateTestSetResponse_status :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe TestSetStatus)
updateTestSetResponse_status = Lens.lens (\UpdateTestSetResponse' {status} -> status) (\s@UpdateTestSetResponse' {} a -> s {status = a} :: UpdateTestSetResponse)

-- | The Amazon S3 storage location for the updated test set.
updateTestSetResponse_storageLocation :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe TestSetStorageLocation)
updateTestSetResponse_storageLocation = Lens.lens (\UpdateTestSetResponse' {storageLocation} -> storageLocation) (\s@UpdateTestSetResponse' {} a -> s {storageLocation = a} :: UpdateTestSetResponse)

-- | The test set Id for which update test operation to be performed.
updateTestSetResponse_testSetId :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.Text)
updateTestSetResponse_testSetId = Lens.lens (\UpdateTestSetResponse' {testSetId} -> testSetId) (\s@UpdateTestSetResponse' {} a -> s {testSetId = a} :: UpdateTestSetResponse)

-- | The test set name for the updated test set.
updateTestSetResponse_testSetName :: Lens.Lens' UpdateTestSetResponse (Prelude.Maybe Prelude.Text)
updateTestSetResponse_testSetName = Lens.lens (\UpdateTestSetResponse' {testSetName} -> testSetName) (\s@UpdateTestSetResponse' {} a -> s {testSetName = a} :: UpdateTestSetResponse)

-- | The response's http status code.
updateTestSetResponse_httpStatus :: Lens.Lens' UpdateTestSetResponse Prelude.Int
updateTestSetResponse_httpStatus = Lens.lens (\UpdateTestSetResponse' {httpStatus} -> httpStatus) (\s@UpdateTestSetResponse' {} a -> s {httpStatus = a} :: UpdateTestSetResponse)

instance Prelude.NFData UpdateTestSetResponse where
  rnf UpdateTestSetResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf modality
      `Prelude.seq` Prelude.rnf numTurns
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf httpStatus
