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
-- Module      : Amazonka.LexV2Models.DescribeTestSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about the test set.
module Amazonka.LexV2Models.DescribeTestSet
  ( -- * Creating a Request
    DescribeTestSet (..),
    newDescribeTestSet,

    -- * Request Lenses
    describeTestSet_testSetId,

    -- * Destructuring the Response
    DescribeTestSetResponse (..),
    newDescribeTestSetResponse,

    -- * Response Lenses
    describeTestSetResponse_creationDateTime,
    describeTestSetResponse_description,
    describeTestSetResponse_lastUpdatedDateTime,
    describeTestSetResponse_modality,
    describeTestSetResponse_numTurns,
    describeTestSetResponse_roleArn,
    describeTestSetResponse_status,
    describeTestSetResponse_storageLocation,
    describeTestSetResponse_testSetId,
    describeTestSetResponse_testSetName,
    describeTestSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTestSet' smart constructor.
data DescribeTestSet = DescribeTestSet'
  { -- | The test set Id for the test set request.
    testSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetId', 'describeTestSet_testSetId' - The test set Id for the test set request.
newDescribeTestSet ::
  -- | 'testSetId'
  Prelude.Text ->
  DescribeTestSet
newDescribeTestSet pTestSetId_ =
  DescribeTestSet' {testSetId = pTestSetId_}

-- | The test set Id for the test set request.
describeTestSet_testSetId :: Lens.Lens' DescribeTestSet Prelude.Text
describeTestSet_testSetId = Lens.lens (\DescribeTestSet' {testSetId} -> testSetId) (\s@DescribeTestSet' {} a -> s {testSetId = a} :: DescribeTestSet)

instance Core.AWSRequest DescribeTestSet where
  type
    AWSResponse DescribeTestSet =
      DescribeTestSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestSetResponse'
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

instance Prelude.Hashable DescribeTestSet where
  hashWithSalt _salt DescribeTestSet' {..} =
    _salt `Prelude.hashWithSalt` testSetId

instance Prelude.NFData DescribeTestSet where
  rnf DescribeTestSet' {..} = Prelude.rnf testSetId

instance Data.ToHeaders DescribeTestSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTestSet where
  toPath DescribeTestSet' {..} =
    Prelude.mconcat ["/testsets/", Data.toBS testSetId]

instance Data.ToQuery DescribeTestSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTestSetResponse' smart constructor.
data DescribeTestSetResponse = DescribeTestSetResponse'
  { -- | The creation date and time for the test set data.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the test set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time for the last update of the test set data.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether the test set is audio or text data.
    modality :: Prelude.Maybe TestSetModality,
    -- | The total number of agent and user turn in the test set.
    numTurns :: Prelude.Maybe Prelude.Int,
    -- | The roleARN used for any operation in the test set to access resources
    -- in the Amazon Web Services account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the test set.
    status :: Prelude.Maybe TestSetStatus,
    -- | The Amazon S3 storage location for the test set data.
    storageLocation :: Prelude.Maybe TestSetStorageLocation,
    -- | The test set Id for the test set response.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The test set name of the test set.
    testSetName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'describeTestSetResponse_creationDateTime' - The creation date and time for the test set data.
--
-- 'description', 'describeTestSetResponse_description' - The description of the test set.
--
-- 'lastUpdatedDateTime', 'describeTestSetResponse_lastUpdatedDateTime' - The date and time for the last update of the test set data.
--
-- 'modality', 'describeTestSetResponse_modality' - Indicates whether the test set is audio or text data.
--
-- 'numTurns', 'describeTestSetResponse_numTurns' - The total number of agent and user turn in the test set.
--
-- 'roleArn', 'describeTestSetResponse_roleArn' - The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
--
-- 'status', 'describeTestSetResponse_status' - The status of the test set.
--
-- 'storageLocation', 'describeTestSetResponse_storageLocation' - The Amazon S3 storage location for the test set data.
--
-- 'testSetId', 'describeTestSetResponse_testSetId' - The test set Id for the test set response.
--
-- 'testSetName', 'describeTestSetResponse_testSetName' - The test set name of the test set.
--
-- 'httpStatus', 'describeTestSetResponse_httpStatus' - The response's http status code.
newDescribeTestSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTestSetResponse
newDescribeTestSetResponse pHttpStatus_ =
  DescribeTestSetResponse'
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

-- | The creation date and time for the test set data.
describeTestSetResponse_creationDateTime :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetResponse_creationDateTime = Lens.lens (\DescribeTestSetResponse' {creationDateTime} -> creationDateTime) (\s@DescribeTestSetResponse' {} a -> s {creationDateTime = a} :: DescribeTestSetResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the test set.
describeTestSetResponse_description :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.Text)
describeTestSetResponse_description = Lens.lens (\DescribeTestSetResponse' {description} -> description) (\s@DescribeTestSetResponse' {} a -> s {description = a} :: DescribeTestSetResponse)

-- | The date and time for the last update of the test set data.
describeTestSetResponse_lastUpdatedDateTime :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetResponse_lastUpdatedDateTime = Lens.lens (\DescribeTestSetResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeTestSetResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeTestSetResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the test set is audio or text data.
describeTestSetResponse_modality :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe TestSetModality)
describeTestSetResponse_modality = Lens.lens (\DescribeTestSetResponse' {modality} -> modality) (\s@DescribeTestSetResponse' {} a -> s {modality = a} :: DescribeTestSetResponse)

-- | The total number of agent and user turn in the test set.
describeTestSetResponse_numTurns :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.Int)
describeTestSetResponse_numTurns = Lens.lens (\DescribeTestSetResponse' {numTurns} -> numTurns) (\s@DescribeTestSetResponse' {} a -> s {numTurns = a} :: DescribeTestSetResponse)

-- | The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
describeTestSetResponse_roleArn :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.Text)
describeTestSetResponse_roleArn = Lens.lens (\DescribeTestSetResponse' {roleArn} -> roleArn) (\s@DescribeTestSetResponse' {} a -> s {roleArn = a} :: DescribeTestSetResponse)

-- | The status of the test set.
describeTestSetResponse_status :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe TestSetStatus)
describeTestSetResponse_status = Lens.lens (\DescribeTestSetResponse' {status} -> status) (\s@DescribeTestSetResponse' {} a -> s {status = a} :: DescribeTestSetResponse)

-- | The Amazon S3 storage location for the test set data.
describeTestSetResponse_storageLocation :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe TestSetStorageLocation)
describeTestSetResponse_storageLocation = Lens.lens (\DescribeTestSetResponse' {storageLocation} -> storageLocation) (\s@DescribeTestSetResponse' {} a -> s {storageLocation = a} :: DescribeTestSetResponse)

-- | The test set Id for the test set response.
describeTestSetResponse_testSetId :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.Text)
describeTestSetResponse_testSetId = Lens.lens (\DescribeTestSetResponse' {testSetId} -> testSetId) (\s@DescribeTestSetResponse' {} a -> s {testSetId = a} :: DescribeTestSetResponse)

-- | The test set name of the test set.
describeTestSetResponse_testSetName :: Lens.Lens' DescribeTestSetResponse (Prelude.Maybe Prelude.Text)
describeTestSetResponse_testSetName = Lens.lens (\DescribeTestSetResponse' {testSetName} -> testSetName) (\s@DescribeTestSetResponse' {} a -> s {testSetName = a} :: DescribeTestSetResponse)

-- | The response's http status code.
describeTestSetResponse_httpStatus :: Lens.Lens' DescribeTestSetResponse Prelude.Int
describeTestSetResponse_httpStatus = Lens.lens (\DescribeTestSetResponse' {httpStatus} -> httpStatus) (\s@DescribeTestSetResponse' {} a -> s {httpStatus = a} :: DescribeTestSetResponse)

instance Prelude.NFData DescribeTestSetResponse where
  rnf DescribeTestSetResponse' {..} =
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
