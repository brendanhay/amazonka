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
-- Module      : Amazonka.LexV2Models.DescribeTestSetGeneration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets metadata information about the test set generation.
module Amazonka.LexV2Models.DescribeTestSetGeneration
  ( -- * Creating a Request
    DescribeTestSetGeneration (..),
    newDescribeTestSetGeneration,

    -- * Request Lenses
    describeTestSetGeneration_testSetGenerationId,

    -- * Destructuring the Response
    DescribeTestSetGenerationResponse (..),
    newDescribeTestSetGenerationResponse,

    -- * Response Lenses
    describeTestSetGenerationResponse_creationDateTime,
    describeTestSetGenerationResponse_description,
    describeTestSetGenerationResponse_failureReasons,
    describeTestSetGenerationResponse_generationDataSource,
    describeTestSetGenerationResponse_lastUpdatedDateTime,
    describeTestSetGenerationResponse_roleArn,
    describeTestSetGenerationResponse_storageLocation,
    describeTestSetGenerationResponse_testSetGenerationId,
    describeTestSetGenerationResponse_testSetGenerationStatus,
    describeTestSetGenerationResponse_testSetId,
    describeTestSetGenerationResponse_testSetName,
    describeTestSetGenerationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTestSetGeneration' smart constructor.
data DescribeTestSetGeneration = DescribeTestSetGeneration'
  { -- | The unique identifier of the test set generation.
    testSetGenerationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSetGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetGenerationId', 'describeTestSetGeneration_testSetGenerationId' - The unique identifier of the test set generation.
newDescribeTestSetGeneration ::
  -- | 'testSetGenerationId'
  Prelude.Text ->
  DescribeTestSetGeneration
newDescribeTestSetGeneration pTestSetGenerationId_ =
  DescribeTestSetGeneration'
    { testSetGenerationId =
        pTestSetGenerationId_
    }

-- | The unique identifier of the test set generation.
describeTestSetGeneration_testSetGenerationId :: Lens.Lens' DescribeTestSetGeneration Prelude.Text
describeTestSetGeneration_testSetGenerationId = Lens.lens (\DescribeTestSetGeneration' {testSetGenerationId} -> testSetGenerationId) (\s@DescribeTestSetGeneration' {} a -> s {testSetGenerationId = a} :: DescribeTestSetGeneration)

instance Core.AWSRequest DescribeTestSetGeneration where
  type
    AWSResponse DescribeTestSetGeneration =
      DescribeTestSetGenerationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTestSetGenerationResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "generationDataSource")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "storageLocation")
            Prelude.<*> (x Data..?> "testSetGenerationId")
            Prelude.<*> (x Data..?> "testSetGenerationStatus")
            Prelude.<*> (x Data..?> "testSetId")
            Prelude.<*> (x Data..?> "testSetName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTestSetGeneration where
  hashWithSalt _salt DescribeTestSetGeneration' {..} =
    _salt `Prelude.hashWithSalt` testSetGenerationId

instance Prelude.NFData DescribeTestSetGeneration where
  rnf DescribeTestSetGeneration' {..} =
    Prelude.rnf testSetGenerationId

instance Data.ToHeaders DescribeTestSetGeneration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTestSetGeneration where
  toPath DescribeTestSetGeneration' {..} =
    Prelude.mconcat
      [ "/testsetgenerations/",
        Data.toBS testSetGenerationId
      ]

instance Data.ToQuery DescribeTestSetGeneration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTestSetGenerationResponse' smart constructor.
data DescribeTestSetGenerationResponse = DescribeTestSetGenerationResponse'
  { -- | The creation date and time for the test set generation.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The test set description for the test set generation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reasons the test set generation failed.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The data source of the test set used for the test set generation.
    generationDataSource :: Prelude.Maybe TestSetGenerationDataSource,
    -- | The date and time of the last update for the test set generation.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The roleARN of the test set used for the test set generation.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 storage location for the test set generation.
    storageLocation :: Prelude.Maybe TestSetStorageLocation,
    -- | The unique identifier of the test set generation.
    testSetGenerationId :: Prelude.Maybe Prelude.Text,
    -- | The status for the test set generation.
    testSetGenerationStatus :: Prelude.Maybe TestSetGenerationStatus,
    -- | The unique identifier for the test set created for the generated test
    -- set.
    testSetId :: Prelude.Maybe Prelude.Text,
    -- | The test set name for the generated test set.
    testSetName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTestSetGenerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'describeTestSetGenerationResponse_creationDateTime' - The creation date and time for the test set generation.
--
-- 'description', 'describeTestSetGenerationResponse_description' - The test set description for the test set generation.
--
-- 'failureReasons', 'describeTestSetGenerationResponse_failureReasons' - The reasons the test set generation failed.
--
-- 'generationDataSource', 'describeTestSetGenerationResponse_generationDataSource' - The data source of the test set used for the test set generation.
--
-- 'lastUpdatedDateTime', 'describeTestSetGenerationResponse_lastUpdatedDateTime' - The date and time of the last update for the test set generation.
--
-- 'roleArn', 'describeTestSetGenerationResponse_roleArn' - The roleARN of the test set used for the test set generation.
--
-- 'storageLocation', 'describeTestSetGenerationResponse_storageLocation' - The Amazon S3 storage location for the test set generation.
--
-- 'testSetGenerationId', 'describeTestSetGenerationResponse_testSetGenerationId' - The unique identifier of the test set generation.
--
-- 'testSetGenerationStatus', 'describeTestSetGenerationResponse_testSetGenerationStatus' - The status for the test set generation.
--
-- 'testSetId', 'describeTestSetGenerationResponse_testSetId' - The unique identifier for the test set created for the generated test
-- set.
--
-- 'testSetName', 'describeTestSetGenerationResponse_testSetName' - The test set name for the generated test set.
--
-- 'httpStatus', 'describeTestSetGenerationResponse_httpStatus' - The response's http status code.
newDescribeTestSetGenerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTestSetGenerationResponse
newDescribeTestSetGenerationResponse pHttpStatus_ =
  DescribeTestSetGenerationResponse'
    { creationDateTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      generationDataSource = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      testSetGenerationId = Prelude.Nothing,
      testSetGenerationStatus =
        Prelude.Nothing,
      testSetId = Prelude.Nothing,
      testSetName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation date and time for the test set generation.
describeTestSetGenerationResponse_creationDateTime :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetGenerationResponse_creationDateTime = Lens.lens (\DescribeTestSetGenerationResponse' {creationDateTime} -> creationDateTime) (\s@DescribeTestSetGenerationResponse' {} a -> s {creationDateTime = a} :: DescribeTestSetGenerationResponse) Prelude.. Lens.mapping Data._Time

-- | The test set description for the test set generation.
describeTestSetGenerationResponse_description :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
describeTestSetGenerationResponse_description = Lens.lens (\DescribeTestSetGenerationResponse' {description} -> description) (\s@DescribeTestSetGenerationResponse' {} a -> s {description = a} :: DescribeTestSetGenerationResponse)

-- | The reasons the test set generation failed.
describeTestSetGenerationResponse_failureReasons :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe [Prelude.Text])
describeTestSetGenerationResponse_failureReasons = Lens.lens (\DescribeTestSetGenerationResponse' {failureReasons} -> failureReasons) (\s@DescribeTestSetGenerationResponse' {} a -> s {failureReasons = a} :: DescribeTestSetGenerationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The data source of the test set used for the test set generation.
describeTestSetGenerationResponse_generationDataSource :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe TestSetGenerationDataSource)
describeTestSetGenerationResponse_generationDataSource = Lens.lens (\DescribeTestSetGenerationResponse' {generationDataSource} -> generationDataSource) (\s@DescribeTestSetGenerationResponse' {} a -> s {generationDataSource = a} :: DescribeTestSetGenerationResponse)

-- | The date and time of the last update for the test set generation.
describeTestSetGenerationResponse_lastUpdatedDateTime :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.UTCTime)
describeTestSetGenerationResponse_lastUpdatedDateTime = Lens.lens (\DescribeTestSetGenerationResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeTestSetGenerationResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeTestSetGenerationResponse) Prelude.. Lens.mapping Data._Time

-- | The roleARN of the test set used for the test set generation.
describeTestSetGenerationResponse_roleArn :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
describeTestSetGenerationResponse_roleArn = Lens.lens (\DescribeTestSetGenerationResponse' {roleArn} -> roleArn) (\s@DescribeTestSetGenerationResponse' {} a -> s {roleArn = a} :: DescribeTestSetGenerationResponse)

-- | The Amazon S3 storage location for the test set generation.
describeTestSetGenerationResponse_storageLocation :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe TestSetStorageLocation)
describeTestSetGenerationResponse_storageLocation = Lens.lens (\DescribeTestSetGenerationResponse' {storageLocation} -> storageLocation) (\s@DescribeTestSetGenerationResponse' {} a -> s {storageLocation = a} :: DescribeTestSetGenerationResponse)

-- | The unique identifier of the test set generation.
describeTestSetGenerationResponse_testSetGenerationId :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
describeTestSetGenerationResponse_testSetGenerationId = Lens.lens (\DescribeTestSetGenerationResponse' {testSetGenerationId} -> testSetGenerationId) (\s@DescribeTestSetGenerationResponse' {} a -> s {testSetGenerationId = a} :: DescribeTestSetGenerationResponse)

-- | The status for the test set generation.
describeTestSetGenerationResponse_testSetGenerationStatus :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe TestSetGenerationStatus)
describeTestSetGenerationResponse_testSetGenerationStatus = Lens.lens (\DescribeTestSetGenerationResponse' {testSetGenerationStatus} -> testSetGenerationStatus) (\s@DescribeTestSetGenerationResponse' {} a -> s {testSetGenerationStatus = a} :: DescribeTestSetGenerationResponse)

-- | The unique identifier for the test set created for the generated test
-- set.
describeTestSetGenerationResponse_testSetId :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
describeTestSetGenerationResponse_testSetId = Lens.lens (\DescribeTestSetGenerationResponse' {testSetId} -> testSetId) (\s@DescribeTestSetGenerationResponse' {} a -> s {testSetId = a} :: DescribeTestSetGenerationResponse)

-- | The test set name for the generated test set.
describeTestSetGenerationResponse_testSetName :: Lens.Lens' DescribeTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
describeTestSetGenerationResponse_testSetName = Lens.lens (\DescribeTestSetGenerationResponse' {testSetName} -> testSetName) (\s@DescribeTestSetGenerationResponse' {} a -> s {testSetName = a} :: DescribeTestSetGenerationResponse)

-- | The response's http status code.
describeTestSetGenerationResponse_httpStatus :: Lens.Lens' DescribeTestSetGenerationResponse Prelude.Int
describeTestSetGenerationResponse_httpStatus = Lens.lens (\DescribeTestSetGenerationResponse' {httpStatus} -> httpStatus) (\s@DescribeTestSetGenerationResponse' {} a -> s {httpStatus = a} :: DescribeTestSetGenerationResponse)

instance
  Prelude.NFData
    DescribeTestSetGenerationResponse
  where
  rnf DescribeTestSetGenerationResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf generationDataSource
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf testSetGenerationId
      `Prelude.seq` Prelude.rnf testSetGenerationStatus
      `Prelude.seq` Prelude.rnf testSetId
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf httpStatus
