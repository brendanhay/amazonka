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
-- Module      : Amazonka.LexV2Models.StartTestSetGeneration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The action to start the generation of test set.
module Amazonka.LexV2Models.StartTestSetGeneration
  ( -- * Creating a Request
    StartTestSetGeneration (..),
    newStartTestSetGeneration,

    -- * Request Lenses
    startTestSetGeneration_description,
    startTestSetGeneration_testSetTags,
    startTestSetGeneration_testSetName,
    startTestSetGeneration_storageLocation,
    startTestSetGeneration_generationDataSource,
    startTestSetGeneration_roleArn,

    -- * Destructuring the Response
    StartTestSetGenerationResponse (..),
    newStartTestSetGenerationResponse,

    -- * Response Lenses
    startTestSetGenerationResponse_creationDateTime,
    startTestSetGenerationResponse_description,
    startTestSetGenerationResponse_generationDataSource,
    startTestSetGenerationResponse_roleArn,
    startTestSetGenerationResponse_storageLocation,
    startTestSetGenerationResponse_testSetGenerationId,
    startTestSetGenerationResponse_testSetGenerationStatus,
    startTestSetGenerationResponse_testSetName,
    startTestSetGenerationResponse_testSetTags,
    startTestSetGenerationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartTestSetGeneration' smart constructor.
data StartTestSetGeneration = StartTestSetGeneration'
  { -- | The test set description for the test set generation request.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to add to the test set. You can only add tags when you
    -- import\/generate a new test set. You can\'t use the @UpdateTestSet@
    -- operation to update tags. To update tags, use the @TagResource@
    -- operation.
    testSetTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The test set name for the test set generation request.
    testSetName :: Prelude.Text,
    -- | The Amazon S3 storage location for the test set generation.
    storageLocation :: TestSetStorageLocation,
    -- | The data source for the test set generation.
    generationDataSource :: TestSetGenerationDataSource,
    -- | The roleARN used for any operation in the test set to access resources
    -- in the Amazon Web Services account.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTestSetGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startTestSetGeneration_description' - The test set description for the test set generation request.
--
-- 'testSetTags', 'startTestSetGeneration_testSetTags' - A list of tags to add to the test set. You can only add tags when you
-- import\/generate a new test set. You can\'t use the @UpdateTestSet@
-- operation to update tags. To update tags, use the @TagResource@
-- operation.
--
-- 'testSetName', 'startTestSetGeneration_testSetName' - The test set name for the test set generation request.
--
-- 'storageLocation', 'startTestSetGeneration_storageLocation' - The Amazon S3 storage location for the test set generation.
--
-- 'generationDataSource', 'startTestSetGeneration_generationDataSource' - The data source for the test set generation.
--
-- 'roleArn', 'startTestSetGeneration_roleArn' - The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
newStartTestSetGeneration ::
  -- | 'testSetName'
  Prelude.Text ->
  -- | 'storageLocation'
  TestSetStorageLocation ->
  -- | 'generationDataSource'
  TestSetGenerationDataSource ->
  -- | 'roleArn'
  Prelude.Text ->
  StartTestSetGeneration
newStartTestSetGeneration
  pTestSetName_
  pStorageLocation_
  pGenerationDataSource_
  pRoleArn_ =
    StartTestSetGeneration'
      { description =
          Prelude.Nothing,
        testSetTags = Prelude.Nothing,
        testSetName = pTestSetName_,
        storageLocation = pStorageLocation_,
        generationDataSource = pGenerationDataSource_,
        roleArn = pRoleArn_
      }

-- | The test set description for the test set generation request.
startTestSetGeneration_description :: Lens.Lens' StartTestSetGeneration (Prelude.Maybe Prelude.Text)
startTestSetGeneration_description = Lens.lens (\StartTestSetGeneration' {description} -> description) (\s@StartTestSetGeneration' {} a -> s {description = a} :: StartTestSetGeneration)

-- | A list of tags to add to the test set. You can only add tags when you
-- import\/generate a new test set. You can\'t use the @UpdateTestSet@
-- operation to update tags. To update tags, use the @TagResource@
-- operation.
startTestSetGeneration_testSetTags :: Lens.Lens' StartTestSetGeneration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTestSetGeneration_testSetTags = Lens.lens (\StartTestSetGeneration' {testSetTags} -> testSetTags) (\s@StartTestSetGeneration' {} a -> s {testSetTags = a} :: StartTestSetGeneration) Prelude.. Lens.mapping Lens.coerced

-- | The test set name for the test set generation request.
startTestSetGeneration_testSetName :: Lens.Lens' StartTestSetGeneration Prelude.Text
startTestSetGeneration_testSetName = Lens.lens (\StartTestSetGeneration' {testSetName} -> testSetName) (\s@StartTestSetGeneration' {} a -> s {testSetName = a} :: StartTestSetGeneration)

-- | The Amazon S3 storage location for the test set generation.
startTestSetGeneration_storageLocation :: Lens.Lens' StartTestSetGeneration TestSetStorageLocation
startTestSetGeneration_storageLocation = Lens.lens (\StartTestSetGeneration' {storageLocation} -> storageLocation) (\s@StartTestSetGeneration' {} a -> s {storageLocation = a} :: StartTestSetGeneration)

-- | The data source for the test set generation.
startTestSetGeneration_generationDataSource :: Lens.Lens' StartTestSetGeneration TestSetGenerationDataSource
startTestSetGeneration_generationDataSource = Lens.lens (\StartTestSetGeneration' {generationDataSource} -> generationDataSource) (\s@StartTestSetGeneration' {} a -> s {generationDataSource = a} :: StartTestSetGeneration)

-- | The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
startTestSetGeneration_roleArn :: Lens.Lens' StartTestSetGeneration Prelude.Text
startTestSetGeneration_roleArn = Lens.lens (\StartTestSetGeneration' {roleArn} -> roleArn) (\s@StartTestSetGeneration' {} a -> s {roleArn = a} :: StartTestSetGeneration)

instance Core.AWSRequest StartTestSetGeneration where
  type
    AWSResponse StartTestSetGeneration =
      StartTestSetGenerationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTestSetGenerationResponse'
            Prelude.<$> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "generationDataSource")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "storageLocation")
            Prelude.<*> (x Data..?> "testSetGenerationId")
            Prelude.<*> (x Data..?> "testSetGenerationStatus")
            Prelude.<*> (x Data..?> "testSetName")
            Prelude.<*> (x Data..?> "testSetTags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTestSetGeneration where
  hashWithSalt _salt StartTestSetGeneration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` testSetTags
      `Prelude.hashWithSalt` testSetName
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` generationDataSource
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartTestSetGeneration where
  rnf StartTestSetGeneration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf testSetTags
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf generationDataSource
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders StartTestSetGeneration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTestSetGeneration where
  toJSON StartTestSetGeneration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("testSetTags" Data..=) Prelude.<$> testSetTags,
            Prelude.Just ("testSetName" Data..= testSetName),
            Prelude.Just
              ("storageLocation" Data..= storageLocation),
            Prelude.Just
              ( "generationDataSource"
                  Data..= generationDataSource
              ),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartTestSetGeneration where
  toPath = Prelude.const "/testsetgenerations"

instance Data.ToQuery StartTestSetGeneration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartTestSetGenerationResponse' smart constructor.
data StartTestSetGenerationResponse = StartTestSetGenerationResponse'
  { -- | The creation date and time for the test set generation.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description used for the test set generation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The data source for the test set generation.
    generationDataSource :: Prelude.Maybe TestSetGenerationDataSource,
    -- | The roleARN used for any operation in the test set to access resources
    -- in the Amazon Web Services account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 storage location for the test set generation.
    storageLocation :: Prelude.Maybe TestSetStorageLocation,
    -- | The unique identifier of the test set generation to describe.
    testSetGenerationId :: Prelude.Maybe Prelude.Text,
    -- | The status for the test set generation.
    testSetGenerationStatus :: Prelude.Maybe TestSetGenerationStatus,
    -- | The test set name used for the test set generation.
    testSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that was used for the test set that is being generated.
    testSetTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTestSetGenerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'startTestSetGenerationResponse_creationDateTime' - The creation date and time for the test set generation.
--
-- 'description', 'startTestSetGenerationResponse_description' - The description used for the test set generation.
--
-- 'generationDataSource', 'startTestSetGenerationResponse_generationDataSource' - The data source for the test set generation.
--
-- 'roleArn', 'startTestSetGenerationResponse_roleArn' - The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
--
-- 'storageLocation', 'startTestSetGenerationResponse_storageLocation' - The Amazon S3 storage location for the test set generation.
--
-- 'testSetGenerationId', 'startTestSetGenerationResponse_testSetGenerationId' - The unique identifier of the test set generation to describe.
--
-- 'testSetGenerationStatus', 'startTestSetGenerationResponse_testSetGenerationStatus' - The status for the test set generation.
--
-- 'testSetName', 'startTestSetGenerationResponse_testSetName' - The test set name used for the test set generation.
--
-- 'testSetTags', 'startTestSetGenerationResponse_testSetTags' - A list of tags that was used for the test set that is being generated.
--
-- 'httpStatus', 'startTestSetGenerationResponse_httpStatus' - The response's http status code.
newStartTestSetGenerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTestSetGenerationResponse
newStartTestSetGenerationResponse pHttpStatus_ =
  StartTestSetGenerationResponse'
    { creationDateTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      generationDataSource = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      testSetGenerationId = Prelude.Nothing,
      testSetGenerationStatus = Prelude.Nothing,
      testSetName = Prelude.Nothing,
      testSetTags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation date and time for the test set generation.
startTestSetGenerationResponse_creationDateTime :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe Prelude.UTCTime)
startTestSetGenerationResponse_creationDateTime = Lens.lens (\StartTestSetGenerationResponse' {creationDateTime} -> creationDateTime) (\s@StartTestSetGenerationResponse' {} a -> s {creationDateTime = a} :: StartTestSetGenerationResponse) Prelude.. Lens.mapping Data._Time

-- | The description used for the test set generation.
startTestSetGenerationResponse_description :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
startTestSetGenerationResponse_description = Lens.lens (\StartTestSetGenerationResponse' {description} -> description) (\s@StartTestSetGenerationResponse' {} a -> s {description = a} :: StartTestSetGenerationResponse)

-- | The data source for the test set generation.
startTestSetGenerationResponse_generationDataSource :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe TestSetGenerationDataSource)
startTestSetGenerationResponse_generationDataSource = Lens.lens (\StartTestSetGenerationResponse' {generationDataSource} -> generationDataSource) (\s@StartTestSetGenerationResponse' {} a -> s {generationDataSource = a} :: StartTestSetGenerationResponse)

-- | The roleARN used for any operation in the test set to access resources
-- in the Amazon Web Services account.
startTestSetGenerationResponse_roleArn :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
startTestSetGenerationResponse_roleArn = Lens.lens (\StartTestSetGenerationResponse' {roleArn} -> roleArn) (\s@StartTestSetGenerationResponse' {} a -> s {roleArn = a} :: StartTestSetGenerationResponse)

-- | The Amazon S3 storage location for the test set generation.
startTestSetGenerationResponse_storageLocation :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe TestSetStorageLocation)
startTestSetGenerationResponse_storageLocation = Lens.lens (\StartTestSetGenerationResponse' {storageLocation} -> storageLocation) (\s@StartTestSetGenerationResponse' {} a -> s {storageLocation = a} :: StartTestSetGenerationResponse)

-- | The unique identifier of the test set generation to describe.
startTestSetGenerationResponse_testSetGenerationId :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
startTestSetGenerationResponse_testSetGenerationId = Lens.lens (\StartTestSetGenerationResponse' {testSetGenerationId} -> testSetGenerationId) (\s@StartTestSetGenerationResponse' {} a -> s {testSetGenerationId = a} :: StartTestSetGenerationResponse)

-- | The status for the test set generation.
startTestSetGenerationResponse_testSetGenerationStatus :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe TestSetGenerationStatus)
startTestSetGenerationResponse_testSetGenerationStatus = Lens.lens (\StartTestSetGenerationResponse' {testSetGenerationStatus} -> testSetGenerationStatus) (\s@StartTestSetGenerationResponse' {} a -> s {testSetGenerationStatus = a} :: StartTestSetGenerationResponse)

-- | The test set name used for the test set generation.
startTestSetGenerationResponse_testSetName :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe Prelude.Text)
startTestSetGenerationResponse_testSetName = Lens.lens (\StartTestSetGenerationResponse' {testSetName} -> testSetName) (\s@StartTestSetGenerationResponse' {} a -> s {testSetName = a} :: StartTestSetGenerationResponse)

-- | A list of tags that was used for the test set that is being generated.
startTestSetGenerationResponse_testSetTags :: Lens.Lens' StartTestSetGenerationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startTestSetGenerationResponse_testSetTags = Lens.lens (\StartTestSetGenerationResponse' {testSetTags} -> testSetTags) (\s@StartTestSetGenerationResponse' {} a -> s {testSetTags = a} :: StartTestSetGenerationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startTestSetGenerationResponse_httpStatus :: Lens.Lens' StartTestSetGenerationResponse Prelude.Int
startTestSetGenerationResponse_httpStatus = Lens.lens (\StartTestSetGenerationResponse' {httpStatus} -> httpStatus) (\s@StartTestSetGenerationResponse' {} a -> s {httpStatus = a} :: StartTestSetGenerationResponse)

instance
  Prelude.NFData
    StartTestSetGenerationResponse
  where
  rnf StartTestSetGenerationResponse' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf generationDataSource
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf testSetGenerationId
      `Prelude.seq` Prelude.rnf testSetGenerationStatus
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf testSetTags
      `Prelude.seq` Prelude.rnf httpStatus
