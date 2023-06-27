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
-- Module      : Amazonka.LexV2Models.Types.TestSetImportResourceSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetImportResourceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.TestSetImportInputLocation
import Amazonka.LexV2Models.Types.TestSetModality
import Amazonka.LexV2Models.Types.TestSetStorageLocation
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the test set that is imported.
--
-- /See:/ 'newTestSetImportResourceSpecification' smart constructor.
data TestSetImportResourceSpecification = TestSetImportResourceSpecification'
  { -- | The description of the test set.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to add to the test set. You can only add tags when you
    -- import\/generate a new test set. You can\'t use the @UpdateTestSet@
    -- operation to update tags. To update tags, use the @TagResource@
    -- operation.
    testSetTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the test set.
    testSetName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- access the test set.
    roleArn :: Prelude.Text,
    -- | Contains information about the location that Amazon Lex uses to store
    -- the test-set.
    storageLocation :: TestSetStorageLocation,
    -- | Contains information about the input location from where test-set should
    -- be imported.
    importInputLocation :: TestSetImportInputLocation,
    -- | Specifies whether the test-set being imported contains written or spoken
    -- data.
    modality :: TestSetModality
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetImportResourceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'testSetImportResourceSpecification_description' - The description of the test set.
--
-- 'testSetTags', 'testSetImportResourceSpecification_testSetTags' - A list of tags to add to the test set. You can only add tags when you
-- import\/generate a new test set. You can\'t use the @UpdateTestSet@
-- operation to update tags. To update tags, use the @TagResource@
-- operation.
--
-- 'testSetName', 'testSetImportResourceSpecification_testSetName' - The name of the test set.
--
-- 'roleArn', 'testSetImportResourceSpecification_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the test set.
--
-- 'storageLocation', 'testSetImportResourceSpecification_storageLocation' - Contains information about the location that Amazon Lex uses to store
-- the test-set.
--
-- 'importInputLocation', 'testSetImportResourceSpecification_importInputLocation' - Contains information about the input location from where test-set should
-- be imported.
--
-- 'modality', 'testSetImportResourceSpecification_modality' - Specifies whether the test-set being imported contains written or spoken
-- data.
newTestSetImportResourceSpecification ::
  -- | 'testSetName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'storageLocation'
  TestSetStorageLocation ->
  -- | 'importInputLocation'
  TestSetImportInputLocation ->
  -- | 'modality'
  TestSetModality ->
  TestSetImportResourceSpecification
newTestSetImportResourceSpecification
  pTestSetName_
  pRoleArn_
  pStorageLocation_
  pImportInputLocation_
  pModality_ =
    TestSetImportResourceSpecification'
      { description =
          Prelude.Nothing,
        testSetTags = Prelude.Nothing,
        testSetName = pTestSetName_,
        roleArn = pRoleArn_,
        storageLocation = pStorageLocation_,
        importInputLocation =
          pImportInputLocation_,
        modality = pModality_
      }

-- | The description of the test set.
testSetImportResourceSpecification_description :: Lens.Lens' TestSetImportResourceSpecification (Prelude.Maybe Prelude.Text)
testSetImportResourceSpecification_description = Lens.lens (\TestSetImportResourceSpecification' {description} -> description) (\s@TestSetImportResourceSpecification' {} a -> s {description = a} :: TestSetImportResourceSpecification)

-- | A list of tags to add to the test set. You can only add tags when you
-- import\/generate a new test set. You can\'t use the @UpdateTestSet@
-- operation to update tags. To update tags, use the @TagResource@
-- operation.
testSetImportResourceSpecification_testSetTags :: Lens.Lens' TestSetImportResourceSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
testSetImportResourceSpecification_testSetTags = Lens.lens (\TestSetImportResourceSpecification' {testSetTags} -> testSetTags) (\s@TestSetImportResourceSpecification' {} a -> s {testSetTags = a} :: TestSetImportResourceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The name of the test set.
testSetImportResourceSpecification_testSetName :: Lens.Lens' TestSetImportResourceSpecification Prelude.Text
testSetImportResourceSpecification_testSetName = Lens.lens (\TestSetImportResourceSpecification' {testSetName} -> testSetName) (\s@TestSetImportResourceSpecification' {} a -> s {testSetName = a} :: TestSetImportResourceSpecification)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the test set.
testSetImportResourceSpecification_roleArn :: Lens.Lens' TestSetImportResourceSpecification Prelude.Text
testSetImportResourceSpecification_roleArn = Lens.lens (\TestSetImportResourceSpecification' {roleArn} -> roleArn) (\s@TestSetImportResourceSpecification' {} a -> s {roleArn = a} :: TestSetImportResourceSpecification)

-- | Contains information about the location that Amazon Lex uses to store
-- the test-set.
testSetImportResourceSpecification_storageLocation :: Lens.Lens' TestSetImportResourceSpecification TestSetStorageLocation
testSetImportResourceSpecification_storageLocation = Lens.lens (\TestSetImportResourceSpecification' {storageLocation} -> storageLocation) (\s@TestSetImportResourceSpecification' {} a -> s {storageLocation = a} :: TestSetImportResourceSpecification)

-- | Contains information about the input location from where test-set should
-- be imported.
testSetImportResourceSpecification_importInputLocation :: Lens.Lens' TestSetImportResourceSpecification TestSetImportInputLocation
testSetImportResourceSpecification_importInputLocation = Lens.lens (\TestSetImportResourceSpecification' {importInputLocation} -> importInputLocation) (\s@TestSetImportResourceSpecification' {} a -> s {importInputLocation = a} :: TestSetImportResourceSpecification)

-- | Specifies whether the test-set being imported contains written or spoken
-- data.
testSetImportResourceSpecification_modality :: Lens.Lens' TestSetImportResourceSpecification TestSetModality
testSetImportResourceSpecification_modality = Lens.lens (\TestSetImportResourceSpecification' {modality} -> modality) (\s@TestSetImportResourceSpecification' {} a -> s {modality = a} :: TestSetImportResourceSpecification)

instance
  Data.FromJSON
    TestSetImportResourceSpecification
  where
  parseJSON =
    Data.withObject
      "TestSetImportResourceSpecification"
      ( \x ->
          TestSetImportResourceSpecification'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "testSetTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "testSetName")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "storageLocation")
            Prelude.<*> (x Data..: "importInputLocation")
            Prelude.<*> (x Data..: "modality")
      )

instance
  Prelude.Hashable
    TestSetImportResourceSpecification
  where
  hashWithSalt
    _salt
    TestSetImportResourceSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` testSetTags
        `Prelude.hashWithSalt` testSetName
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` storageLocation
        `Prelude.hashWithSalt` importInputLocation
        `Prelude.hashWithSalt` modality

instance
  Prelude.NFData
    TestSetImportResourceSpecification
  where
  rnf TestSetImportResourceSpecification' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf testSetTags
      `Prelude.seq` Prelude.rnf testSetName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf importInputLocation
      `Prelude.seq` Prelude.rnf modality

instance
  Data.ToJSON
    TestSetImportResourceSpecification
  where
  toJSON TestSetImportResourceSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("testSetTags" Data..=) Prelude.<$> testSetTags,
            Prelude.Just ("testSetName" Data..= testSetName),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just
              ("storageLocation" Data..= storageLocation),
            Prelude.Just
              ("importInputLocation" Data..= importInputLocation),
            Prelude.Just ("modality" Data..= modality)
          ]
      )
