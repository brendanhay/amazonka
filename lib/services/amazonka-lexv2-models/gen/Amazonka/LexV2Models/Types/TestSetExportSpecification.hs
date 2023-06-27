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
-- Module      : Amazonka.LexV2Models.Types.TestSetExportSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestSetExportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the test set that is exported.
--
-- /See:/ 'newTestSetExportSpecification' smart constructor.
data TestSetExportSpecification = TestSetExportSpecification'
  { -- | The unique identifier of the test set.
    testSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestSetExportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testSetId', 'testSetExportSpecification_testSetId' - The unique identifier of the test set.
newTestSetExportSpecification ::
  -- | 'testSetId'
  Prelude.Text ->
  TestSetExportSpecification
newTestSetExportSpecification pTestSetId_ =
  TestSetExportSpecification'
    { testSetId =
        pTestSetId_
    }

-- | The unique identifier of the test set.
testSetExportSpecification_testSetId :: Lens.Lens' TestSetExportSpecification Prelude.Text
testSetExportSpecification_testSetId = Lens.lens (\TestSetExportSpecification' {testSetId} -> testSetId) (\s@TestSetExportSpecification' {} a -> s {testSetId = a} :: TestSetExportSpecification)

instance Data.FromJSON TestSetExportSpecification where
  parseJSON =
    Data.withObject
      "TestSetExportSpecification"
      ( \x ->
          TestSetExportSpecification'
            Prelude.<$> (x Data..: "testSetId")
      )

instance Prelude.Hashable TestSetExportSpecification where
  hashWithSalt _salt TestSetExportSpecification' {..} =
    _salt `Prelude.hashWithSalt` testSetId

instance Prelude.NFData TestSetExportSpecification where
  rnf TestSetExportSpecification' {..} =
    Prelude.rnf testSetId

instance Data.ToJSON TestSetExportSpecification where
  toJSON TestSetExportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("testSetId" Data..= testSetId)]
      )
