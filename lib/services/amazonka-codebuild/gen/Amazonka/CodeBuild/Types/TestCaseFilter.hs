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
-- Module      : Amazonka.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.TestCaseFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter used to return specific types of test cases. In order to pass
-- the filter, the report must meet all of the filter properties.
--
-- /See:/ 'newTestCaseFilter' smart constructor.
data TestCaseFilter = TestCaseFilter'
  { -- | A keyword that is used to filter on the @name@ or the @prefix@ of the
    -- test cases. Only test cases where the keyword is a substring of the
    -- @name@ or the @prefix@ will be returned.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | The status used to filter test cases. A @TestCaseFilter@ can have one
    -- status. Valid values are:
    --
    -- -   @SUCCEEDED@
    --
    -- -   @FAILED@
    --
    -- -   @ERROR@
    --
    -- -   @SKIPPED@
    --
    -- -   @UNKNOWN@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCaseFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyword', 'testCaseFilter_keyword' - A keyword that is used to filter on the @name@ or the @prefix@ of the
-- test cases. Only test cases where the keyword is a substring of the
-- @name@ or the @prefix@ will be returned.
--
-- 'status', 'testCaseFilter_status' - The status used to filter test cases. A @TestCaseFilter@ can have one
-- status. Valid values are:
--
-- -   @SUCCEEDED@
--
-- -   @FAILED@
--
-- -   @ERROR@
--
-- -   @SKIPPED@
--
-- -   @UNKNOWN@
newTestCaseFilter ::
  TestCaseFilter
newTestCaseFilter =
  TestCaseFilter'
    { keyword = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A keyword that is used to filter on the @name@ or the @prefix@ of the
-- test cases. Only test cases where the keyword is a substring of the
-- @name@ or the @prefix@ will be returned.
testCaseFilter_keyword :: Lens.Lens' TestCaseFilter (Prelude.Maybe Prelude.Text)
testCaseFilter_keyword = Lens.lens (\TestCaseFilter' {keyword} -> keyword) (\s@TestCaseFilter' {} a -> s {keyword = a} :: TestCaseFilter)

-- | The status used to filter test cases. A @TestCaseFilter@ can have one
-- status. Valid values are:
--
-- -   @SUCCEEDED@
--
-- -   @FAILED@
--
-- -   @ERROR@
--
-- -   @SKIPPED@
--
-- -   @UNKNOWN@
testCaseFilter_status :: Lens.Lens' TestCaseFilter (Prelude.Maybe Prelude.Text)
testCaseFilter_status = Lens.lens (\TestCaseFilter' {status} -> status) (\s@TestCaseFilter' {} a -> s {status = a} :: TestCaseFilter)

instance Prelude.Hashable TestCaseFilter where
  hashWithSalt _salt TestCaseFilter' {..} =
    _salt `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` status

instance Prelude.NFData TestCaseFilter where
  rnf TestCaseFilter' {..} =
    Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON TestCaseFilter where
  toJSON TestCaseFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyword" Data..=) Prelude.<$> keyword,
            ("status" Data..=) Prelude.<$> status
          ]
      )
