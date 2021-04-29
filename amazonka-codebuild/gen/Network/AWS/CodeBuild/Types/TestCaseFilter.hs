{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCaseFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A filter used to return specific types of test cases. In order to pass
-- the filter, the report must meet all of the filter properties.
--
-- /See:/ 'newTestCaseFilter' smart constructor.
data TestCaseFilter = TestCaseFilter'
  { -- | The status used to filter test cases. A @TestCaseFilter@ can have one
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
    status :: Prelude.Maybe Prelude.Text,
    -- | A keyword that is used to filter on the @name@ or the @prefix@ of the
    -- test cases. Only test cases where the keyword is a substring of the
    -- @name@ or the @prefix@ will be returned.
    keyword :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TestCaseFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'keyword', 'testCaseFilter_keyword' - A keyword that is used to filter on the @name@ or the @prefix@ of the
-- test cases. Only test cases where the keyword is a substring of the
-- @name@ or the @prefix@ will be returned.
newTestCaseFilter ::
  TestCaseFilter
newTestCaseFilter =
  TestCaseFilter'
    { status = Prelude.Nothing,
      keyword = Prelude.Nothing
    }

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

-- | A keyword that is used to filter on the @name@ or the @prefix@ of the
-- test cases. Only test cases where the keyword is a substring of the
-- @name@ or the @prefix@ will be returned.
testCaseFilter_keyword :: Lens.Lens' TestCaseFilter (Prelude.Maybe Prelude.Text)
testCaseFilter_keyword = Lens.lens (\TestCaseFilter' {keyword} -> keyword) (\s@TestCaseFilter' {} a -> s {keyword = a} :: TestCaseFilter)

instance Prelude.Hashable TestCaseFilter

instance Prelude.NFData TestCaseFilter

instance Prelude.ToJSON TestCaseFilter where
  toJSON TestCaseFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("status" Prelude..=) Prelude.<$> status,
            ("keyword" Prelude..=) Prelude.<$> keyword
          ]
      )
