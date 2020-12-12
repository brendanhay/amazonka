{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCaseFilter
  ( TestCaseFilter (..),

    -- * Smart constructor
    mkTestCaseFilter,

    -- * Lenses
    tcfStatus,
    tcfKeyword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter used to return specific types of test cases. In order to pass the filter, the report must meet all of the filter properties.
--
-- /See:/ 'mkTestCaseFilter' smart constructor.
data TestCaseFilter = TestCaseFilter'
  { status ::
      Lude.Maybe Lude.Text,
    keyword :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestCaseFilter' with the minimum fields required to make a request.
--
-- * 'keyword' - A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
-- * 'status' - The status used to filter test cases. A @TestCaseFilter@ can have one status. Valid values are:
--
--
--     * @SUCCEEDED@
--
--
--     * @FAILED@
--
--
--     * @ERROR@
--
--
--     * @SKIPPED@
--
--
--     * @UNKNOWN@
mkTestCaseFilter ::
  TestCaseFilter
mkTestCaseFilter =
  TestCaseFilter' {status = Lude.Nothing, keyword = Lude.Nothing}

-- | The status used to filter test cases. A @TestCaseFilter@ can have one status. Valid values are:
--
--
--     * @SUCCEEDED@
--
--
--     * @FAILED@
--
--
--     * @ERROR@
--
--
--     * @SKIPPED@
--
--
--     * @UNKNOWN@
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcfStatus :: Lens.Lens' TestCaseFilter (Lude.Maybe Lude.Text)
tcfStatus = Lens.lens (status :: TestCaseFilter -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: TestCaseFilter)
{-# DEPRECATED tcfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
--
-- /Note:/ Consider using 'keyword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcfKeyword :: Lens.Lens' TestCaseFilter (Lude.Maybe Lude.Text)
tcfKeyword = Lens.lens (keyword :: TestCaseFilter -> Lude.Maybe Lude.Text) (\s a -> s {keyword = a} :: TestCaseFilter)
{-# DEPRECATED tcfKeyword "Use generic-lens or generic-optics with 'keyword' instead." #-}

instance Lude.ToJSON TestCaseFilter where
  toJSON TestCaseFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("keyword" Lude..=) Lude.<$> keyword
          ]
      )
