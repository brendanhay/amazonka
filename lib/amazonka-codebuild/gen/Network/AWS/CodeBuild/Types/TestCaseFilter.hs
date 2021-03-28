{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.TestCaseFilter
  ( TestCaseFilter (..)
  -- * Smart constructor
  , mkTestCaseFilter
  -- * Lenses
  , tcfKeyword
  , tcfStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter used to return specific types of test cases. In order to pass the filter, the report must meet all of the filter properties.
--
-- /See:/ 'mkTestCaseFilter' smart constructor.
data TestCaseFilter = TestCaseFilter'
  { keyword :: Core.Maybe Core.Text
    -- ^ A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
  , status :: Core.Maybe Core.Text
    -- ^ The status used to filter test cases. A @TestCaseFilter@ can have one status. Valid values are:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestCaseFilter' value with any optional fields omitted.
mkTestCaseFilter
    :: TestCaseFilter
mkTestCaseFilter
  = TestCaseFilter'{keyword = Core.Nothing, status = Core.Nothing}

-- | A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
--
-- /Note:/ Consider using 'keyword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcfKeyword :: Lens.Lens' TestCaseFilter (Core.Maybe Core.Text)
tcfKeyword = Lens.field @"keyword"
{-# INLINEABLE tcfKeyword #-}
{-# DEPRECATED keyword "Use generic-lens or generic-optics with 'keyword' instead"  #-}

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
tcfStatus :: Lens.Lens' TestCaseFilter (Core.Maybe Core.Text)
tcfStatus = Lens.field @"status"
{-# INLINEABLE tcfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON TestCaseFilter where
        toJSON TestCaseFilter{..}
          = Core.object
              (Core.catMaybes
                 [("keyword" Core..=) Core.<$> keyword,
                  ("status" Core..=) Core.<$> status])
