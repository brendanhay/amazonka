{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCase
  ( TestCase (..),

    -- * Smart constructor
    mkTestCase,

    -- * Lenses
    tcDurationInNanoSeconds,
    tcExpired,
    tcMessage,
    tcName,
    tcPrefix,
    tcReportArn,
    tcStatus,
    tcTestRawDataPath,
  )
where

import qualified Network.AWS.CodeBuild.Types.Message as Types
import qualified Network.AWS.CodeBuild.Types.Name as Types
import qualified Network.AWS.CodeBuild.Types.Prefix as Types
import qualified Network.AWS.CodeBuild.Types.ReportArn as Types
import qualified Network.AWS.CodeBuild.Types.Status as Types
import qualified Network.AWS.CodeBuild.Types.TestRawDataPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a test case created using a framework such as NUnit or Cucumber. A test case might be a unit test or a configuration test.
--
-- /See:/ 'mkTestCase' smart constructor.
data TestCase = TestCase'
  { -- | The number of nanoseconds it took to run this test case.
    durationInNanoSeconds :: Core.Maybe Core.Integer,
    -- | The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
    expired :: Core.Maybe Core.NominalDiffTime,
    -- | A message associated with a test case. For example, an error message or stack trace.
    message :: Core.Maybe Types.Message,
    -- | The name of the test case.
    name :: Core.Maybe Types.Name,
    -- | A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
    prefix :: Core.Maybe Types.Prefix,
    -- | The ARN of the report to which the test case belongs.
    reportArn :: Core.Maybe Types.ReportArn,
    -- | The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
    status :: Core.Maybe Types.Status,
    -- | The path to the raw data file that contains the test result.
    testRawDataPath :: Core.Maybe Types.TestRawDataPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TestCase' value with any optional fields omitted.
mkTestCase ::
  TestCase
mkTestCase =
  TestCase'
    { durationInNanoSeconds = Core.Nothing,
      expired = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      prefix = Core.Nothing,
      reportArn = Core.Nothing,
      status = Core.Nothing,
      testRawDataPath = Core.Nothing
    }

-- | The number of nanoseconds it took to run this test case.
--
-- /Note:/ Consider using 'durationInNanoSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcDurationInNanoSeconds :: Lens.Lens' TestCase (Core.Maybe Core.Integer)
tcDurationInNanoSeconds = Lens.field @"durationInNanoSeconds"
{-# DEPRECATED tcDurationInNanoSeconds "Use generic-lens or generic-optics with 'durationInNanoSeconds' instead." #-}

-- | The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcExpired :: Lens.Lens' TestCase (Core.Maybe Core.NominalDiffTime)
tcExpired = Lens.field @"expired"
{-# DEPRECATED tcExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | A message associated with a test case. For example, an error message or stack trace.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMessage :: Lens.Lens' TestCase (Core.Maybe Types.Message)
tcMessage = Lens.field @"message"
{-# DEPRECATED tcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The name of the test case.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcName :: Lens.Lens' TestCase (Core.Maybe Types.Name)
tcName = Lens.field @"name"
{-# DEPRECATED tcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPrefix :: Lens.Lens' TestCase (Core.Maybe Types.Prefix)
tcPrefix = Lens.field @"prefix"
{-# DEPRECATED tcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The ARN of the report to which the test case belongs.
--
-- /Note:/ Consider using 'reportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcReportArn :: Lens.Lens' TestCase (Core.Maybe Types.ReportArn)
tcReportArn = Lens.field @"reportArn"
{-# DEPRECATED tcReportArn "Use generic-lens or generic-optics with 'reportArn' instead." #-}

-- | The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStatus :: Lens.Lens' TestCase (Core.Maybe Types.Status)
tcStatus = Lens.field @"status"
{-# DEPRECATED tcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The path to the raw data file that contains the test result.
--
-- /Note:/ Consider using 'testRawDataPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTestRawDataPath :: Lens.Lens' TestCase (Core.Maybe Types.TestRawDataPath)
tcTestRawDataPath = Lens.field @"testRawDataPath"
{-# DEPRECATED tcTestRawDataPath "Use generic-lens or generic-optics with 'testRawDataPath' instead." #-}

instance Core.FromJSON TestCase where
  parseJSON =
    Core.withObject "TestCase" Core.$
      \x ->
        TestCase'
          Core.<$> (x Core..:? "durationInNanoSeconds")
          Core.<*> (x Core..:? "expired")
          Core.<*> (x Core..:? "message")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "prefix")
          Core.<*> (x Core..:? "reportArn")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "testRawDataPath")
