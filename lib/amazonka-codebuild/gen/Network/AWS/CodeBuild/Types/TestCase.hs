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
    tcStatus,
    tcExpired,
    tcPrefix,
    tcName,
    tcTestRawDataPath,
    tcMessage,
    tcReportARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a test case created using a framework such as NUnit or Cucumber. A test case might be a unit test or a configuration test.
--
-- /See:/ 'mkTestCase' smart constructor.
data TestCase = TestCase'
  { durationInNanoSeconds ::
      Lude.Maybe Lude.Integer,
    status :: Lude.Maybe Lude.Text,
    expired :: Lude.Maybe Lude.Timestamp,
    prefix :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    testRawDataPath :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    reportARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestCase' with the minimum fields required to make a request.
--
-- * 'durationInNanoSeconds' - The number of nanoseconds it took to run this test case.
-- * 'expired' - The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
-- * 'message' - A message associated with a test case. For example, an error message or stack trace.
-- * 'name' - The name of the test case.
-- * 'prefix' - A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
-- * 'reportARN' - The ARN of the report to which the test case belongs.
-- * 'status' - The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
-- * 'testRawDataPath' - The path to the raw data file that contains the test result.
mkTestCase ::
  TestCase
mkTestCase =
  TestCase'
    { durationInNanoSeconds = Lude.Nothing,
      status = Lude.Nothing,
      expired = Lude.Nothing,
      prefix = Lude.Nothing,
      name = Lude.Nothing,
      testRawDataPath = Lude.Nothing,
      message = Lude.Nothing,
      reportARN = Lude.Nothing
    }

-- | The number of nanoseconds it took to run this test case.
--
-- /Note:/ Consider using 'durationInNanoSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcDurationInNanoSeconds :: Lens.Lens' TestCase (Lude.Maybe Lude.Integer)
tcDurationInNanoSeconds = Lens.lens (durationInNanoSeconds :: TestCase -> Lude.Maybe Lude.Integer) (\s a -> s {durationInNanoSeconds = a} :: TestCase)
{-# DEPRECATED tcDurationInNanoSeconds "Use generic-lens or generic-optics with 'durationInNanoSeconds' instead." #-}

-- | The status returned by the test case after it was run. Valid statuses are @SUCCEEDED@ , @FAILED@ , @ERROR@ , @SKIPPED@ , and @UNKNOWN@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStatus :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcStatus = Lens.lens (status :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: TestCase)
{-# DEPRECATED tcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time a test case expires. A test case expires 30 days after it is created. An expired test case is not available to view in CodeBuild.
--
-- /Note:/ Consider using 'expired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcExpired :: Lens.Lens' TestCase (Lude.Maybe Lude.Timestamp)
tcExpired = Lens.lens (expired :: TestCase -> Lude.Maybe Lude.Timestamp) (\s a -> s {expired = a} :: TestCase)
{-# DEPRECATED tcExpired "Use generic-lens or generic-optics with 'expired' instead." #-}

-- | A string that is applied to a series of related test cases. CodeBuild generates the prefix. The prefix depends on the framework used to generate the tests.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPrefix :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcPrefix = Lens.lens (prefix :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: TestCase)
{-# DEPRECATED tcPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The name of the test case.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcName :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcName = Lens.lens (name :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: TestCase)
{-# DEPRECATED tcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The path to the raw data file that contains the test result.
--
-- /Note:/ Consider using 'testRawDataPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTestRawDataPath :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcTestRawDataPath = Lens.lens (testRawDataPath :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {testRawDataPath = a} :: TestCase)
{-# DEPRECATED tcTestRawDataPath "Use generic-lens or generic-optics with 'testRawDataPath' instead." #-}

-- | A message associated with a test case. For example, an error message or stack trace.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMessage :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcMessage = Lens.lens (message :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: TestCase)
{-# DEPRECATED tcMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ARN of the report to which the test case belongs.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcReportARN :: Lens.Lens' TestCase (Lude.Maybe Lude.Text)
tcReportARN = Lens.lens (reportARN :: TestCase -> Lude.Maybe Lude.Text) (\s a -> s {reportARN = a} :: TestCase)
{-# DEPRECATED tcReportARN "Use generic-lens or generic-optics with 'reportARN' instead." #-}

instance Lude.FromJSON TestCase where
  parseJSON =
    Lude.withObject
      "TestCase"
      ( \x ->
          TestCase'
            Lude.<$> (x Lude..:? "durationInNanoSeconds")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "expired")
            Lude.<*> (x Lude..:? "prefix")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "testRawDataPath")
            Lude.<*> (x Lude..:? "message")
            Lude.<*> (x Lude..:? "reportArn")
      )
