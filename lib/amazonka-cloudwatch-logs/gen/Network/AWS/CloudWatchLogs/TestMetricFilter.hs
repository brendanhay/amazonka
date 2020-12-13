{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.
module Network.AWS.CloudWatchLogs.TestMetricFilter
  ( -- * Creating a request
    TestMetricFilter (..),
    mkTestMetricFilter,

    -- ** Request lenses
    tmfFilterPattern,
    tmfLogEventMessages,

    -- * Destructuring the response
    TestMetricFilterResponse (..),
    mkTestMetricFilterResponse,

    -- ** Response lenses
    tmfrsMatches,
    tmfrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTestMetricFilter' smart constructor.
data TestMetricFilter = TestMetricFilter'
  { filterPattern :: Lude.Text,
    -- | The log event messages to test.
    logEventMessages :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestMetricFilter' with the minimum fields required to make a request.
--
-- * 'filterPattern' -
-- * 'logEventMessages' - The log event messages to test.
mkTestMetricFilter ::
  -- | 'filterPattern'
  Lude.Text ->
  -- | 'logEventMessages'
  Lude.NonEmpty Lude.Text ->
  TestMetricFilter
mkTestMetricFilter pFilterPattern_ pLogEventMessages_ =
  TestMetricFilter'
    { filterPattern = pFilterPattern_,
      logEventMessages = pLogEventMessages_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfFilterPattern :: Lens.Lens' TestMetricFilter Lude.Text
tmfFilterPattern = Lens.lens (filterPattern :: TestMetricFilter -> Lude.Text) (\s a -> s {filterPattern = a} :: TestMetricFilter)
{-# DEPRECATED tmfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The log event messages to test.
--
-- /Note:/ Consider using 'logEventMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfLogEventMessages :: Lens.Lens' TestMetricFilter (Lude.NonEmpty Lude.Text)
tmfLogEventMessages = Lens.lens (logEventMessages :: TestMetricFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {logEventMessages = a} :: TestMetricFilter)
{-# DEPRECATED tmfLogEventMessages "Use generic-lens or generic-optics with 'logEventMessages' instead." #-}

instance Lude.AWSRequest TestMetricFilter where
  type Rs TestMetricFilter = TestMetricFilterResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TestMetricFilterResponse'
            Lude.<$> (x Lude..?> "matches" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TestMetricFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.TestMetricFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TestMetricFilter where
  toJSON TestMetricFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filterPattern" Lude..= filterPattern),
            Lude.Just ("logEventMessages" Lude..= logEventMessages)
          ]
      )

instance Lude.ToPath TestMetricFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery TestMetricFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTestMetricFilterResponse' smart constructor.
data TestMetricFilterResponse = TestMetricFilterResponse'
  { -- | The matched events.
    matches :: Lude.Maybe [MetricFilterMatchRecord],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestMetricFilterResponse' with the minimum fields required to make a request.
--
-- * 'matches' - The matched events.
-- * 'responseStatus' - The response status code.
mkTestMetricFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TestMetricFilterResponse
mkTestMetricFilterResponse pResponseStatus_ =
  TestMetricFilterResponse'
    { matches = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The matched events.
--
-- /Note:/ Consider using 'matches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrsMatches :: Lens.Lens' TestMetricFilterResponse (Lude.Maybe [MetricFilterMatchRecord])
tmfrsMatches = Lens.lens (matches :: TestMetricFilterResponse -> Lude.Maybe [MetricFilterMatchRecord]) (\s a -> s {matches = a} :: TestMetricFilterResponse)
{-# DEPRECATED tmfrsMatches "Use generic-lens or generic-optics with 'matches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrsResponseStatus :: Lens.Lens' TestMetricFilterResponse Lude.Int
tmfrsResponseStatus = Lens.lens (responseStatus :: TestMetricFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TestMetricFilterResponse)
{-# DEPRECATED tmfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
