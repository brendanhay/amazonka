{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetLogRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the fields and values of a single log event. All fields are retrieved, even if the original query that produced the @logRecordPointer@ retrieved only a subset of fields. Fields are returned as field name/field value pairs.
--
-- The full unparsed log event is returned within @@message@ .
module Network.AWS.CloudWatchLogs.GetLogRecord
  ( -- * Creating a request
    GetLogRecord (..),
    mkGetLogRecord,

    -- ** Request lenses
    glrLogRecordPointer,

    -- * Destructuring the response
    GetLogRecordResponse (..),
    mkGetLogRecordResponse,

    -- ** Response lenses
    glrrsLogRecord,
    glrrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLogRecord' smart constructor.
newtype GetLogRecord = GetLogRecord'
  { -- | The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
    logRecordPointer :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLogRecord' with the minimum fields required to make a request.
--
-- * 'logRecordPointer' - The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
mkGetLogRecord ::
  -- | 'logRecordPointer'
  Lude.Text ->
  GetLogRecord
mkGetLogRecord pLogRecordPointer_ =
  GetLogRecord' {logRecordPointer = pLogRecordPointer_}

-- | The pointer corresponding to the log event record you want to retrieve. You get this from the response of a @GetQueryResults@ operation. In that response, the value of the @@ptr@ field for a log event is the value to use as @logRecordPointer@ to retrieve that complete log event record.
--
-- /Note:/ Consider using 'logRecordPointer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrLogRecordPointer :: Lens.Lens' GetLogRecord Lude.Text
glrLogRecordPointer = Lens.lens (logRecordPointer :: GetLogRecord -> Lude.Text) (\s a -> s {logRecordPointer = a} :: GetLogRecord)
{-# DEPRECATED glrLogRecordPointer "Use generic-lens or generic-optics with 'logRecordPointer' instead." #-}

instance Lude.AWSRequest GetLogRecord where
  type Rs GetLogRecord = GetLogRecordResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLogRecordResponse'
            Lude.<$> (x Lude..?> "logRecord" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLogRecord where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.GetLogRecord" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLogRecord where
  toJSON GetLogRecord' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("logRecordPointer" Lude..= logRecordPointer)]
      )

instance Lude.ToPath GetLogRecord where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLogRecord where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLogRecordResponse' smart constructor.
data GetLogRecordResponse = GetLogRecordResponse'
  { -- | The requested log event, as a JSON string.
    logRecord :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLogRecordResponse' with the minimum fields required to make a request.
--
-- * 'logRecord' - The requested log event, as a JSON string.
-- * 'responseStatus' - The response status code.
mkGetLogRecordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLogRecordResponse
mkGetLogRecordResponse pResponseStatus_ =
  GetLogRecordResponse'
    { logRecord = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested log event, as a JSON string.
--
-- /Note:/ Consider using 'logRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrsLogRecord :: Lens.Lens' GetLogRecordResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
glrrsLogRecord = Lens.lens (logRecord :: GetLogRecordResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {logRecord = a} :: GetLogRecordResponse)
{-# DEPRECATED glrrsLogRecord "Use generic-lens or generic-optics with 'logRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glrrsResponseStatus :: Lens.Lens' GetLogRecordResponse Lude.Int
glrrsResponseStatus = Lens.lens (responseStatus :: GetLogRecordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLogRecordResponse)
{-# DEPRECATED glrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
