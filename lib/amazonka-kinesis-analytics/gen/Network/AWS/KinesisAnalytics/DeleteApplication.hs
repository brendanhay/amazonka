{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplication@ action.
module Network.AWS.KinesisAnalytics.DeleteApplication
  ( -- * Creating a request
    DeleteApplication (..),
    mkDeleteApplication,

    -- ** Request lenses
    dApplicationName,
    dCreateTimestamp,

    -- * Destructuring the response
    DeleteApplicationResponse (..),
    mkDeleteApplicationResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { applicationName ::
      Lude.Text,
    createTimestamp :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of the Amazon Kinesis Analytics application to delete.
-- * 'createTimestamp' - You can use the @DescribeApplication@ operation to get this value.
mkDeleteApplication ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'createTimestamp'
  Lude.Timestamp ->
  DeleteApplication
mkDeleteApplication pApplicationName_ pCreateTimestamp_ =
  DeleteApplication'
    { applicationName = pApplicationName_,
      createTimestamp = pCreateTimestamp_
    }

-- | Name of the Amazon Kinesis Analytics application to delete.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationName :: Lens.Lens' DeleteApplication Lude.Text
dApplicationName = Lens.lens (applicationName :: DeleteApplication -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplication)
{-# DEPRECATED dApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | You can use the @DescribeApplication@ operation to get this value.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreateTimestamp :: Lens.Lens' DeleteApplication Lude.Timestamp
dCreateTimestamp = Lens.lens (createTimestamp :: DeleteApplication -> Lude.Timestamp) (\s a -> s {createTimestamp = a} :: DeleteApplication)
{-# DEPRECATED dCreateTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead." #-}

instance Lude.AWSRequest DeleteApplication where
  type Rs DeleteApplication = DeleteApplicationResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("KinesisAnalytics_20150814.DeleteApplication" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just ("CreateTimestamp" Lude..= createTimestamp)
          ]
      )

instance Lude.ToPath DeleteApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplication where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteApplicationResponse' smart constructor.
newtype DeleteApplicationResponse = DeleteApplicationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationResponse
mkDeleteApplicationResponse pResponseStatus_ =
  DeleteApplicationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteApplicationResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
