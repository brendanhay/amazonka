{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an import task, which allows you to import details of your on-premises environment directly into AWS Migration Hub without having to use the Application Discovery Service (ADS) tools such as the Discovery Connector or Discovery Agent. This gives you the option to perform migration assessment and planning directly from your imported data, including the ability to group your devices as applications and track their migration status.
--
-- To start an import request, do this:
--
--     * Download the specially formatted comma separated value (CSV) import template, which you can find here: <https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv> .
--
--
--     * Fill out the template with your server and application data.
--
--
--     * Upload your import file to an Amazon S3 bucket, and make a note of it's Object URL. Your import file must be in the CSV format.
--
--
--     * Use the console or the @StartImportTask@ command with the AWS CLI or one of the AWS SDKs to import the records from your file.
--
--
-- For more information, including step-by-step procedures, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-import.html Migration Hub Import> in the /AWS Application Discovery Service User Guide/ .
module Network.AWS.Discovery.StartImportTask
  ( -- * Creating a request
    StartImportTask (..),
    mkStartImportTask,

    -- ** Request lenses
    sitName,
    sitClientRequestToken,
    sitImportURL,

    -- * Destructuring the response
    StartImportTaskResponse (..),
    mkStartImportTaskResponse,

    -- ** Response lenses
    sitrsTask,
    sitrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartImportTask' smart constructor.
data StartImportTask = StartImportTask'
  { -- | A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
    name :: Lude.Text,
    -- | Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated.
    --
    -- Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | The URL for your import file that you've uploaded to Amazon S3.
    importURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImportTask' with the minimum fields required to make a request.
--
-- * 'name' - A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
-- * 'clientRequestToken' - Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
-- * 'importURL' - The URL for your import file that you've uploaded to Amazon S3.
mkStartImportTask ::
  -- | 'name'
  Lude.Text ->
  -- | 'importURL'
  Lude.Text ->
  StartImportTask
mkStartImportTask pName_ pImportURL_ =
  StartImportTask'
    { name = pName_,
      clientRequestToken = Lude.Nothing,
      importURL = pImportURL_
    }

-- | A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitName :: Lens.Lens' StartImportTask Lude.Text
sitName = Lens.lens (name :: StartImportTask -> Lude.Text) (\s a -> s {name = a} :: StartImportTask)
{-# DEPRECATED sitName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitClientRequestToken :: Lens.Lens' StartImportTask (Lude.Maybe Lude.Text)
sitClientRequestToken = Lens.lens (clientRequestToken :: StartImportTask -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartImportTask)
{-# DEPRECATED sitClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The URL for your import file that you've uploaded to Amazon S3.
--
-- /Note:/ Consider using 'importURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitImportURL :: Lens.Lens' StartImportTask Lude.Text
sitImportURL = Lens.lens (importURL :: StartImportTask -> Lude.Text) (\s a -> s {importURL = a} :: StartImportTask)
{-# DEPRECATED sitImportURL "Use generic-lens or generic-optics with 'importURL' instead." #-}

instance Lude.AWSRequest StartImportTask where
  type Rs StartImportTask = StartImportTaskResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartImportTaskResponse'
            Lude.<$> (x Lude..?> "task") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartImportTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.StartImportTask" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartImportTask where
  toJSON StartImportTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            ("clientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("importUrl" Lude..= importURL)
          ]
      )

instance Lude.ToPath StartImportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StartImportTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartImportTaskResponse' smart constructor.
data StartImportTaskResponse = StartImportTaskResponse'
  { -- | An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
    task :: Lude.Maybe ImportTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartImportTaskResponse' with the minimum fields required to make a request.
--
-- * 'task' - An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
-- * 'responseStatus' - The response status code.
mkStartImportTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartImportTaskResponse
mkStartImportTaskResponse pResponseStatus_ =
  StartImportTaskResponse'
    { task = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitrsTask :: Lens.Lens' StartImportTaskResponse (Lude.Maybe ImportTask)
sitrsTask = Lens.lens (task :: StartImportTaskResponse -> Lude.Maybe ImportTask) (\s a -> s {task = a} :: StartImportTaskResponse)
{-# DEPRECATED sitrsTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitrsResponseStatus :: Lens.Lens' StartImportTaskResponse Lude.Int
sitrsResponseStatus = Lens.lens (responseStatus :: StartImportTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartImportTaskResponse)
{-# DEPRECATED sitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
