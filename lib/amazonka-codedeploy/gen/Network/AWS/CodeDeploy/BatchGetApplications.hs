{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications. The maximum number of applications that can be returned is 100.
module Network.AWS.CodeDeploy.BatchGetApplications
  ( -- * Creating a request
    BatchGetApplications (..),
    mkBatchGetApplications,

    -- ** Request lenses
    bgaApplicationNames,

    -- * Destructuring the response
    BatchGetApplicationsResponse (..),
    mkBatchGetApplicationsResponse,

    -- ** Response lenses
    bgarsApplicationsInfo,
    bgarsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplications' smart constructor.
newtype BatchGetApplications = BatchGetApplications'
  { -- | A list of application names separated by spaces. The maximum number of application names you can specify is 100.
    applicationNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetApplications' with the minimum fields required to make a request.
--
-- * 'applicationNames' - A list of application names separated by spaces. The maximum number of application names you can specify is 100.
mkBatchGetApplications ::
  BatchGetApplications
mkBatchGetApplications =
  BatchGetApplications' {applicationNames = Lude.mempty}

-- | A list of application names separated by spaces. The maximum number of application names you can specify is 100.
--
-- /Note:/ Consider using 'applicationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgaApplicationNames :: Lens.Lens' BatchGetApplications [Lude.Text]
bgaApplicationNames = Lens.lens (applicationNames :: BatchGetApplications -> [Lude.Text]) (\s a -> s {applicationNames = a} :: BatchGetApplications)
{-# DEPRECATED bgaApplicationNames "Use generic-lens or generic-optics with 'applicationNames' instead." #-}

instance Lude.AWSRequest BatchGetApplications where
  type Rs BatchGetApplications = BatchGetApplicationsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetApplicationsResponse'
            Lude.<$> (x Lude..?> "applicationsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetApplications where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.BatchGetApplications" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetApplications where
  toJSON BatchGetApplications' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("applicationNames" Lude..= applicationNames)]
      )

instance Lude.ToPath BatchGetApplications where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetApplications where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetApplications@ operation.
--
-- /See:/ 'mkBatchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
  { -- | Information about the applications.
    applicationsInfo :: Lude.Maybe [ApplicationInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetApplicationsResponse' with the minimum fields required to make a request.
--
-- * 'applicationsInfo' - Information about the applications.
-- * 'responseStatus' - The response status code.
mkBatchGetApplicationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetApplicationsResponse
mkBatchGetApplicationsResponse pResponseStatus_ =
  BatchGetApplicationsResponse'
    { applicationsInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the applications.
--
-- /Note:/ Consider using 'applicationsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarsApplicationsInfo :: Lens.Lens' BatchGetApplicationsResponse (Lude.Maybe [ApplicationInfo])
bgarsApplicationsInfo = Lens.lens (applicationsInfo :: BatchGetApplicationsResponse -> Lude.Maybe [ApplicationInfo]) (\s a -> s {applicationsInfo = a} :: BatchGetApplicationsResponse)
{-# DEPRECATED bgarsApplicationsInfo "Use generic-lens or generic-optics with 'applicationsInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarsResponseStatus :: Lens.Lens' BatchGetApplicationsResponse Lude.Int
bgarsResponseStatus = Lens.lens (responseStatus :: BatchGetApplicationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetApplicationsResponse)
{-# DEPRECATED bgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
