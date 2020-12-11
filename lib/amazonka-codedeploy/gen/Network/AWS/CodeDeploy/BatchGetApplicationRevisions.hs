{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.BatchGetApplicationRevisions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more application revisions. The maximum number of application revisions that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetApplicationRevisions
  ( -- * Creating a request
    BatchGetApplicationRevisions (..),
    mkBatchGetApplicationRevisions,

    -- ** Request lenses
    bgarApplicationName,
    bgarRevisions,

    -- * Destructuring the response
    BatchGetApplicationRevisionsResponse (..),
    mkBatchGetApplicationRevisionsResponse,

    -- ** Response lenses
    bgarrsApplicationName,
    bgarrsRevisions,
    bgarrsErrorMessage,
    bgarrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisions' smart constructor.
data BatchGetApplicationRevisions = BatchGetApplicationRevisions'
  { applicationName ::
      Lude.Text,
    revisions :: [RevisionLocation]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetApplicationRevisions' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of an AWS CodeDeploy application about which to get revision information.
-- * 'revisions' - An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
mkBatchGetApplicationRevisions ::
  -- | 'applicationName'
  Lude.Text ->
  BatchGetApplicationRevisions
mkBatchGetApplicationRevisions pApplicationName_ =
  BatchGetApplicationRevisions'
    { applicationName =
        pApplicationName_,
      revisions = Lude.mempty
    }

-- | The name of an AWS CodeDeploy application about which to get revision information.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarApplicationName :: Lens.Lens' BatchGetApplicationRevisions Lude.Text
bgarApplicationName = Lens.lens (applicationName :: BatchGetApplicationRevisions -> Lude.Text) (\s a -> s {applicationName = a} :: BatchGetApplicationRevisions)
{-# DEPRECATED bgarApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | An array of @RevisionLocation@ objects that specify information to get about the application revisions, including type and location. The maximum number of @RevisionLocation@ objects you can specify is 25.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarRevisions :: Lens.Lens' BatchGetApplicationRevisions [RevisionLocation]
bgarRevisions = Lens.lens (revisions :: BatchGetApplicationRevisions -> [RevisionLocation]) (\s a -> s {revisions = a} :: BatchGetApplicationRevisions)
{-# DEPRECATED bgarRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

instance Lude.AWSRequest BatchGetApplicationRevisions where
  type
    Rs BatchGetApplicationRevisions =
      BatchGetApplicationRevisionsResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetApplicationRevisionsResponse'
            Lude.<$> (x Lude..?> "applicationName")
            Lude.<*> (x Lude..?> "revisions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "errorMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetApplicationRevisions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeDeploy_20141006.BatchGetApplicationRevisions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetApplicationRevisions where
  toJSON BatchGetApplicationRevisions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("applicationName" Lude..= applicationName),
            Lude.Just ("revisions" Lude..= revisions)
          ]
      )

instance Lude.ToPath BatchGetApplicationRevisions where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetApplicationRevisions where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @BatchGetApplicationRevisions@ operation.
--
-- /See:/ 'mkBatchGetApplicationRevisionsResponse' smart constructor.
data BatchGetApplicationRevisionsResponse = BatchGetApplicationRevisionsResponse'
  { applicationName ::
      Lude.Maybe
        Lude.Text,
    revisions ::
      Lude.Maybe
        [RevisionInfo],
    errorMessage ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetApplicationRevisionsResponse' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application that corresponds to the revisions.
-- * 'errorMessage' - Information about errors that might have occurred during the API call.
-- * 'responseStatus' - The response status code.
-- * 'revisions' - Additional information about the revisions, including the type and location.
mkBatchGetApplicationRevisionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetApplicationRevisionsResponse
mkBatchGetApplicationRevisionsResponse pResponseStatus_ =
  BatchGetApplicationRevisionsResponse'
    { applicationName =
        Lude.Nothing,
      revisions = Lude.Nothing,
      errorMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the application that corresponds to the revisions.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsApplicationName :: Lens.Lens' BatchGetApplicationRevisionsResponse (Lude.Maybe Lude.Text)
bgarrsApplicationName = Lens.lens (applicationName :: BatchGetApplicationRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: BatchGetApplicationRevisionsResponse)
{-# DEPRECATED bgarrsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Additional information about the revisions, including the type and location.
--
-- /Note:/ Consider using 'revisions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsRevisions :: Lens.Lens' BatchGetApplicationRevisionsResponse (Lude.Maybe [RevisionInfo])
bgarrsRevisions = Lens.lens (revisions :: BatchGetApplicationRevisionsResponse -> Lude.Maybe [RevisionInfo]) (\s a -> s {revisions = a} :: BatchGetApplicationRevisionsResponse)
{-# DEPRECATED bgarrsRevisions "Use generic-lens or generic-optics with 'revisions' instead." #-}

-- | Information about errors that might have occurred during the API call.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsErrorMessage :: Lens.Lens' BatchGetApplicationRevisionsResponse (Lude.Maybe Lude.Text)
bgarrsErrorMessage = Lens.lens (errorMessage :: BatchGetApplicationRevisionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchGetApplicationRevisionsResponse)
{-# DEPRECATED bgarrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgarrsResponseStatus :: Lens.Lens' BatchGetApplicationRevisionsResponse Lude.Int
bgarrsResponseStatus = Lens.lens (responseStatus :: BatchGetApplicationRevisionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetApplicationRevisionsResponse)
{-# DEPRECATED bgarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
