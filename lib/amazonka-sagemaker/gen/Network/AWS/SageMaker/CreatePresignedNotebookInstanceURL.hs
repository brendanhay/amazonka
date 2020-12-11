{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a URL that you can use to connect to the Jupyter server from a notebook instance. In the Amazon SageMaker console, when you choose @Open@ next to a notebook instance, Amazon SageMaker opens a new tab showing the Jupyter server home page from the notebook instance. The console uses this API to get the URL and show the page.
--
-- The IAM role or user used to call this API defines the permissions to access the notebook instance. Once the presigned URL is created, no additional permission is required to access this URL. IAM authorization policies for this API are also enforced for every HTTP request and WebSocket frame that attempts to connect to the notebook instance.
-- You can restrict access to this API and to the URL that it returns to a list of IP addresses that you specify. Use the @NotIpAddress@ condition operator and the @aws:SourceIP@ condition context key to specify the list of IP addresses that you want to have access to the notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/security_iam_id-based-policy-examples.html#nbi-ip-filter Limit Access to a Notebook Instance by IP Address> .
module Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
  ( -- * Creating a request
    CreatePresignedNotebookInstanceURL (..),
    mkCreatePresignedNotebookInstanceURL,

    -- ** Request lenses
    cpniuSessionExpirationDurationInSeconds,
    cpniuNotebookInstanceName,

    -- * Destructuring the response
    CreatePresignedNotebookInstanceURLResponse (..),
    mkCreatePresignedNotebookInstanceURLResponse,

    -- ** Response lenses
    cpniursAuthorizedURL,
    cpniursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreatePresignedNotebookInstanceURL' smart constructor.
data CreatePresignedNotebookInstanceURL = CreatePresignedNotebookInstanceURL'
  { sessionExpirationDurationInSeconds ::
      Lude.Maybe
        Lude.Natural,
    notebookInstanceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePresignedNotebookInstanceURL' with the minimum fields required to make a request.
--
-- * 'notebookInstanceName' - The name of the notebook instance.
-- * 'sessionExpirationDurationInSeconds' - The duration of the session, in seconds. The default is 12 hours.
mkCreatePresignedNotebookInstanceURL ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  CreatePresignedNotebookInstanceURL
mkCreatePresignedNotebookInstanceURL pNotebookInstanceName_ =
  CreatePresignedNotebookInstanceURL'
    { sessionExpirationDurationInSeconds =
        Lude.Nothing,
      notebookInstanceName = pNotebookInstanceName_
    }

-- | The duration of the session, in seconds. The default is 12 hours.
--
-- /Note:/ Consider using 'sessionExpirationDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniuSessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedNotebookInstanceURL (Lude.Maybe Lude.Natural)
cpniuSessionExpirationDurationInSeconds = Lens.lens (sessionExpirationDurationInSeconds :: CreatePresignedNotebookInstanceURL -> Lude.Maybe Lude.Natural) (\s a -> s {sessionExpirationDurationInSeconds = a} :: CreatePresignedNotebookInstanceURL)
{-# DEPRECATED cpniuSessionExpirationDurationInSeconds "Use generic-lens or generic-optics with 'sessionExpirationDurationInSeconds' instead." #-}

-- | The name of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniuNotebookInstanceName :: Lens.Lens' CreatePresignedNotebookInstanceURL Lude.Text
cpniuNotebookInstanceName = Lens.lens (notebookInstanceName :: CreatePresignedNotebookInstanceURL -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: CreatePresignedNotebookInstanceURL)
{-# DEPRECATED cpniuNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

instance Lude.AWSRequest CreatePresignedNotebookInstanceURL where
  type
    Rs CreatePresignedNotebookInstanceURL =
      CreatePresignedNotebookInstanceURLResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePresignedNotebookInstanceURLResponse'
            Lude.<$> (x Lude..?> "AuthorizedUrl")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePresignedNotebookInstanceURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SageMaker.CreatePresignedNotebookInstanceUrl" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePresignedNotebookInstanceURL where
  toJSON CreatePresignedNotebookInstanceURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SessionExpirationDurationInSeconds" Lude..=)
              Lude.<$> sessionExpirationDurationInSeconds,
            Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName)
          ]
      )

instance Lude.ToPath CreatePresignedNotebookInstanceURL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePresignedNotebookInstanceURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePresignedNotebookInstanceURLResponse' smart constructor.
data CreatePresignedNotebookInstanceURLResponse = CreatePresignedNotebookInstanceURLResponse'
  { authorizedURL ::
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

-- | Creates a value of 'CreatePresignedNotebookInstanceURLResponse' with the minimum fields required to make a request.
--
-- * 'authorizedURL' - A JSON object that contains the URL string.
-- * 'responseStatus' - The response status code.
mkCreatePresignedNotebookInstanceURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePresignedNotebookInstanceURLResponse
mkCreatePresignedNotebookInstanceURLResponse pResponseStatus_ =
  CreatePresignedNotebookInstanceURLResponse'
    { authorizedURL =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON object that contains the URL string.
--
-- /Note:/ Consider using 'authorizedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniursAuthorizedURL :: Lens.Lens' CreatePresignedNotebookInstanceURLResponse (Lude.Maybe Lude.Text)
cpniursAuthorizedURL = Lens.lens (authorizedURL :: CreatePresignedNotebookInstanceURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizedURL = a} :: CreatePresignedNotebookInstanceURLResponse)
{-# DEPRECATED cpniursAuthorizedURL "Use generic-lens or generic-optics with 'authorizedURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpniursResponseStatus :: Lens.Lens' CreatePresignedNotebookInstanceURLResponse Lude.Int
cpniursResponseStatus = Lens.lens (responseStatus :: CreatePresignedNotebookInstanceURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePresignedNotebookInstanceURLResponse)
{-# DEPRECATED cpniursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
