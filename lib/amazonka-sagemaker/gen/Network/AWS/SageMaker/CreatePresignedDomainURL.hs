{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreatePresignedDomainURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL for a specified UserProfile in a Domain. When accessed in a web browser, the user will be automatically signed in to Amazon SageMaker Studio, and granted access to all of the Apps and files associated with the Domain's Amazon Elastic File System (EFS) volume. This operation can only be called when the authentication mode equals IAM.
module Network.AWS.SageMaker.CreatePresignedDomainURL
  ( -- * Creating a request
    CreatePresignedDomainURL (..),
    mkCreatePresignedDomainURL,

    -- ** Request lenses
    cpduSessionExpirationDurationInSeconds,
    cpduDomainId,
    cpduUserProfileName,

    -- * Destructuring the response
    CreatePresignedDomainURLResponse (..),
    mkCreatePresignedDomainURLResponse,

    -- ** Response lenses
    cpdursAuthorizedURL,
    cpdursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreatePresignedDomainURL' smart constructor.
data CreatePresignedDomainURL = CreatePresignedDomainURL'
  { sessionExpirationDurationInSeconds ::
      Lude.Maybe Lude.Natural,
    domainId :: Lude.Text,
    userProfileName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePresignedDomainURL' with the minimum fields required to make a request.
--
-- * 'domainId' - The domain ID.
-- * 'sessionExpirationDurationInSeconds' - The session expiration duration in seconds.
-- * 'userProfileName' - The name of the UserProfile to sign-in as.
mkCreatePresignedDomainURL ::
  -- | 'domainId'
  Lude.Text ->
  -- | 'userProfileName'
  Lude.Text ->
  CreatePresignedDomainURL
mkCreatePresignedDomainURL pDomainId_ pUserProfileName_ =
  CreatePresignedDomainURL'
    { sessionExpirationDurationInSeconds =
        Lude.Nothing,
      domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | The session expiration duration in seconds.
--
-- /Note:/ Consider using 'sessionExpirationDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduSessionExpirationDurationInSeconds :: Lens.Lens' CreatePresignedDomainURL (Lude.Maybe Lude.Natural)
cpduSessionExpirationDurationInSeconds = Lens.lens (sessionExpirationDurationInSeconds :: CreatePresignedDomainURL -> Lude.Maybe Lude.Natural) (\s a -> s {sessionExpirationDurationInSeconds = a} :: CreatePresignedDomainURL)
{-# DEPRECATED cpduSessionExpirationDurationInSeconds "Use generic-lens or generic-optics with 'sessionExpirationDurationInSeconds' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduDomainId :: Lens.Lens' CreatePresignedDomainURL Lude.Text
cpduDomainId = Lens.lens (domainId :: CreatePresignedDomainURL -> Lude.Text) (\s a -> s {domainId = a} :: CreatePresignedDomainURL)
{-# DEPRECATED cpduDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The name of the UserProfile to sign-in as.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpduUserProfileName :: Lens.Lens' CreatePresignedDomainURL Lude.Text
cpduUserProfileName = Lens.lens (userProfileName :: CreatePresignedDomainURL -> Lude.Text) (\s a -> s {userProfileName = a} :: CreatePresignedDomainURL)
{-# DEPRECATED cpduUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

instance Lude.AWSRequest CreatePresignedDomainURL where
  type Rs CreatePresignedDomainURL = CreatePresignedDomainURLResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePresignedDomainURLResponse'
            Lude.<$> (x Lude..?> "AuthorizedUrl")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePresignedDomainURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreatePresignedDomainUrl" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePresignedDomainURL where
  toJSON CreatePresignedDomainURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SessionExpirationDurationInSeconds" Lude..=)
              Lude.<$> sessionExpirationDurationInSeconds,
            Lude.Just ("DomainId" Lude..= domainId),
            Lude.Just ("UserProfileName" Lude..= userProfileName)
          ]
      )

instance Lude.ToPath CreatePresignedDomainURL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePresignedDomainURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePresignedDomainURLResponse' smart constructor.
data CreatePresignedDomainURLResponse = CreatePresignedDomainURLResponse'
  { authorizedURL ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreatePresignedDomainURLResponse' with the minimum fields required to make a request.
--
-- * 'authorizedURL' - The presigned URL.
-- * 'responseStatus' - The response status code.
mkCreatePresignedDomainURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePresignedDomainURLResponse
mkCreatePresignedDomainURLResponse pResponseStatus_ =
  CreatePresignedDomainURLResponse'
    { authorizedURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The presigned URL.
--
-- /Note:/ Consider using 'authorizedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdursAuthorizedURL :: Lens.Lens' CreatePresignedDomainURLResponse (Lude.Maybe Lude.Text)
cpdursAuthorizedURL = Lens.lens (authorizedURL :: CreatePresignedDomainURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizedURL = a} :: CreatePresignedDomainURLResponse)
{-# DEPRECATED cpdursAuthorizedURL "Use generic-lens or generic-optics with 'authorizedURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdursResponseStatus :: Lens.Lens' CreatePresignedDomainURLResponse Lude.Int
cpdursResponseStatus = Lens.lens (responseStatus :: CreatePresignedDomainURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePresignedDomainURLResponse)
{-# DEPRECATED cpdursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
