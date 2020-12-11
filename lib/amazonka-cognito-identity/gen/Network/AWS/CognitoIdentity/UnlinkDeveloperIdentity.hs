{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked developer users will be considered new identities next time they are seen. If, for a given Cognito identity, you remove all federated identities as well as the developer user identifier, the Cognito identity becomes inaccessible.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
  ( -- * Creating a request
    UnlinkDeveloperIdentity (..),
    mkUnlinkDeveloperIdentity,

    -- ** Request lenses
    udiIdentityId,
    udiIdentityPoolId,
    udiDeveloperProviderName,
    udiDeveloperUserIdentifier,

    -- * Destructuring the response
    UnlinkDeveloperIdentityResponse (..),
    mkUnlinkDeveloperIdentityResponse,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @UnlinkDeveloperIdentity@ action.
--
-- /See:/ 'mkUnlinkDeveloperIdentity' smart constructor.
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'
  { identityId ::
      Lude.Text,
    identityPoolId :: Lude.Text,
    developerProviderName :: Lude.Text,
    developerUserIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnlinkDeveloperIdentity' with the minimum fields required to make a request.
--
-- * 'developerProviderName' - The "domain" by which Cognito will refer to your users.
-- * 'developerUserIdentifier' - A unique ID used by your backend authentication process to identify a user.
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
mkUnlinkDeveloperIdentity ::
  -- | 'identityId'
  Lude.Text ->
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'developerProviderName'
  Lude.Text ->
  -- | 'developerUserIdentifier'
  Lude.Text ->
  UnlinkDeveloperIdentity
mkUnlinkDeveloperIdentity
  pIdentityId_
  pIdentityPoolId_
  pDeveloperProviderName_
  pDeveloperUserIdentifier_ =
    UnlinkDeveloperIdentity'
      { identityId = pIdentityId_,
        identityPoolId = pIdentityPoolId_,
        developerProviderName = pDeveloperProviderName_,
        developerUserIdentifier = pDeveloperUserIdentifier_
      }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiIdentityId :: Lens.Lens' UnlinkDeveloperIdentity Lude.Text
udiIdentityId = Lens.lens (identityId :: UnlinkDeveloperIdentity -> Lude.Text) (\s a -> s {identityId = a} :: UnlinkDeveloperIdentity)
{-# DEPRECATED udiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiIdentityPoolId :: Lens.Lens' UnlinkDeveloperIdentity Lude.Text
udiIdentityPoolId = Lens.lens (identityPoolId :: UnlinkDeveloperIdentity -> Lude.Text) (\s a -> s {identityPoolId = a} :: UnlinkDeveloperIdentity)
{-# DEPRECATED udiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | The "domain" by which Cognito will refer to your users.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiDeveloperProviderName :: Lens.Lens' UnlinkDeveloperIdentity Lude.Text
udiDeveloperProviderName = Lens.lens (developerProviderName :: UnlinkDeveloperIdentity -> Lude.Text) (\s a -> s {developerProviderName = a} :: UnlinkDeveloperIdentity)
{-# DEPRECATED udiDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | A unique ID used by your backend authentication process to identify a user.
--
-- /Note:/ Consider using 'developerUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udiDeveloperUserIdentifier :: Lens.Lens' UnlinkDeveloperIdentity Lude.Text
udiDeveloperUserIdentifier = Lens.lens (developerUserIdentifier :: UnlinkDeveloperIdentity -> Lude.Text) (\s a -> s {developerUserIdentifier = a} :: UnlinkDeveloperIdentity)
{-# DEPRECATED udiDeveloperUserIdentifier "Use generic-lens or generic-optics with 'developerUserIdentifier' instead." #-}

instance Lude.AWSRequest UnlinkDeveloperIdentity where
  type Rs UnlinkDeveloperIdentity = UnlinkDeveloperIdentityResponse
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveNull UnlinkDeveloperIdentityResponse'

instance Lude.ToHeaders UnlinkDeveloperIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.UnlinkDeveloperIdentity" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnlinkDeveloperIdentity where
  toJSON UnlinkDeveloperIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdentityId" Lude..= identityId),
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            Lude.Just ("DeveloperProviderName" Lude..= developerProviderName),
            Lude.Just
              ("DeveloperUserIdentifier" Lude..= developerUserIdentifier)
          ]
      )

instance Lude.ToPath UnlinkDeveloperIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery UnlinkDeveloperIdentity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnlinkDeveloperIdentityResponse' with the minimum fields required to make a request.
mkUnlinkDeveloperIdentityResponse ::
  UnlinkDeveloperIdentityResponse
mkUnlinkDeveloperIdentityResponse =
  UnlinkDeveloperIdentityResponse'
