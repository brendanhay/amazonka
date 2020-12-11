{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two users having different @IdentityId@ s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (@SourceUserIdentifier@ ) with the @IdentityId@ of the @DestinationUserIdentifier@ . Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.
--
-- The number of linked logins is limited to 20. So, the number of linked logins for the source user, @SourceUserIdentifier@ , and the destination user, @DestinationUserIdentifier@ , together should not be larger than 20. Otherwise, an exception will be thrown.
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
  ( -- * Creating a request
    MergeDeveloperIdentities (..),
    mkMergeDeveloperIdentities,

    -- ** Request lenses
    mdiSourceUserIdentifier,
    mdiDestinationUserIdentifier,
    mdiDeveloperProviderName,
    mdiIdentityPoolId,

    -- * Destructuring the response
    MergeDeveloperIdentitiesResponse (..),
    mkMergeDeveloperIdentitiesResponse,

    -- ** Response lenses
    mdirsIdentityId,
    mdirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mkMergeDeveloperIdentities' smart constructor.
data MergeDeveloperIdentities = MergeDeveloperIdentities'
  { sourceUserIdentifier ::
      Lude.Text,
    destinationUserIdentifier :: Lude.Text,
    developerProviderName :: Lude.Text,
    identityPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeDeveloperIdentities' with the minimum fields required to make a request.
--
-- * 'destinationUserIdentifier' - User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
-- * 'developerProviderName' - The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'sourceUserIdentifier' - User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
mkMergeDeveloperIdentities ::
  -- | 'sourceUserIdentifier'
  Lude.Text ->
  -- | 'destinationUserIdentifier'
  Lude.Text ->
  -- | 'developerProviderName'
  Lude.Text ->
  -- | 'identityPoolId'
  Lude.Text ->
  MergeDeveloperIdentities
mkMergeDeveloperIdentities
  pSourceUserIdentifier_
  pDestinationUserIdentifier_
  pDeveloperProviderName_
  pIdentityPoolId_ =
    MergeDeveloperIdentities'
      { sourceUserIdentifier =
          pSourceUserIdentifier_,
        destinationUserIdentifier = pDestinationUserIdentifier_,
        developerProviderName = pDeveloperProviderName_,
        identityPoolId = pIdentityPoolId_
      }

-- | User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
--
-- /Note:/ Consider using 'sourceUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiSourceUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Lude.Text
mdiSourceUserIdentifier = Lens.lens (sourceUserIdentifier :: MergeDeveloperIdentities -> Lude.Text) (\s a -> s {sourceUserIdentifier = a} :: MergeDeveloperIdentities)
{-# DEPRECATED mdiSourceUserIdentifier "Use generic-lens or generic-optics with 'sourceUserIdentifier' instead." #-}

-- | User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
--
-- /Note:/ Consider using 'destinationUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDestinationUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Lude.Text
mdiDestinationUserIdentifier = Lens.lens (destinationUserIdentifier :: MergeDeveloperIdentities -> Lude.Text) (\s a -> s {destinationUserIdentifier = a} :: MergeDeveloperIdentities)
{-# DEPRECATED mdiDestinationUserIdentifier "Use generic-lens or generic-optics with 'destinationUserIdentifier' instead." #-}

-- | The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiDeveloperProviderName :: Lens.Lens' MergeDeveloperIdentities Lude.Text
mdiDeveloperProviderName = Lens.lens (developerProviderName :: MergeDeveloperIdentities -> Lude.Text) (\s a -> s {developerProviderName = a} :: MergeDeveloperIdentities)
{-# DEPRECATED mdiDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdiIdentityPoolId :: Lens.Lens' MergeDeveloperIdentities Lude.Text
mdiIdentityPoolId = Lens.lens (identityPoolId :: MergeDeveloperIdentities -> Lude.Text) (\s a -> s {identityPoolId = a} :: MergeDeveloperIdentities)
{-# DEPRECATED mdiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest MergeDeveloperIdentities where
  type Rs MergeDeveloperIdentities = MergeDeveloperIdentitiesResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          MergeDeveloperIdentitiesResponse'
            Lude.<$> (x Lude..?> "IdentityId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MergeDeveloperIdentities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.MergeDeveloperIdentities" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MergeDeveloperIdentities where
  toJSON MergeDeveloperIdentities' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SourceUserIdentifier" Lude..= sourceUserIdentifier),
            Lude.Just
              ("DestinationUserIdentifier" Lude..= destinationUserIdentifier),
            Lude.Just ("DeveloperProviderName" Lude..= developerProviderName),
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId)
          ]
      )

instance Lude.ToPath MergeDeveloperIdentities where
  toPath = Lude.const "/"

instance Lude.ToQuery MergeDeveloperIdentities where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @MergeDeveloperIdentities@ action.
--
-- /See:/ 'mkMergeDeveloperIdentitiesResponse' smart constructor.
data MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse'
  { identityId ::
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

-- | Creates a value of 'MergeDeveloperIdentitiesResponse' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'responseStatus' - The response status code.
mkMergeDeveloperIdentitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MergeDeveloperIdentitiesResponse
mkMergeDeveloperIdentitiesResponse pResponseStatus_ =
  MergeDeveloperIdentitiesResponse'
    { identityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirsIdentityId :: Lens.Lens' MergeDeveloperIdentitiesResponse (Lude.Maybe Lude.Text)
mdirsIdentityId = Lens.lens (identityId :: MergeDeveloperIdentitiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: MergeDeveloperIdentitiesResponse)
{-# DEPRECATED mdirsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdirsResponseStatus :: Lens.Lens' MergeDeveloperIdentitiesResponse Lude.Int
mdirsResponseStatus = Lens.lens (responseStatus :: MergeDeveloperIdentitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MergeDeveloperIdentitiesResponse)
{-# DEPRECATED mdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
