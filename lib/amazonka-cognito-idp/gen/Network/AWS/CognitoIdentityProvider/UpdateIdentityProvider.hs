{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates identity provider information for a user pool.
module Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
  ( -- * Creating a request
    UpdateIdentityProvider (..),
    mkUpdateIdentityProvider,

    -- ** Request lenses
    uipIdpIdentifiers,
    uipAttributeMapping,
    uipProviderDetails,
    uipUserPoolId,
    uipProviderName,

    -- * Destructuring the response
    UpdateIdentityProviderResponse (..),
    mkUpdateIdentityProviderResponse,

    -- ** Response lenses
    uiprsResponseStatus,
    uiprsIdentityProvider,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateIdentityProvider' smart constructor.
data UpdateIdentityProvider = UpdateIdentityProvider'
  { idpIdentifiers ::
      Lude.Maybe [Lude.Text],
    attributeMapping ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    providerDetails ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    userPoolId :: Lude.Text,
    providerName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIdentityProvider' with the minimum fields required to make a request.
--
-- * 'attributeMapping' - The identity provider attribute mapping to be changed.
-- * 'idpIdentifiers' - A list of identity provider identifiers.
-- * 'providerDetails' - The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
-- * 'providerName' - The identity provider name.
-- * 'userPoolId' - The user pool ID.
mkUpdateIdentityProvider ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'providerName'
  Lude.Text ->
  UpdateIdentityProvider
mkUpdateIdentityProvider pUserPoolId_ pProviderName_ =
  UpdateIdentityProvider'
    { idpIdentifiers = Lude.Nothing,
      attributeMapping = Lude.Nothing,
      providerDetails = Lude.Nothing,
      userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | A list of identity provider identifiers.
--
-- /Note:/ Consider using 'idpIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdpIdentifiers :: Lens.Lens' UpdateIdentityProvider (Lude.Maybe [Lude.Text])
uipIdpIdentifiers = Lens.lens (idpIdentifiers :: UpdateIdentityProvider -> Lude.Maybe [Lude.Text]) (\s a -> s {idpIdentifiers = a} :: UpdateIdentityProvider)
{-# DEPRECATED uipIdpIdentifiers "Use generic-lens or generic-optics with 'idpIdentifiers' instead." #-}

-- | The identity provider attribute mapping to be changed.
--
-- /Note:/ Consider using 'attributeMapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAttributeMapping :: Lens.Lens' UpdateIdentityProvider (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uipAttributeMapping = Lens.lens (attributeMapping :: UpdateIdentityProvider -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributeMapping = a} :: UpdateIdentityProvider)
{-# DEPRECATED uipAttributeMapping "Use generic-lens or generic-optics with 'attributeMapping' instead." #-}

-- | The identity provider details to be updated, such as @MetadataURL@ and @MetadataFile@ .
--
-- /Note:/ Consider using 'providerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderDetails :: Lens.Lens' UpdateIdentityProvider (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uipProviderDetails = Lens.lens (providerDetails :: UpdateIdentityProvider -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {providerDetails = a} :: UpdateIdentityProvider)
{-# DEPRECATED uipProviderDetails "Use generic-lens or generic-optics with 'providerDetails' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipUserPoolId :: Lens.Lens' UpdateIdentityProvider Lude.Text
uipUserPoolId = Lens.lens (userPoolId :: UpdateIdentityProvider -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateIdentityProvider)
{-# DEPRECATED uipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipProviderName :: Lens.Lens' UpdateIdentityProvider Lude.Text
uipProviderName = Lens.lens (providerName :: UpdateIdentityProvider -> Lude.Text) (\s a -> s {providerName = a} :: UpdateIdentityProvider)
{-# DEPRECATED uipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.AWSRequest UpdateIdentityProvider where
  type Rs UpdateIdentityProvider = UpdateIdentityProviderResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateIdentityProviderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "IdentityProvider")
      )

instance Lude.ToHeaders UpdateIdentityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateIdentityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateIdentityProvider where
  toJSON UpdateIdentityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdpIdentifiers" Lude..=) Lude.<$> idpIdentifiers,
            ("AttributeMapping" Lude..=) Lude.<$> attributeMapping,
            ("ProviderDetails" Lude..=) Lude.<$> providerDetails,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ProviderName" Lude..= providerName)
          ]
      )

instance Lude.ToPath UpdateIdentityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateIdentityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateIdentityProviderResponse' smart constructor.
data UpdateIdentityProviderResponse = UpdateIdentityProviderResponse'
  { responseStatus ::
      Lude.Int,
    identityProvider ::
      IdentityProviderType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIdentityProviderResponse' with the minimum fields required to make a request.
--
-- * 'identityProvider' - The identity provider object.
-- * 'responseStatus' - The response status code.
mkUpdateIdentityProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  UpdateIdentityProviderResponse
mkUpdateIdentityProviderResponse
  pResponseStatus_
  pIdentityProvider_ =
    UpdateIdentityProviderResponse'
      { responseStatus =
          pResponseStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprsResponseStatus :: Lens.Lens' UpdateIdentityProviderResponse Lude.Int
uiprsResponseStatus = Lens.lens (responseStatus :: UpdateIdentityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateIdentityProviderResponse)
{-# DEPRECATED uiprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The identity provider object.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiprsIdentityProvider :: Lens.Lens' UpdateIdentityProviderResponse IdentityProviderType
uiprsIdentityProvider = Lens.lens (identityProvider :: UpdateIdentityProviderResponse -> IdentityProviderType) (\s a -> s {identityProvider = a} :: UpdateIdentityProviderResponse)
{-# DEPRECATED uiprsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}
