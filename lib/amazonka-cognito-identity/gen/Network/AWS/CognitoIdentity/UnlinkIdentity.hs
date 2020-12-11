{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a federated identity from an existing account. Unlinked logins will be considered new identities next time they are seen. Removing the last linked login will make this identity inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.UnlinkIdentity
  ( -- * Creating a request
    UnlinkIdentity (..),
    mkUnlinkIdentity,

    -- ** Request lenses
    uiIdentityId,
    uiLogins,
    uiLoginsToRemove,

    -- * Destructuring the response
    UnlinkIdentityResponse (..),
    mkUnlinkIdentityResponse,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the UnlinkIdentity action.
--
-- /See:/ 'mkUnlinkIdentity' smart constructor.
data UnlinkIdentity = UnlinkIdentity'
  { identityId :: Lude.Text,
    logins :: Lude.HashMap Lude.Text (Lude.Text),
    loginsToRemove :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnlinkIdentity' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'logins' - A set of optional name-value pairs that map provider names to provider tokens.
-- * 'loginsToRemove' - Provider names to unlink from this identity.
mkUnlinkIdentity ::
  -- | 'identityId'
  Lude.Text ->
  UnlinkIdentity
mkUnlinkIdentity pIdentityId_ =
  UnlinkIdentity'
    { identityId = pIdentityId_,
      logins = Lude.mempty,
      loginsToRemove = Lude.mempty
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiIdentityId :: Lens.Lens' UnlinkIdentity Lude.Text
uiIdentityId = Lens.lens (identityId :: UnlinkIdentity -> Lude.Text) (\s a -> s {identityId = a} :: UnlinkIdentity)
{-# DEPRECATED uiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiLogins :: Lens.Lens' UnlinkIdentity (Lude.HashMap Lude.Text (Lude.Text))
uiLogins = Lens.lens (logins :: UnlinkIdentity -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {logins = a} :: UnlinkIdentity)
{-# DEPRECATED uiLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | Provider names to unlink from this identity.
--
-- /Note:/ Consider using 'loginsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiLoginsToRemove :: Lens.Lens' UnlinkIdentity [Lude.Text]
uiLoginsToRemove = Lens.lens (loginsToRemove :: UnlinkIdentity -> [Lude.Text]) (\s a -> s {loginsToRemove = a} :: UnlinkIdentity)
{-# DEPRECATED uiLoginsToRemove "Use generic-lens or generic-optics with 'loginsToRemove' instead." #-}

instance Lude.AWSRequest UnlinkIdentity where
  type Rs UnlinkIdentity = UnlinkIdentityResponse
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveNull UnlinkIdentityResponse'

instance Lude.ToHeaders UnlinkIdentity where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.UnlinkIdentity" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnlinkIdentity where
  toJSON UnlinkIdentity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdentityId" Lude..= identityId),
            Lude.Just ("Logins" Lude..= logins),
            Lude.Just ("LoginsToRemove" Lude..= loginsToRemove)
          ]
      )

instance Lude.ToPath UnlinkIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery UnlinkIdentity where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse = UnlinkIdentityResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnlinkIdentityResponse' with the minimum fields required to make a request.
mkUnlinkIdentityResponse ::
  UnlinkIdentityResponse
mkUnlinkIdentityResponse = UnlinkIdentityResponse'
