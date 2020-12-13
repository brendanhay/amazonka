{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerDescription
  ( AuthorizerDescription (..),

    -- * Smart constructor
    mkAuthorizerDescription,

    -- * Lenses
    adStatus,
    adLastModifiedDate,
    adSigningDisabled,
    adAuthorizerName,
    adAuthorizerFunctionARN,
    adAuthorizerARN,
    adCreationDate,
    adTokenSigningPublicKeys,
    adTokenKeyName,
  )
where

import Network.AWS.IoT.Types.AuthorizerStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorizer description.
--
-- /See:/ 'mkAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { -- | The status of the authorizer.
    status :: Lude.Maybe AuthorizerStatus,
    -- | The UNIX timestamp of when the authorizer was last updated.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | Specifies whether AWS IoT validates the token signature in an authorization request.
    signingDisabled :: Lude.Maybe Lude.Bool,
    -- | The authorizer name.
    authorizerName :: Lude.Maybe Lude.Text,
    -- | The authorizer's Lambda function ARN.
    authorizerFunctionARN :: Lude.Maybe Lude.Text,
    -- | The authorizer ARN.
    authorizerARN :: Lude.Maybe Lude.Text,
    -- | The UNIX timestamp of when the authorizer was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The public keys used to validate the token signature returned by your custom authentication service.
    tokenSigningPublicKeys :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The key used to extract the token from the HTTP headers.
    tokenKeyName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizerDescription' with the minimum fields required to make a request.
--
-- * 'status' - The status of the authorizer.
-- * 'lastModifiedDate' - The UNIX timestamp of when the authorizer was last updated.
-- * 'signingDisabled' - Specifies whether AWS IoT validates the token signature in an authorization request.
-- * 'authorizerName' - The authorizer name.
-- * 'authorizerFunctionARN' - The authorizer's Lambda function ARN.
-- * 'authorizerARN' - The authorizer ARN.
-- * 'creationDate' - The UNIX timestamp of when the authorizer was created.
-- * 'tokenSigningPublicKeys' - The public keys used to validate the token signature returned by your custom authentication service.
-- * 'tokenKeyName' - The key used to extract the token from the HTTP headers.
mkAuthorizerDescription ::
  AuthorizerDescription
mkAuthorizerDescription =
  AuthorizerDescription'
    { status = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      signingDisabled = Lude.Nothing,
      authorizerName = Lude.Nothing,
      authorizerFunctionARN = Lude.Nothing,
      authorizerARN = Lude.Nothing,
      creationDate = Lude.Nothing,
      tokenSigningPublicKeys = Lude.Nothing,
      tokenKeyName = Lude.Nothing
    }

-- | The status of the authorizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatus :: Lens.Lens' AuthorizerDescription (Lude.Maybe AuthorizerStatus)
adStatus = Lens.lens (status :: AuthorizerDescription -> Lude.Maybe AuthorizerStatus) (\s a -> s {status = a} :: AuthorizerDescription)
{-# DEPRECATED adStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The UNIX timestamp of when the authorizer was last updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastModifiedDate :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Timestamp)
adLastModifiedDate = Lens.lens (lastModifiedDate :: AuthorizerDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: AuthorizerDescription)
{-# DEPRECATED adLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- /Note:/ Consider using 'signingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSigningDisabled :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Bool)
adSigningDisabled = Lens.lens (signingDisabled :: AuthorizerDescription -> Lude.Maybe Lude.Bool) (\s a -> s {signingDisabled = a} :: AuthorizerDescription)
{-# DEPRECATED adSigningDisabled "Use generic-lens or generic-optics with 'signingDisabled' instead." #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerName :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Text)
adAuthorizerName = Lens.lens (authorizerName :: AuthorizerDescription -> Lude.Maybe Lude.Text) (\s a -> s {authorizerName = a} :: AuthorizerDescription)
{-# DEPRECATED adAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The authorizer's Lambda function ARN.
--
-- /Note:/ Consider using 'authorizerFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerFunctionARN :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Text)
adAuthorizerFunctionARN = Lens.lens (authorizerFunctionARN :: AuthorizerDescription -> Lude.Maybe Lude.Text) (\s a -> s {authorizerFunctionARN = a} :: AuthorizerDescription)
{-# DEPRECATED adAuthorizerFunctionARN "Use generic-lens or generic-optics with 'authorizerFunctionARN' instead." #-}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerARN :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Text)
adAuthorizerARN = Lens.lens (authorizerARN :: AuthorizerDescription -> Lude.Maybe Lude.Text) (\s a -> s {authorizerARN = a} :: AuthorizerDescription)
{-# DEPRECATED adAuthorizerARN "Use generic-lens or generic-optics with 'authorizerARN' instead." #-}

-- | The UNIX timestamp of when the authorizer was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCreationDate :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Timestamp)
adCreationDate = Lens.lens (creationDate :: AuthorizerDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: AuthorizerDescription)
{-# DEPRECATED adCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The public keys used to validate the token signature returned by your custom authentication service.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
adTokenSigningPublicKeys = Lens.lens (tokenSigningPublicKeys :: AuthorizerDescription -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tokenSigningPublicKeys = a} :: AuthorizerDescription)
{-# DEPRECATED adTokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead." #-}

-- | The key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTokenKeyName :: Lens.Lens' AuthorizerDescription (Lude.Maybe Lude.Text)
adTokenKeyName = Lens.lens (tokenKeyName :: AuthorizerDescription -> Lude.Maybe Lude.Text) (\s a -> s {tokenKeyName = a} :: AuthorizerDescription)
{-# DEPRECATED adTokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead." #-}

instance Lude.FromJSON AuthorizerDescription where
  parseJSON =
    Lude.withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "signingDisabled")
            Lude.<*> (x Lude..:? "authorizerName")
            Lude.<*> (x Lude..:? "authorizerFunctionArn")
            Lude.<*> (x Lude..:? "authorizerArn")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "tokenSigningPublicKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "tokenKeyName")
      )
