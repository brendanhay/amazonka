{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.Credentials
  ( Credentials (..),

    -- * Smart constructor
    mkCredentials,

    -- * Lenses
    cSessionToken,
    cExpiration,
    cSecretKey,
    cAccessKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Credentials for the provided identity ID.
--
-- /See:/ 'mkCredentials' smart constructor.
data Credentials = Credentials'
  { sessionToken ::
      Lude.Maybe Lude.Text,
    expiration :: Lude.Maybe Lude.Timestamp,
    secretKey :: Lude.Maybe Lude.Text,
    accessKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The Access Key portion of the credentials.
-- * 'expiration' - The date at which these credentials will expire.
-- * 'secretKey' - The Secret Access Key portion of the credentials
-- * 'sessionToken' - The Session Token portion of the credentials
mkCredentials ::
  Credentials
mkCredentials =
  Credentials'
    { sessionToken = Lude.Nothing,
      expiration = Lude.Nothing,
      secretKey = Lude.Nothing,
      accessKeyId = Lude.Nothing
    }

-- | The Session Token portion of the credentials
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSessionToken :: Lens.Lens' Credentials (Lude.Maybe Lude.Text)
cSessionToken = Lens.lens (sessionToken :: Credentials -> Lude.Maybe Lude.Text) (\s a -> s {sessionToken = a} :: Credentials)
{-# DEPRECATED cSessionToken "Use generic-lens or generic-optics with 'sessionToken' instead." #-}

-- | The date at which these credentials will expire.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiration :: Lens.Lens' Credentials (Lude.Maybe Lude.Timestamp)
cExpiration = Lens.lens (expiration :: Credentials -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiration = a} :: Credentials)
{-# DEPRECATED cExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The Secret Access Key portion of the credentials
--
-- /Note:/ Consider using 'secretKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecretKey :: Lens.Lens' Credentials (Lude.Maybe Lude.Text)
cSecretKey = Lens.lens (secretKey :: Credentials -> Lude.Maybe Lude.Text) (\s a -> s {secretKey = a} :: Credentials)
{-# DEPRECATED cSecretKey "Use generic-lens or generic-optics with 'secretKey' instead." #-}

-- | The Access Key portion of the credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessKeyId :: Lens.Lens' Credentials (Lude.Maybe Lude.Text)
cAccessKeyId = Lens.lens (accessKeyId :: Credentials -> Lude.Maybe Lude.Text) (\s a -> s {accessKeyId = a} :: Credentials)
{-# DEPRECATED cAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.FromJSON Credentials where
  parseJSON =
    Lude.withObject
      "Credentials"
      ( \x ->
          Credentials'
            Lude.<$> (x Lude..:? "SessionToken")
            Lude.<*> (x Lude..:? "Expiration")
            Lude.<*> (x Lude..:? "SecretKey")
            Lude.<*> (x Lude..:? "AccessKeyId")
      )
