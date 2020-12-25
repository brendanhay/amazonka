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
    cAccessKeyId,
    cExpiration,
    cSecretKey,
    cSessionToken,
  )
where

import qualified Network.AWS.CognitoIdentity.Types.AccessKeyId as Types
import qualified Network.AWS.CognitoIdentity.Types.SecretKeyString as Types
import qualified Network.AWS.CognitoIdentity.Types.SessionToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Credentials for the provided identity ID.
--
-- /See:/ 'mkCredentials' smart constructor.
data Credentials = Credentials'
  { -- | The Access Key portion of the credentials.
    accessKeyId :: Core.Maybe Types.AccessKeyId,
    -- | The date at which these credentials will expire.
    expiration :: Core.Maybe Core.NominalDiffTime,
    -- | The Secret Access Key portion of the credentials
    secretKey :: Core.Maybe Types.SecretKeyString,
    -- | The Session Token portion of the credentials
    sessionToken :: Core.Maybe Types.SessionToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Credentials' value with any optional fields omitted.
mkCredentials ::
  Credentials
mkCredentials =
  Credentials'
    { accessKeyId = Core.Nothing,
      expiration = Core.Nothing,
      secretKey = Core.Nothing,
      sessionToken = Core.Nothing
    }

-- | The Access Key portion of the credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessKeyId :: Lens.Lens' Credentials (Core.Maybe Types.AccessKeyId)
cAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED cAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The date at which these credentials will expire.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiration :: Lens.Lens' Credentials (Core.Maybe Core.NominalDiffTime)
cExpiration = Lens.field @"expiration"
{-# DEPRECATED cExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The Secret Access Key portion of the credentials
--
-- /Note:/ Consider using 'secretKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecretKey :: Lens.Lens' Credentials (Core.Maybe Types.SecretKeyString)
cSecretKey = Lens.field @"secretKey"
{-# DEPRECATED cSecretKey "Use generic-lens or generic-optics with 'secretKey' instead." #-}

-- | The Session Token portion of the credentials
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSessionToken :: Lens.Lens' Credentials (Core.Maybe Types.SessionToken)
cSessionToken = Lens.field @"sessionToken"
{-# DEPRECATED cSessionToken "Use generic-lens or generic-optics with 'sessionToken' instead." #-}

instance Core.FromJSON Credentials where
  parseJSON =
    Core.withObject "Credentials" Core.$
      \x ->
        Credentials'
          Core.<$> (x Core..:? "AccessKeyId")
          Core.<*> (x Core..:? "Expiration")
          Core.<*> (x Core..:? "SecretKey")
          Core.<*> (x Core..:? "SessionToken")
