{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Credentials
  ( Credentials (..)
  -- * Smart constructor
  , mkCredentials
  -- * Lenses
  , cAccessKeyId
  , cExpiration
  , cSecretKey
  , cSessionToken
  ) where

import qualified Network.AWS.CognitoIdentity.Types.AccessKeyId as Types
import qualified Network.AWS.CognitoIdentity.Types.SecretKeyString as Types
import qualified Network.AWS.CognitoIdentity.Types.SessionToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Credentials for the provided identity ID.
--
-- /See:/ 'mkCredentials' smart constructor.
data Credentials = Credentials'
  { accessKeyId :: Core.Maybe Types.AccessKeyId
    -- ^ The Access Key portion of the credentials.
  , expiration :: Core.Maybe Core.NominalDiffTime
    -- ^ The date at which these credentials will expire.
  , secretKey :: Core.Maybe Types.SecretKeyString
    -- ^ The Secret Access Key portion of the credentials
  , sessionToken :: Core.Maybe Types.SessionToken
    -- ^ The Session Token portion of the credentials
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Credentials' value with any optional fields omitted.
mkCredentials
    :: Credentials
mkCredentials
  = Credentials'{accessKeyId = Core.Nothing,
                 expiration = Core.Nothing, secretKey = Core.Nothing,
                 sessionToken = Core.Nothing}

-- | The Access Key portion of the credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessKeyId :: Lens.Lens' Credentials (Core.Maybe Types.AccessKeyId)
cAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE cAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | The date at which these credentials will expire.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiration :: Lens.Lens' Credentials (Core.Maybe Core.NominalDiffTime)
cExpiration = Lens.field @"expiration"
{-# INLINEABLE cExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | The Secret Access Key portion of the credentials
--
-- /Note:/ Consider using 'secretKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSecretKey :: Lens.Lens' Credentials (Core.Maybe Types.SecretKeyString)
cSecretKey = Lens.field @"secretKey"
{-# INLINEABLE cSecretKey #-}
{-# DEPRECATED secretKey "Use generic-lens or generic-optics with 'secretKey' instead"  #-}

-- | The Session Token portion of the credentials
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSessionToken :: Lens.Lens' Credentials (Core.Maybe Types.SessionToken)
cSessionToken = Lens.field @"sessionToken"
{-# INLINEABLE cSessionToken #-}
{-# DEPRECATED sessionToken "Use generic-lens or generic-optics with 'sessionToken' instead"  #-}

instance Core.FromJSON Credentials where
        parseJSON
          = Core.withObject "Credentials" Core.$
              \ x ->
                Credentials' Core.<$>
                  (x Core..:? "AccessKeyId") Core.<*> x Core..:? "Expiration"
                    Core.<*> x Core..:? "SecretKey"
                    Core.<*> x Core..:? "SessionToken"
