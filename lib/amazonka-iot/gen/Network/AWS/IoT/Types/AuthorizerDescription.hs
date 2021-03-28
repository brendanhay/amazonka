{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuthorizerDescription
  ( AuthorizerDescription (..)
  -- * Smart constructor
  , mkAuthorizerDescription
  -- * Lenses
  , adAuthorizerArn
  , adAuthorizerFunctionArn
  , adAuthorizerName
  , adCreationDate
  , adLastModifiedDate
  , adSigningDisabled
  , adStatus
  , adTokenKeyName
  , adTokenSigningPublicKeys
  ) where

import qualified Network.AWS.IoT.Types.AuthorizerArn as Types
import qualified Network.AWS.IoT.Types.AuthorizerFunctionArn as Types
import qualified Network.AWS.IoT.Types.AuthorizerName as Types
import qualified Network.AWS.IoT.Types.AuthorizerStatus as Types
import qualified Network.AWS.IoT.Types.KeyName as Types
import qualified Network.AWS.IoT.Types.KeyValue as Types
import qualified Network.AWS.IoT.Types.TokenKeyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorizer description.
--
-- /See:/ 'mkAuthorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { authorizerArn :: Core.Maybe Types.AuthorizerArn
    -- ^ The authorizer ARN.
  , authorizerFunctionArn :: Core.Maybe Types.AuthorizerFunctionArn
    -- ^ The authorizer's Lambda function ARN.
  , authorizerName :: Core.Maybe Types.AuthorizerName
    -- ^ The authorizer name.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The UNIX timestamp of when the authorizer was created.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The UNIX timestamp of when the authorizer was last updated.
  , signingDisabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether AWS IoT validates the token signature in an authorization request.
  , status :: Core.Maybe Types.AuthorizerStatus
    -- ^ The status of the authorizer.
  , tokenKeyName :: Core.Maybe Types.TokenKeyName
    -- ^ The key used to extract the token from the HTTP headers.
  , tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue)
    -- ^ The public keys used to validate the token signature returned by your custom authentication service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AuthorizerDescription' value with any optional fields omitted.
mkAuthorizerDescription
    :: AuthorizerDescription
mkAuthorizerDescription
  = AuthorizerDescription'{authorizerArn = Core.Nothing,
                           authorizerFunctionArn = Core.Nothing,
                           authorizerName = Core.Nothing, creationDate = Core.Nothing,
                           lastModifiedDate = Core.Nothing, signingDisabled = Core.Nothing,
                           status = Core.Nothing, tokenKeyName = Core.Nothing,
                           tokenSigningPublicKeys = Core.Nothing}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerArn :: Lens.Lens' AuthorizerDescription (Core.Maybe Types.AuthorizerArn)
adAuthorizerArn = Lens.field @"authorizerArn"
{-# INLINEABLE adAuthorizerArn #-}
{-# DEPRECATED authorizerArn "Use generic-lens or generic-optics with 'authorizerArn' instead"  #-}

-- | The authorizer's Lambda function ARN.
--
-- /Note:/ Consider using 'authorizerFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerFunctionArn :: Lens.Lens' AuthorizerDescription (Core.Maybe Types.AuthorizerFunctionArn)
adAuthorizerFunctionArn = Lens.field @"authorizerFunctionArn"
{-# INLINEABLE adAuthorizerFunctionArn #-}
{-# DEPRECATED authorizerFunctionArn "Use generic-lens or generic-optics with 'authorizerFunctionArn' instead"  #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizerName :: Lens.Lens' AuthorizerDescription (Core.Maybe Types.AuthorizerName)
adAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE adAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

-- | The UNIX timestamp of when the authorizer was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adCreationDate :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.NominalDiffTime)
adCreationDate = Lens.field @"creationDate"
{-# INLINEABLE adCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The UNIX timestamp of when the authorizer was last updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastModifiedDate :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.NominalDiffTime)
adLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE adLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- /Note:/ Consider using 'signingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSigningDisabled :: Lens.Lens' AuthorizerDescription (Core.Maybe Core.Bool)
adSigningDisabled = Lens.field @"signingDisabled"
{-# INLINEABLE adSigningDisabled #-}
{-# DEPRECATED signingDisabled "Use generic-lens or generic-optics with 'signingDisabled' instead"  #-}

-- | The status of the authorizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStatus :: Lens.Lens' AuthorizerDescription (Core.Maybe Types.AuthorizerStatus)
adStatus = Lens.field @"status"
{-# INLINEABLE adStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTokenKeyName :: Lens.Lens' AuthorizerDescription (Core.Maybe Types.TokenKeyName)
adTokenKeyName = Lens.field @"tokenKeyName"
{-# INLINEABLE adTokenKeyName #-}
{-# DEPRECATED tokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead"  #-}

-- | The public keys used to validate the token signature returned by your custom authentication service.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTokenSigningPublicKeys :: Lens.Lens' AuthorizerDescription (Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue))
adTokenSigningPublicKeys = Lens.field @"tokenSigningPublicKeys"
{-# INLINEABLE adTokenSigningPublicKeys #-}
{-# DEPRECATED tokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead"  #-}

instance Core.FromJSON AuthorizerDescription where
        parseJSON
          = Core.withObject "AuthorizerDescription" Core.$
              \ x ->
                AuthorizerDescription' Core.<$>
                  (x Core..:? "authorizerArn") Core.<*>
                    x Core..:? "authorizerFunctionArn"
                    Core.<*> x Core..:? "authorizerName"
                    Core.<*> x Core..:? "creationDate"
                    Core.<*> x Core..:? "lastModifiedDate"
                    Core.<*> x Core..:? "signingDisabled"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "tokenKeyName"
                    Core.<*> x Core..:? "tokenSigningPublicKeys"
