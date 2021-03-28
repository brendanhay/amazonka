{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
  ( UICustomizationType (..)
  -- * Smart constructor
  , mkUICustomizationType
  -- * Lenses
  , uictCSS
  , uictCSSVersion
  , uictClientId
  , uictCreationDate
  , uictImageUrl
  , uictLastModifiedDate
  , uictUserPoolId
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.CSSType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CSSVersion as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientIdType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ImageUrlType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolIdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for the UI customization information for a user pool's built-in app UI.
--
-- /See:/ 'mkUICustomizationType' smart constructor.
data UICustomizationType = UICustomizationType'
  { css :: Core.Maybe Types.CSSType
    -- ^ The CSS values in the UI customization.
  , cSSVersion :: Core.Maybe Types.CSSVersion
    -- ^ The CSS version number.
  , clientId :: Core.Maybe Types.ClientIdType
    -- ^ The client ID for the client app.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date for the UI customization.
  , imageUrl :: Core.Maybe Types.ImageUrlType
    -- ^ The logo image for the UI customization.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The last-modified date for the UI customization.
  , userPoolId :: Core.Maybe Types.UserPoolIdType
    -- ^ The user pool ID for the user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UICustomizationType' value with any optional fields omitted.
mkUICustomizationType
    :: UICustomizationType
mkUICustomizationType
  = UICustomizationType'{css = Core.Nothing,
                         cSSVersion = Core.Nothing, clientId = Core.Nothing,
                         creationDate = Core.Nothing, imageUrl = Core.Nothing,
                         lastModifiedDate = Core.Nothing, userPoolId = Core.Nothing}

-- | The CSS values in the UI customization.
--
-- /Note:/ Consider using 'css' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCSS :: Lens.Lens' UICustomizationType (Core.Maybe Types.CSSType)
uictCSS = Lens.field @"css"
{-# INLINEABLE uictCSS #-}
{-# DEPRECATED css "Use generic-lens or generic-optics with 'css' instead"  #-}

-- | The CSS version number.
--
-- /Note:/ Consider using 'cSSVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCSSVersion :: Lens.Lens' UICustomizationType (Core.Maybe Types.CSSVersion)
uictCSSVersion = Lens.field @"cSSVersion"
{-# INLINEABLE uictCSSVersion #-}
{-# DEPRECATED cSSVersion "Use generic-lens or generic-optics with 'cSSVersion' instead"  #-}

-- | The client ID for the client app.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictClientId :: Lens.Lens' UICustomizationType (Core.Maybe Types.ClientIdType)
uictClientId = Lens.field @"clientId"
{-# INLINEABLE uictClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The creation date for the UI customization.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCreationDate :: Lens.Lens' UICustomizationType (Core.Maybe Core.NominalDiffTime)
uictCreationDate = Lens.field @"creationDate"
{-# INLINEABLE uictCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The logo image for the UI customization.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictImageUrl :: Lens.Lens' UICustomizationType (Core.Maybe Types.ImageUrlType)
uictImageUrl = Lens.field @"imageUrl"
{-# INLINEABLE uictImageUrl #-}
{-# DEPRECATED imageUrl "Use generic-lens or generic-optics with 'imageUrl' instead"  #-}

-- | The last-modified date for the UI customization.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictLastModifiedDate :: Lens.Lens' UICustomizationType (Core.Maybe Core.NominalDiffTime)
uictLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE uictLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictUserPoolId :: Lens.Lens' UICustomizationType (Core.Maybe Types.UserPoolIdType)
uictUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE uictUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.FromJSON UICustomizationType where
        parseJSON
          = Core.withObject "UICustomizationType" Core.$
              \ x ->
                UICustomizationType' Core.<$>
                  (x Core..:? "CSS") Core.<*> x Core..:? "CSSVersion" Core.<*>
                    x Core..:? "ClientId"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "ImageUrl"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "UserPoolId"
