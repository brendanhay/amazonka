{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
  ( UICustomizationType (..),

    -- * Smart constructor
    mkUICustomizationType,

    -- * Lenses
    uictClientId,
    uictLastModifiedDate,
    uictUserPoolId,
    uictCSS,
    uictCSSVersion,
    uictImageURL,
    uictCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for the UI customization information for a user pool's built-in app UI.
--
-- /See:/ 'mkUICustomizationType' smart constructor.
data UICustomizationType = UICustomizationType'
  { clientId ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    userPoolId :: Lude.Maybe Lude.Text,
    cSS :: Lude.Maybe Lude.Text,
    cSSVersion :: Lude.Maybe Lude.Text,
    imageURL :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UICustomizationType' with the minimum fields required to make a request.
--
-- * 'cSS' - The CSS values in the UI customization.
-- * 'cSSVersion' - The CSS version number.
-- * 'clientId' - The client ID for the client app.
-- * 'creationDate' - The creation date for the UI customization.
-- * 'imageURL' - The logo image for the UI customization.
-- * 'lastModifiedDate' - The last-modified date for the UI customization.
-- * 'userPoolId' - The user pool ID for the user pool.
mkUICustomizationType ::
  UICustomizationType
mkUICustomizationType =
  UICustomizationType'
    { clientId = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      userPoolId = Lude.Nothing,
      cSS = Lude.Nothing,
      cSSVersion = Lude.Nothing,
      imageURL = Lude.Nothing,
      creationDate = Lude.Nothing
    }

-- | The client ID for the client app.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictClientId :: Lens.Lens' UICustomizationType (Lude.Maybe (Lude.Sensitive Lude.Text))
uictClientId = Lens.lens (clientId :: UICustomizationType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: UICustomizationType)
{-# DEPRECATED uictClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The last-modified date for the UI customization.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictLastModifiedDate :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Timestamp)
uictLastModifiedDate = Lens.lens (lastModifiedDate :: UICustomizationType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UICustomizationType)
{-# DEPRECATED uictLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictUserPoolId :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Text)
uictUserPoolId = Lens.lens (userPoolId :: UICustomizationType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: UICustomizationType)
{-# DEPRECATED uictUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The CSS values in the UI customization.
--
-- /Note:/ Consider using 'cSS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCSS :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Text)
uictCSS = Lens.lens (cSS :: UICustomizationType -> Lude.Maybe Lude.Text) (\s a -> s {cSS = a} :: UICustomizationType)
{-# DEPRECATED uictCSS "Use generic-lens or generic-optics with 'cSS' instead." #-}

-- | The CSS version number.
--
-- /Note:/ Consider using 'cSSVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCSSVersion :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Text)
uictCSSVersion = Lens.lens (cSSVersion :: UICustomizationType -> Lude.Maybe Lude.Text) (\s a -> s {cSSVersion = a} :: UICustomizationType)
{-# DEPRECATED uictCSSVersion "Use generic-lens or generic-optics with 'cSSVersion' instead." #-}

-- | The logo image for the UI customization.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictImageURL :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Text)
uictImageURL = Lens.lens (imageURL :: UICustomizationType -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: UICustomizationType)
{-# DEPRECATED uictImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The creation date for the UI customization.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uictCreationDate :: Lens.Lens' UICustomizationType (Lude.Maybe Lude.Timestamp)
uictCreationDate = Lens.lens (creationDate :: UICustomizationType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UICustomizationType)
{-# DEPRECATED uictCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON UICustomizationType where
  parseJSON =
    Lude.withObject
      "UICustomizationType"
      ( \x ->
          UICustomizationType'
            Lude.<$> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "CSS")
            Lude.<*> (x Lude..:? "CSSVersion")
            Lude.<*> (x Lude..:? "ImageUrl")
            Lude.<*> (x Lude..:? "CreationDate")
      )
