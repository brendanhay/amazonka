{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UICustomizationType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for the UI customization information for a user pool\'s
-- built-in app UI.
--
-- /See:/ 'newUICustomizationType' smart constructor.
data UICustomizationType = UICustomizationType'
  { -- | The last-modified date for the UI customization.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The client ID for the client app.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The CSS version number.
    cSSVersion :: Core.Maybe Core.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Maybe Core.Text,
    -- | The creation date for the UI customization.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The logo image for the UI customization.
    imageUrl :: Core.Maybe Core.Text,
    -- | The CSS values in the UI customization.
    css :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UICustomizationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'uICustomizationType_lastModifiedDate' - The last-modified date for the UI customization.
--
-- 'clientId', 'uICustomizationType_clientId' - The client ID for the client app.
--
-- 'cSSVersion', 'uICustomizationType_cSSVersion' - The CSS version number.
--
-- 'userPoolId', 'uICustomizationType_userPoolId' - The user pool ID for the user pool.
--
-- 'creationDate', 'uICustomizationType_creationDate' - The creation date for the UI customization.
--
-- 'imageUrl', 'uICustomizationType_imageUrl' - The logo image for the UI customization.
--
-- 'css', 'uICustomizationType_css' - The CSS values in the UI customization.
newUICustomizationType ::
  UICustomizationType
newUICustomizationType =
  UICustomizationType'
    { lastModifiedDate =
        Core.Nothing,
      clientId = Core.Nothing,
      cSSVersion = Core.Nothing,
      userPoolId = Core.Nothing,
      creationDate = Core.Nothing,
      imageUrl = Core.Nothing,
      css = Core.Nothing
    }

-- | The last-modified date for the UI customization.
uICustomizationType_lastModifiedDate :: Lens.Lens' UICustomizationType (Core.Maybe Core.UTCTime)
uICustomizationType_lastModifiedDate = Lens.lens (\UICustomizationType' {lastModifiedDate} -> lastModifiedDate) (\s@UICustomizationType' {} a -> s {lastModifiedDate = a} :: UICustomizationType) Core.. Lens.mapping Core._Time

-- | The client ID for the client app.
uICustomizationType_clientId :: Lens.Lens' UICustomizationType (Core.Maybe Core.Text)
uICustomizationType_clientId = Lens.lens (\UICustomizationType' {clientId} -> clientId) (\s@UICustomizationType' {} a -> s {clientId = a} :: UICustomizationType) Core.. Lens.mapping Core._Sensitive

-- | The CSS version number.
uICustomizationType_cSSVersion :: Lens.Lens' UICustomizationType (Core.Maybe Core.Text)
uICustomizationType_cSSVersion = Lens.lens (\UICustomizationType' {cSSVersion} -> cSSVersion) (\s@UICustomizationType' {} a -> s {cSSVersion = a} :: UICustomizationType)

-- | The user pool ID for the user pool.
uICustomizationType_userPoolId :: Lens.Lens' UICustomizationType (Core.Maybe Core.Text)
uICustomizationType_userPoolId = Lens.lens (\UICustomizationType' {userPoolId} -> userPoolId) (\s@UICustomizationType' {} a -> s {userPoolId = a} :: UICustomizationType)

-- | The creation date for the UI customization.
uICustomizationType_creationDate :: Lens.Lens' UICustomizationType (Core.Maybe Core.UTCTime)
uICustomizationType_creationDate = Lens.lens (\UICustomizationType' {creationDate} -> creationDate) (\s@UICustomizationType' {} a -> s {creationDate = a} :: UICustomizationType) Core.. Lens.mapping Core._Time

-- | The logo image for the UI customization.
uICustomizationType_imageUrl :: Lens.Lens' UICustomizationType (Core.Maybe Core.Text)
uICustomizationType_imageUrl = Lens.lens (\UICustomizationType' {imageUrl} -> imageUrl) (\s@UICustomizationType' {} a -> s {imageUrl = a} :: UICustomizationType)

-- | The CSS values in the UI customization.
uICustomizationType_css :: Lens.Lens' UICustomizationType (Core.Maybe Core.Text)
uICustomizationType_css = Lens.lens (\UICustomizationType' {css} -> css) (\s@UICustomizationType' {} a -> s {css = a} :: UICustomizationType)

instance Core.FromJSON UICustomizationType where
  parseJSON =
    Core.withObject
      "UICustomizationType"
      ( \x ->
          UICustomizationType'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "CSSVersion")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "ImageUrl")
            Core.<*> (x Core..:? "CSS")
      )

instance Core.Hashable UICustomizationType

instance Core.NFData UICustomizationType
