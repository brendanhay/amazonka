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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UICustomizationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UICustomizationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for the UI customization information for a user pool\'s
-- built-in app UI.
--
-- /See:/ 'newUICustomizationType' smart constructor.
data UICustomizationType = UICustomizationType'
  { -- | The CSS values in the UI customization.
    css :: Prelude.Maybe Prelude.Text,
    -- | The CSS version number.
    cSSVersion :: Prelude.Maybe Prelude.Text,
    -- | The client ID for the client app.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The creation date for the UI customization.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The logo image for the UI customization.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The last-modified date for the UI customization.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UICustomizationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'css', 'uICustomizationType_css' - The CSS values in the UI customization.
--
-- 'cSSVersion', 'uICustomizationType_cSSVersion' - The CSS version number.
--
-- 'clientId', 'uICustomizationType_clientId' - The client ID for the client app.
--
-- 'creationDate', 'uICustomizationType_creationDate' - The creation date for the UI customization.
--
-- 'imageUrl', 'uICustomizationType_imageUrl' - The logo image for the UI customization.
--
-- 'lastModifiedDate', 'uICustomizationType_lastModifiedDate' - The last-modified date for the UI customization.
--
-- 'userPoolId', 'uICustomizationType_userPoolId' - The user pool ID for the user pool.
newUICustomizationType ::
  UICustomizationType
newUICustomizationType =
  UICustomizationType'
    { css = Prelude.Nothing,
      cSSVersion = Prelude.Nothing,
      clientId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | The CSS values in the UI customization.
uICustomizationType_css :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.Text)
uICustomizationType_css = Lens.lens (\UICustomizationType' {css} -> css) (\s@UICustomizationType' {} a -> s {css = a} :: UICustomizationType)

-- | The CSS version number.
uICustomizationType_cSSVersion :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.Text)
uICustomizationType_cSSVersion = Lens.lens (\UICustomizationType' {cSSVersion} -> cSSVersion) (\s@UICustomizationType' {} a -> s {cSSVersion = a} :: UICustomizationType)

-- | The client ID for the client app.
uICustomizationType_clientId :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.Text)
uICustomizationType_clientId = Lens.lens (\UICustomizationType' {clientId} -> clientId) (\s@UICustomizationType' {} a -> s {clientId = a} :: UICustomizationType) Prelude.. Lens.mapping Data._Sensitive

-- | The creation date for the UI customization.
uICustomizationType_creationDate :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.UTCTime)
uICustomizationType_creationDate = Lens.lens (\UICustomizationType' {creationDate} -> creationDate) (\s@UICustomizationType' {} a -> s {creationDate = a} :: UICustomizationType) Prelude.. Lens.mapping Data._Time

-- | The logo image for the UI customization.
uICustomizationType_imageUrl :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.Text)
uICustomizationType_imageUrl = Lens.lens (\UICustomizationType' {imageUrl} -> imageUrl) (\s@UICustomizationType' {} a -> s {imageUrl = a} :: UICustomizationType)

-- | The last-modified date for the UI customization.
uICustomizationType_lastModifiedDate :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.UTCTime)
uICustomizationType_lastModifiedDate = Lens.lens (\UICustomizationType' {lastModifiedDate} -> lastModifiedDate) (\s@UICustomizationType' {} a -> s {lastModifiedDate = a} :: UICustomizationType) Prelude.. Lens.mapping Data._Time

-- | The user pool ID for the user pool.
uICustomizationType_userPoolId :: Lens.Lens' UICustomizationType (Prelude.Maybe Prelude.Text)
uICustomizationType_userPoolId = Lens.lens (\UICustomizationType' {userPoolId} -> userPoolId) (\s@UICustomizationType' {} a -> s {userPoolId = a} :: UICustomizationType)

instance Data.FromJSON UICustomizationType where
  parseJSON =
    Data.withObject
      "UICustomizationType"
      ( \x ->
          UICustomizationType'
            Prelude.<$> (x Data..:? "CSS")
            Prelude.<*> (x Data..:? "CSSVersion")
            Prelude.<*> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "ImageUrl")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "UserPoolId")
      )

instance Prelude.Hashable UICustomizationType where
  hashWithSalt _salt UICustomizationType' {..} =
    _salt `Prelude.hashWithSalt` css
      `Prelude.hashWithSalt` cSSVersion
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` imageUrl
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData UICustomizationType where
  rnf UICustomizationType' {..} =
    Prelude.rnf css
      `Prelude.seq` Prelude.rnf cSSVersion
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf imageUrl
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf userPoolId
