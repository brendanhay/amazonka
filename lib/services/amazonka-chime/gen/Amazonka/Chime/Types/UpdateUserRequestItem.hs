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
-- Module      : Amazonka.Chime.Types.UpdateUserRequestItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.UpdateUserRequestItem where

import Amazonka.Chime.Types.AlexaForBusinessMetadata
import Amazonka.Chime.Types.License
import Amazonka.Chime.Types.UserType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The user ID and user fields to update, used with the BatchUpdateUser
-- action.
--
-- /See:/ 'newUpdateUserRequestItem' smart constructor.
data UpdateUserRequestItem = UpdateUserRequestItem'
  { -- | The user license type.
    licenseType :: Prelude.Maybe License,
    -- | The user type.
    userType :: Prelude.Maybe UserType,
    -- | The Alexa for Business metadata.
    alexaForBusinessMetadata :: Prelude.Maybe AlexaForBusinessMetadata,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseType', 'updateUserRequestItem_licenseType' - The user license type.
--
-- 'userType', 'updateUserRequestItem_userType' - The user type.
--
-- 'alexaForBusinessMetadata', 'updateUserRequestItem_alexaForBusinessMetadata' - The Alexa for Business metadata.
--
-- 'userId', 'updateUserRequestItem_userId' - The user ID.
newUpdateUserRequestItem ::
  -- | 'userId'
  Prelude.Text ->
  UpdateUserRequestItem
newUpdateUserRequestItem pUserId_ =
  UpdateUserRequestItem'
    { licenseType =
        Prelude.Nothing,
      userType = Prelude.Nothing,
      alexaForBusinessMetadata = Prelude.Nothing,
      userId = pUserId_
    }

-- | The user license type.
updateUserRequestItem_licenseType :: Lens.Lens' UpdateUserRequestItem (Prelude.Maybe License)
updateUserRequestItem_licenseType = Lens.lens (\UpdateUserRequestItem' {licenseType} -> licenseType) (\s@UpdateUserRequestItem' {} a -> s {licenseType = a} :: UpdateUserRequestItem)

-- | The user type.
updateUserRequestItem_userType :: Lens.Lens' UpdateUserRequestItem (Prelude.Maybe UserType)
updateUserRequestItem_userType = Lens.lens (\UpdateUserRequestItem' {userType} -> userType) (\s@UpdateUserRequestItem' {} a -> s {userType = a} :: UpdateUserRequestItem)

-- | The Alexa for Business metadata.
updateUserRequestItem_alexaForBusinessMetadata :: Lens.Lens' UpdateUserRequestItem (Prelude.Maybe AlexaForBusinessMetadata)
updateUserRequestItem_alexaForBusinessMetadata = Lens.lens (\UpdateUserRequestItem' {alexaForBusinessMetadata} -> alexaForBusinessMetadata) (\s@UpdateUserRequestItem' {} a -> s {alexaForBusinessMetadata = a} :: UpdateUserRequestItem)

-- | The user ID.
updateUserRequestItem_userId :: Lens.Lens' UpdateUserRequestItem Prelude.Text
updateUserRequestItem_userId = Lens.lens (\UpdateUserRequestItem' {userId} -> userId) (\s@UpdateUserRequestItem' {} a -> s {userId = a} :: UpdateUserRequestItem)

instance Prelude.Hashable UpdateUserRequestItem where
  hashWithSalt salt' UpdateUserRequestItem' {..} =
    salt' `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` alexaForBusinessMetadata
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` licenseType

instance Prelude.NFData UpdateUserRequestItem where
  rnf UpdateUserRequestItem' {..} =
    Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf alexaForBusinessMetadata
      `Prelude.seq` Prelude.rnf userType

instance Core.ToJSON UpdateUserRequestItem where
  toJSON UpdateUserRequestItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LicenseType" Core..=) Prelude.<$> licenseType,
            ("UserType" Core..=) Prelude.<$> userType,
            ("AlexaForBusinessMetadata" Core..=)
              Prelude.<$> alexaForBusinessMetadata,
            Prelude.Just ("UserId" Core..= userId)
          ]
      )
