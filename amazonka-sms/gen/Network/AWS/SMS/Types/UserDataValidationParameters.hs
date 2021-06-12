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
-- Module      : Network.AWS.SMS.Types.UserDataValidationParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserDataValidationParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'newUserDataValidationParameters' smart constructor.
data UserDataValidationParameters = UserDataValidationParameters'
  { -- | The location of the validation script.
    source :: Core.Maybe Source,
    -- | The type of validation script.
    scriptType :: Core.Maybe ScriptType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserDataValidationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'userDataValidationParameters_source' - The location of the validation script.
--
-- 'scriptType', 'userDataValidationParameters_scriptType' - The type of validation script.
newUserDataValidationParameters ::
  UserDataValidationParameters
newUserDataValidationParameters =
  UserDataValidationParameters'
    { source =
        Core.Nothing,
      scriptType = Core.Nothing
    }

-- | The location of the validation script.
userDataValidationParameters_source :: Lens.Lens' UserDataValidationParameters (Core.Maybe Source)
userDataValidationParameters_source = Lens.lens (\UserDataValidationParameters' {source} -> source) (\s@UserDataValidationParameters' {} a -> s {source = a} :: UserDataValidationParameters)

-- | The type of validation script.
userDataValidationParameters_scriptType :: Lens.Lens' UserDataValidationParameters (Core.Maybe ScriptType)
userDataValidationParameters_scriptType = Lens.lens (\UserDataValidationParameters' {scriptType} -> scriptType) (\s@UserDataValidationParameters' {} a -> s {scriptType = a} :: UserDataValidationParameters)

instance Core.FromJSON UserDataValidationParameters where
  parseJSON =
    Core.withObject
      "UserDataValidationParameters"
      ( \x ->
          UserDataValidationParameters'
            Core.<$> (x Core..:? "source")
            Core.<*> (x Core..:? "scriptType")
      )

instance Core.Hashable UserDataValidationParameters

instance Core.NFData UserDataValidationParameters

instance Core.ToJSON UserDataValidationParameters where
  toJSON UserDataValidationParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("source" Core..=) Core.<$> source,
            ("scriptType" Core..=) Core.<$> scriptType
          ]
      )
