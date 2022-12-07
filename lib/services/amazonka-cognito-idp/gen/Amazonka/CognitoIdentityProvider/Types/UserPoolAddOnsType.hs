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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType where

import Amazonka.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user pool add-ons type.
--
-- /See:/ 'newUserPoolAddOnsType' smart constructor.
data UserPoolAddOnsType = UserPoolAddOnsType'
  { -- | The advanced security mode.
    advancedSecurityMode :: AdvancedSecurityModeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolAddOnsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedSecurityMode', 'userPoolAddOnsType_advancedSecurityMode' - The advanced security mode.
newUserPoolAddOnsType ::
  -- | 'advancedSecurityMode'
  AdvancedSecurityModeType ->
  UserPoolAddOnsType
newUserPoolAddOnsType pAdvancedSecurityMode_ =
  UserPoolAddOnsType'
    { advancedSecurityMode =
        pAdvancedSecurityMode_
    }

-- | The advanced security mode.
userPoolAddOnsType_advancedSecurityMode :: Lens.Lens' UserPoolAddOnsType AdvancedSecurityModeType
userPoolAddOnsType_advancedSecurityMode = Lens.lens (\UserPoolAddOnsType' {advancedSecurityMode} -> advancedSecurityMode) (\s@UserPoolAddOnsType' {} a -> s {advancedSecurityMode = a} :: UserPoolAddOnsType)

instance Data.FromJSON UserPoolAddOnsType where
  parseJSON =
    Data.withObject
      "UserPoolAddOnsType"
      ( \x ->
          UserPoolAddOnsType'
            Prelude.<$> (x Data..: "AdvancedSecurityMode")
      )

instance Prelude.Hashable UserPoolAddOnsType where
  hashWithSalt _salt UserPoolAddOnsType' {..} =
    _salt `Prelude.hashWithSalt` advancedSecurityMode

instance Prelude.NFData UserPoolAddOnsType where
  rnf UserPoolAddOnsType' {..} =
    Prelude.rnf advancedSecurityMode

instance Data.ToJSON UserPoolAddOnsType where
  toJSON UserPoolAddOnsType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AdvancedSecurityMode"
                  Data..= advancedSecurityMode
              )
          ]
      )
