{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType where

import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user pool add-ons type.
--
-- /See:/ 'newUserPoolAddOnsType' smart constructor.
data UserPoolAddOnsType = UserPoolAddOnsType'
  { -- | The advanced security mode.
    advancedSecurityMode :: AdvancedSecurityModeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON UserPoolAddOnsType where
  parseJSON =
    Prelude.withObject
      "UserPoolAddOnsType"
      ( \x ->
          UserPoolAddOnsType'
            Prelude.<$> (x Prelude..: "AdvancedSecurityMode")
      )

instance Prelude.Hashable UserPoolAddOnsType

instance Prelude.NFData UserPoolAddOnsType

instance Prelude.ToJSON UserPoolAddOnsType where
  toJSON UserPoolAddOnsType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AdvancedSecurityMode"
                  Prelude..= advancedSecurityMode
              )
          ]
      )
