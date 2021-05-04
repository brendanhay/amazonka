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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type used for enabling software token MFA at the user pool level.
--
-- /See:/ 'newSoftwareTokenMfaConfigType' smart constructor.
data SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType'
  { -- | Specifies whether software token MFA is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SoftwareTokenMfaConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'softwareTokenMfaConfigType_enabled' - Specifies whether software token MFA is enabled.
newSoftwareTokenMfaConfigType ::
  SoftwareTokenMfaConfigType
newSoftwareTokenMfaConfigType =
  SoftwareTokenMfaConfigType'
    { enabled =
        Prelude.Nothing
    }

-- | Specifies whether software token MFA is enabled.
softwareTokenMfaConfigType_enabled :: Lens.Lens' SoftwareTokenMfaConfigType (Prelude.Maybe Prelude.Bool)
softwareTokenMfaConfigType_enabled = Lens.lens (\SoftwareTokenMfaConfigType' {enabled} -> enabled) (\s@SoftwareTokenMfaConfigType' {} a -> s {enabled = a} :: SoftwareTokenMfaConfigType)

instance Prelude.FromJSON SoftwareTokenMfaConfigType where
  parseJSON =
    Prelude.withObject
      "SoftwareTokenMfaConfigType"
      ( \x ->
          SoftwareTokenMfaConfigType'
            Prelude.<$> (x Prelude..:? "Enabled")
      )

instance Prelude.Hashable SoftwareTokenMfaConfigType

instance Prelude.NFData SoftwareTokenMfaConfigType

instance Prelude.ToJSON SoftwareTokenMfaConfigType where
  toJSON SoftwareTokenMfaConfigType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Enabled" Prelude..=) Prelude.<$> enabled]
      )
