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
-- Module      : Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type used for enabling software token MFA at the user pool level.
--
-- /See:/ 'newSoftwareTokenMfaConfigType' smart constructor.
data SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType'
  { -- | Specifies whether software token MFA is activated.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwareTokenMfaConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'softwareTokenMfaConfigType_enabled' - Specifies whether software token MFA is activated.
newSoftwareTokenMfaConfigType ::
  SoftwareTokenMfaConfigType
newSoftwareTokenMfaConfigType =
  SoftwareTokenMfaConfigType'
    { enabled =
        Prelude.Nothing
    }

-- | Specifies whether software token MFA is activated.
softwareTokenMfaConfigType_enabled :: Lens.Lens' SoftwareTokenMfaConfigType (Prelude.Maybe Prelude.Bool)
softwareTokenMfaConfigType_enabled = Lens.lens (\SoftwareTokenMfaConfigType' {enabled} -> enabled) (\s@SoftwareTokenMfaConfigType' {} a -> s {enabled = a} :: SoftwareTokenMfaConfigType)

instance Data.FromJSON SoftwareTokenMfaConfigType where
  parseJSON =
    Data.withObject
      "SoftwareTokenMfaConfigType"
      ( \x ->
          SoftwareTokenMfaConfigType'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance Prelude.Hashable SoftwareTokenMfaConfigType where
  hashWithSalt _salt SoftwareTokenMfaConfigType' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData SoftwareTokenMfaConfigType where
  rnf SoftwareTokenMfaConfigType' {..} =
    Prelude.rnf enabled

instance Data.ToJSON SoftwareTokenMfaConfigType where
  toJSON SoftwareTokenMfaConfigType' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Enabled" Data..=) Prelude.<$> enabled]
      )
