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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The type used for enabling software token MFA at the user pool level.
--
-- /See:/ 'newSoftwareTokenMfaConfigType' smart constructor.
data SoftwareTokenMfaConfigType = SoftwareTokenMfaConfigType'
  { -- | Specifies whether software token MFA is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  SoftwareTokenMfaConfigType' {enabled = Core.Nothing}

-- | Specifies whether software token MFA is enabled.
softwareTokenMfaConfigType_enabled :: Lens.Lens' SoftwareTokenMfaConfigType (Core.Maybe Core.Bool)
softwareTokenMfaConfigType_enabled = Lens.lens (\SoftwareTokenMfaConfigType' {enabled} -> enabled) (\s@SoftwareTokenMfaConfigType' {} a -> s {enabled = a} :: SoftwareTokenMfaConfigType)

instance Core.FromJSON SoftwareTokenMfaConfigType where
  parseJSON =
    Core.withObject
      "SoftwareTokenMfaConfigType"
      ( \x ->
          SoftwareTokenMfaConfigType'
            Core.<$> (x Core..:? "Enabled")
      )

instance Core.Hashable SoftwareTokenMfaConfigType

instance Core.NFData SoftwareTokenMfaConfigType

instance Core.ToJSON SoftwareTokenMfaConfigType where
  toJSON SoftwareTokenMfaConfigType' {..} =
    Core.object
      ( Core.catMaybes
          [("Enabled" Core..=) Core.<$> enabled]
      )
