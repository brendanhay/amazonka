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
-- Module      : Network.AWS.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileIdentifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifying information for a Device Defender security profile.
--
-- /See:/ 'newSecurityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { -- | The name you\'ve given to the security profile.
    name :: Core.Text,
    -- | The ARN of the security profile.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityProfileIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'securityProfileIdentifier_name' - The name you\'ve given to the security profile.
--
-- 'arn', 'securityProfileIdentifier_arn' - The ARN of the security profile.
newSecurityProfileIdentifier ::
  -- | 'name'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  SecurityProfileIdentifier
newSecurityProfileIdentifier pName_ pArn_ =
  SecurityProfileIdentifier'
    { name = pName_,
      arn = pArn_
    }

-- | The name you\'ve given to the security profile.
securityProfileIdentifier_name :: Lens.Lens' SecurityProfileIdentifier Core.Text
securityProfileIdentifier_name = Lens.lens (\SecurityProfileIdentifier' {name} -> name) (\s@SecurityProfileIdentifier' {} a -> s {name = a} :: SecurityProfileIdentifier)

-- | The ARN of the security profile.
securityProfileIdentifier_arn :: Lens.Lens' SecurityProfileIdentifier Core.Text
securityProfileIdentifier_arn = Lens.lens (\SecurityProfileIdentifier' {arn} -> arn) (\s@SecurityProfileIdentifier' {} a -> s {arn = a} :: SecurityProfileIdentifier)

instance Core.FromJSON SecurityProfileIdentifier where
  parseJSON =
    Core.withObject
      "SecurityProfileIdentifier"
      ( \x ->
          SecurityProfileIdentifier'
            Core.<$> (x Core..: "name") Core.<*> (x Core..: "arn")
      )

instance Core.Hashable SecurityProfileIdentifier

instance Core.NFData SecurityProfileIdentifier
