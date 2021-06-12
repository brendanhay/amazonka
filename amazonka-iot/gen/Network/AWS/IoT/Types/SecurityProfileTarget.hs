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
-- Module      : Network.AWS.IoT.Types.SecurityProfileTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A target to which an alert is sent when a security profile behavior is
-- violated.
--
-- /See:/ 'newSecurityProfileTarget' smart constructor.
data SecurityProfileTarget = SecurityProfileTarget'
  { -- | The ARN of the security profile.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityProfileTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'securityProfileTarget_arn' - The ARN of the security profile.
newSecurityProfileTarget ::
  -- | 'arn'
  Core.Text ->
  SecurityProfileTarget
newSecurityProfileTarget pArn_ =
  SecurityProfileTarget' {arn = pArn_}

-- | The ARN of the security profile.
securityProfileTarget_arn :: Lens.Lens' SecurityProfileTarget Core.Text
securityProfileTarget_arn = Lens.lens (\SecurityProfileTarget' {arn} -> arn) (\s@SecurityProfileTarget' {} a -> s {arn = a} :: SecurityProfileTarget)

instance Core.FromJSON SecurityProfileTarget where
  parseJSON =
    Core.withObject
      "SecurityProfileTarget"
      ( \x ->
          SecurityProfileTarget' Core.<$> (x Core..: "arn")
      )

instance Core.Hashable SecurityProfileTarget

instance Core.NFData SecurityProfileTarget
