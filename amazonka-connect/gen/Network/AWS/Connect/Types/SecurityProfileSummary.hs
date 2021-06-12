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
-- Module      : Network.AWS.Connect.Types.SecurityProfileSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityProfileSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a security profile.
--
-- /See:/ 'newSecurityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { -- | The Amazon Resource Name (ARN) of the security profile.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the security profile.
    id :: Core.Maybe Core.Text,
    -- | The name of the security profile.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'securityProfileSummary_arn' - The Amazon Resource Name (ARN) of the security profile.
--
-- 'id', 'securityProfileSummary_id' - The identifier of the security profile.
--
-- 'name', 'securityProfileSummary_name' - The name of the security profile.
newSecurityProfileSummary ::
  SecurityProfileSummary
newSecurityProfileSummary =
  SecurityProfileSummary'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the security profile.
securityProfileSummary_arn :: Lens.Lens' SecurityProfileSummary (Core.Maybe Core.Text)
securityProfileSummary_arn = Lens.lens (\SecurityProfileSummary' {arn} -> arn) (\s@SecurityProfileSummary' {} a -> s {arn = a} :: SecurityProfileSummary)

-- | The identifier of the security profile.
securityProfileSummary_id :: Lens.Lens' SecurityProfileSummary (Core.Maybe Core.Text)
securityProfileSummary_id = Lens.lens (\SecurityProfileSummary' {id} -> id) (\s@SecurityProfileSummary' {} a -> s {id = a} :: SecurityProfileSummary)

-- | The name of the security profile.
securityProfileSummary_name :: Lens.Lens' SecurityProfileSummary (Core.Maybe Core.Text)
securityProfileSummary_name = Lens.lens (\SecurityProfileSummary' {name} -> name) (\s@SecurityProfileSummary' {} a -> s {name = a} :: SecurityProfileSummary)

instance Core.FromJSON SecurityProfileSummary where
  parseJSON =
    Core.withObject
      "SecurityProfileSummary"
      ( \x ->
          SecurityProfileSummary'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable SecurityProfileSummary

instance Core.NFData SecurityProfileSummary
