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
-- Module      : Network.AWS.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemNotification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A notification about the OpsItem.
--
-- /See:/ 'newOpsItemNotification' smart constructor.
data OpsItemNotification = OpsItemNotification'
  { -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsItemNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'opsItemNotification_arn' - The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
newOpsItemNotification ::
  OpsItemNotification
newOpsItemNotification =
  OpsItemNotification' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
opsItemNotification_arn :: Lens.Lens' OpsItemNotification (Core.Maybe Core.Text)
opsItemNotification_arn = Lens.lens (\OpsItemNotification' {arn} -> arn) (\s@OpsItemNotification' {} a -> s {arn = a} :: OpsItemNotification)

instance Core.FromJSON OpsItemNotification where
  parseJSON =
    Core.withObject
      "OpsItemNotification"
      ( \x ->
          OpsItemNotification' Core.<$> (x Core..:? "Arn")
      )

instance Core.Hashable OpsItemNotification

instance Core.NFData OpsItemNotification

instance Core.ToJSON OpsItemNotification where
  toJSON OpsItemNotification' {..} =
    Core.object
      (Core.catMaybes [("Arn" Core..=) Core.<$> arn])
