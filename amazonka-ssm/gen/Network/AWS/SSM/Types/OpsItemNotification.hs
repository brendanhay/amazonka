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
-- Module      : Network.AWS.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemNotification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A notification about the OpsItem.
--
-- /See:/ 'newOpsItemNotification' smart constructor.
data OpsItemNotification = OpsItemNotification'
  { -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
    -- sent when this OpsItem is edited or changed.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  OpsItemNotification' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are
-- sent when this OpsItem is edited or changed.
opsItemNotification_arn :: Lens.Lens' OpsItemNotification (Prelude.Maybe Prelude.Text)
opsItemNotification_arn = Lens.lens (\OpsItemNotification' {arn} -> arn) (\s@OpsItemNotification' {} a -> s {arn = a} :: OpsItemNotification)

instance Prelude.FromJSON OpsItemNotification where
  parseJSON =
    Prelude.withObject
      "OpsItemNotification"
      ( \x ->
          OpsItemNotification'
            Prelude.<$> (x Prelude..:? "Arn")
      )

instance Prelude.Hashable OpsItemNotification

instance Prelude.NFData OpsItemNotification

instance Prelude.ToJSON OpsItemNotification where
  toJSON OpsItemNotification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Arn" Prelude..=) Prelude.<$> arn]
      )
