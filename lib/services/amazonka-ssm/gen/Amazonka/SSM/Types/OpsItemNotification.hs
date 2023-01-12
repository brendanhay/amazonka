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
-- Module      : Amazonka.SSM.Types.OpsItemNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemNotification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A notification about the OpsItem.
--
-- /See:/ 'newOpsItemNotification' smart constructor.
data OpsItemNotification = OpsItemNotification'
  { -- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
    -- (Amazon SNS) topic where notifications are sent when this OpsItem is
    -- edited or changed.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'opsItemNotification_arn' - The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- (Amazon SNS) topic where notifications are sent when this OpsItem is
-- edited or changed.
newOpsItemNotification ::
  OpsItemNotification
newOpsItemNotification =
  OpsItemNotification' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon Simple Notification Service
-- (Amazon SNS) topic where notifications are sent when this OpsItem is
-- edited or changed.
opsItemNotification_arn :: Lens.Lens' OpsItemNotification (Prelude.Maybe Prelude.Text)
opsItemNotification_arn = Lens.lens (\OpsItemNotification' {arn} -> arn) (\s@OpsItemNotification' {} a -> s {arn = a} :: OpsItemNotification)

instance Data.FromJSON OpsItemNotification where
  parseJSON =
    Data.withObject
      "OpsItemNotification"
      ( \x ->
          OpsItemNotification' Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable OpsItemNotification where
  hashWithSalt _salt OpsItemNotification' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData OpsItemNotification where
  rnf OpsItemNotification' {..} = Prelude.rnf arn

instance Data.ToJSON OpsItemNotification where
  toJSON OpsItemNotification' {..} =
    Data.object
      (Prelude.catMaybes [("Arn" Data..=) Prelude.<$> arn])
