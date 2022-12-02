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
-- Module      : Amazonka.IoTEvents.Types.AcknowledgeFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AcknowledgeFlow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether to get notified for alarm state changes.
--
-- /See:/ 'newAcknowledgeFlow' smart constructor.
data AcknowledgeFlow = AcknowledgeFlow'
  { -- | The value must be @TRUE@ or @FALSE@. If @TRUE@, you receive a
    -- notification when the alarm state changes. You must choose to
    -- acknowledge the notification before the alarm state can return to
    -- @NORMAL@. If @FALSE@, you won\'t receive notifications. The alarm
    -- automatically changes to the @NORMAL@ state when the input property
    -- value returns to the specified range.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'acknowledgeFlow_enabled' - The value must be @TRUE@ or @FALSE@. If @TRUE@, you receive a
-- notification when the alarm state changes. You must choose to
-- acknowledge the notification before the alarm state can return to
-- @NORMAL@. If @FALSE@, you won\'t receive notifications. The alarm
-- automatically changes to the @NORMAL@ state when the input property
-- value returns to the specified range.
newAcknowledgeFlow ::
  -- | 'enabled'
  Prelude.Bool ->
  AcknowledgeFlow
newAcknowledgeFlow pEnabled_ =
  AcknowledgeFlow' {enabled = pEnabled_}

-- | The value must be @TRUE@ or @FALSE@. If @TRUE@, you receive a
-- notification when the alarm state changes. You must choose to
-- acknowledge the notification before the alarm state can return to
-- @NORMAL@. If @FALSE@, you won\'t receive notifications. The alarm
-- automatically changes to the @NORMAL@ state when the input property
-- value returns to the specified range.
acknowledgeFlow_enabled :: Lens.Lens' AcknowledgeFlow Prelude.Bool
acknowledgeFlow_enabled = Lens.lens (\AcknowledgeFlow' {enabled} -> enabled) (\s@AcknowledgeFlow' {} a -> s {enabled = a} :: AcknowledgeFlow)

instance Data.FromJSON AcknowledgeFlow where
  parseJSON =
    Data.withObject
      "AcknowledgeFlow"
      ( \x ->
          AcknowledgeFlow' Prelude.<$> (x Data..: "enabled")
      )

instance Prelude.Hashable AcknowledgeFlow where
  hashWithSalt _salt AcknowledgeFlow' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData AcknowledgeFlow where
  rnf AcknowledgeFlow' {..} = Prelude.rnf enabled

instance Data.ToJSON AcknowledgeFlow where
  toJSON AcknowledgeFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("enabled" Data..= enabled)]
      )
