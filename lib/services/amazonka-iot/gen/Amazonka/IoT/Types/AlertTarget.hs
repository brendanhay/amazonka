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
-- Module      : Amazonka.IoT.Types.AlertTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AlertTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the alert target ARN and the role ARN.
--
-- /See:/ 'newAlertTarget' smart constructor.
data AlertTarget = AlertTarget'
  { -- | The Amazon Resource Name (ARN) of the notification target to which
    -- alerts are sent.
    alertTargetArn :: Prelude.Text,
    -- | The ARN of the role that grants permission to send alerts to the
    -- notification target.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlertTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertTargetArn', 'alertTarget_alertTargetArn' - The Amazon Resource Name (ARN) of the notification target to which
-- alerts are sent.
--
-- 'roleArn', 'alertTarget_roleArn' - The ARN of the role that grants permission to send alerts to the
-- notification target.
newAlertTarget ::
  -- | 'alertTargetArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  AlertTarget
newAlertTarget pAlertTargetArn_ pRoleArn_ =
  AlertTarget'
    { alertTargetArn = pAlertTargetArn_,
      roleArn = pRoleArn_
    }

-- | The Amazon Resource Name (ARN) of the notification target to which
-- alerts are sent.
alertTarget_alertTargetArn :: Lens.Lens' AlertTarget Prelude.Text
alertTarget_alertTargetArn = Lens.lens (\AlertTarget' {alertTargetArn} -> alertTargetArn) (\s@AlertTarget' {} a -> s {alertTargetArn = a} :: AlertTarget)

-- | The ARN of the role that grants permission to send alerts to the
-- notification target.
alertTarget_roleArn :: Lens.Lens' AlertTarget Prelude.Text
alertTarget_roleArn = Lens.lens (\AlertTarget' {roleArn} -> roleArn) (\s@AlertTarget' {} a -> s {roleArn = a} :: AlertTarget)

instance Data.FromJSON AlertTarget where
  parseJSON =
    Data.withObject
      "AlertTarget"
      ( \x ->
          AlertTarget'
            Prelude.<$> (x Data..: "alertTargetArn")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable AlertTarget where
  hashWithSalt _salt AlertTarget' {..} =
    _salt
      `Prelude.hashWithSalt` alertTargetArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData AlertTarget where
  rnf AlertTarget' {..} =
    Prelude.rnf alertTargetArn `Prelude.seq`
      Prelude.rnf roleArn

instance Data.ToJSON AlertTarget where
  toJSON AlertTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("alertTargetArn" Data..= alertTargetArn),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
