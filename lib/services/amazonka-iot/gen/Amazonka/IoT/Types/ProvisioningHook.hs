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
-- Module      : Amazonka.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ProvisioningHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Structure that contains @payloadVersion@ and @targetArn@.
--
-- /See:/ 'newProvisioningHook' smart constructor.
data ProvisioningHook = ProvisioningHook'
  { -- | The payload that was sent to the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    payloadVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloadVersion', 'provisioningHook_payloadVersion' - The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- 'targetArn', 'provisioningHook_targetArn' - The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
newProvisioningHook ::
  -- | 'targetArn'
  Prelude.Text ->
  ProvisioningHook
newProvisioningHook pTargetArn_ =
  ProvisioningHook'
    { payloadVersion = Prelude.Nothing,
      targetArn = pTargetArn_
    }

-- | The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
provisioningHook_payloadVersion :: Lens.Lens' ProvisioningHook (Prelude.Maybe Prelude.Text)
provisioningHook_payloadVersion = Lens.lens (\ProvisioningHook' {payloadVersion} -> payloadVersion) (\s@ProvisioningHook' {} a -> s {payloadVersion = a} :: ProvisioningHook)

-- | The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
provisioningHook_targetArn :: Lens.Lens' ProvisioningHook Prelude.Text
provisioningHook_targetArn = Lens.lens (\ProvisioningHook' {targetArn} -> targetArn) (\s@ProvisioningHook' {} a -> s {targetArn = a} :: ProvisioningHook)

instance Data.FromJSON ProvisioningHook where
  parseJSON =
    Data.withObject
      "ProvisioningHook"
      ( \x ->
          ProvisioningHook'
            Prelude.<$> (x Data..:? "payloadVersion")
            Prelude.<*> (x Data..: "targetArn")
      )

instance Prelude.Hashable ProvisioningHook where
  hashWithSalt _salt ProvisioningHook' {..} =
    _salt
      `Prelude.hashWithSalt` payloadVersion
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData ProvisioningHook where
  rnf ProvisioningHook' {..} =
    Prelude.rnf payloadVersion
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToJSON ProvisioningHook where
  toJSON ProvisioningHook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("payloadVersion" Data..=)
              Prelude.<$> payloadVersion,
            Prelude.Just ("targetArn" Data..= targetArn)
          ]
      )
