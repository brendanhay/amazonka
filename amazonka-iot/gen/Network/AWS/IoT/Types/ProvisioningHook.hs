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
-- Module      : Network.AWS.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningHook where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ProvisioningHook where
  parseJSON =
    Prelude.withObject
      "ProvisioningHook"
      ( \x ->
          ProvisioningHook'
            Prelude.<$> (x Prelude..:? "payloadVersion")
            Prelude.<*> (x Prelude..: "targetArn")
      )

instance Prelude.Hashable ProvisioningHook

instance Prelude.NFData ProvisioningHook

instance Prelude.ToJSON ProvisioningHook where
  toJSON ProvisioningHook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("payloadVersion" Prelude..=)
              Prelude.<$> payloadVersion,
            Prelude.Just ("targetArn" Prelude..= targetArn)
          ]
      )
