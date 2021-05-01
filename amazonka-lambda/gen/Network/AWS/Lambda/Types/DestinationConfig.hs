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
-- Module      : Network.AWS.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DestinationConfig where

import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A configuration object that specifies the destination of an event after
-- Lambda processes it.
--
-- /See:/ 'newDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The destination configuration for failed invocations.
    onFailure :: Prelude.Maybe OnFailure,
    -- | The destination configuration for successful invocations.
    onSuccess :: Prelude.Maybe OnSuccess
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onFailure', 'destinationConfig_onFailure' - The destination configuration for failed invocations.
--
-- 'onSuccess', 'destinationConfig_onSuccess' - The destination configuration for successful invocations.
newDestinationConfig ::
  DestinationConfig
newDestinationConfig =
  DestinationConfig'
    { onFailure = Prelude.Nothing,
      onSuccess = Prelude.Nothing
    }

-- | The destination configuration for failed invocations.
destinationConfig_onFailure :: Lens.Lens' DestinationConfig (Prelude.Maybe OnFailure)
destinationConfig_onFailure = Lens.lens (\DestinationConfig' {onFailure} -> onFailure) (\s@DestinationConfig' {} a -> s {onFailure = a} :: DestinationConfig)

-- | The destination configuration for successful invocations.
destinationConfig_onSuccess :: Lens.Lens' DestinationConfig (Prelude.Maybe OnSuccess)
destinationConfig_onSuccess = Lens.lens (\DestinationConfig' {onSuccess} -> onSuccess) (\s@DestinationConfig' {} a -> s {onSuccess = a} :: DestinationConfig)

instance Prelude.FromJSON DestinationConfig where
  parseJSON =
    Prelude.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Prelude.<$> (x Prelude..:? "OnFailure")
            Prelude.<*> (x Prelude..:? "OnSuccess")
      )

instance Prelude.Hashable DestinationConfig

instance Prelude.NFData DestinationConfig

instance Prelude.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OnFailure" Prelude..=) Prelude.<$> onFailure,
            ("OnSuccess" Prelude..=) Prelude.<$> onSuccess
          ]
      )
