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
-- Module      : Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexMediaConnectOutputDestinationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Multiplex MediaConnect output destination settings.
--
-- /See:/ 'newMultiplexMediaConnectOutputDestinationSettings' smart constructor.
data MultiplexMediaConnectOutputDestinationSettings = MultiplexMediaConnectOutputDestinationSettings'
  { -- | The MediaConnect entitlement ARN available as a Flow source.
    entitlementArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexMediaConnectOutputDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlementArn', 'multiplexMediaConnectOutputDestinationSettings_entitlementArn' - The MediaConnect entitlement ARN available as a Flow source.
newMultiplexMediaConnectOutputDestinationSettings ::
  MultiplexMediaConnectOutputDestinationSettings
newMultiplexMediaConnectOutputDestinationSettings =
  MultiplexMediaConnectOutputDestinationSettings'
    { entitlementArn =
        Core.Nothing
    }

-- | The MediaConnect entitlement ARN available as a Flow source.
multiplexMediaConnectOutputDestinationSettings_entitlementArn :: Lens.Lens' MultiplexMediaConnectOutputDestinationSettings (Core.Maybe Core.Text)
multiplexMediaConnectOutputDestinationSettings_entitlementArn = Lens.lens (\MultiplexMediaConnectOutputDestinationSettings' {entitlementArn} -> entitlementArn) (\s@MultiplexMediaConnectOutputDestinationSettings' {} a -> s {entitlementArn = a} :: MultiplexMediaConnectOutputDestinationSettings)

instance
  Core.FromJSON
    MultiplexMediaConnectOutputDestinationSettings
  where
  parseJSON =
    Core.withObject
      "MultiplexMediaConnectOutputDestinationSettings"
      ( \x ->
          MultiplexMediaConnectOutputDestinationSettings'
            Core.<$> (x Core..:? "entitlementArn")
      )

instance
  Core.Hashable
    MultiplexMediaConnectOutputDestinationSettings

instance
  Core.NFData
    MultiplexMediaConnectOutputDestinationSettings
