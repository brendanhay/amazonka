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
-- Module      : Network.AWS.LexV2Models.Types.FulfillmentCodeHookSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.FulfillmentCodeHookSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.FulfillmentUpdatesSpecification
import Network.AWS.LexV2Models.Types.PostFulfillmentStatusSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Determines if a Lambda function should be invoked for a specific intent.
--
-- /See:/ 'newFulfillmentCodeHookSettings' smart constructor.
data FulfillmentCodeHookSettings = FulfillmentCodeHookSettings'
  { -- | Provides settings for messages sent to the user for after the Lambda
    -- fulfillment function completes. Post-fulfillment messages can be sent
    -- for both streaming and non-streaming conversations.
    postFulfillmentStatusSpecification :: Prelude.Maybe PostFulfillmentStatusSpecification,
    -- | Provides settings for update messages sent to the user for long-running
    -- Lambda fulfillment functions. Fulfillment updates can be used only with
    -- streaming conversations.
    fulfillmentUpdatesSpecification :: Prelude.Maybe FulfillmentUpdatesSpecification,
    -- | Indicates whether a Lambda function should be invoked to fulfill a
    -- specific intent.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FulfillmentCodeHookSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postFulfillmentStatusSpecification', 'fulfillmentCodeHookSettings_postFulfillmentStatusSpecification' - Provides settings for messages sent to the user for after the Lambda
-- fulfillment function completes. Post-fulfillment messages can be sent
-- for both streaming and non-streaming conversations.
--
-- 'fulfillmentUpdatesSpecification', 'fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification' - Provides settings for update messages sent to the user for long-running
-- Lambda fulfillment functions. Fulfillment updates can be used only with
-- streaming conversations.
--
-- 'enabled', 'fulfillmentCodeHookSettings_enabled' - Indicates whether a Lambda function should be invoked to fulfill a
-- specific intent.
newFulfillmentCodeHookSettings ::
  -- | 'enabled'
  Prelude.Bool ->
  FulfillmentCodeHookSettings
newFulfillmentCodeHookSettings pEnabled_ =
  FulfillmentCodeHookSettings'
    { postFulfillmentStatusSpecification =
        Prelude.Nothing,
      fulfillmentUpdatesSpecification =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Provides settings for messages sent to the user for after the Lambda
-- fulfillment function completes. Post-fulfillment messages can be sent
-- for both streaming and non-streaming conversations.
fulfillmentCodeHookSettings_postFulfillmentStatusSpecification :: Lens.Lens' FulfillmentCodeHookSettings (Prelude.Maybe PostFulfillmentStatusSpecification)
fulfillmentCodeHookSettings_postFulfillmentStatusSpecification = Lens.lens (\FulfillmentCodeHookSettings' {postFulfillmentStatusSpecification} -> postFulfillmentStatusSpecification) (\s@FulfillmentCodeHookSettings' {} a -> s {postFulfillmentStatusSpecification = a} :: FulfillmentCodeHookSettings)

-- | Provides settings for update messages sent to the user for long-running
-- Lambda fulfillment functions. Fulfillment updates can be used only with
-- streaming conversations.
fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification :: Lens.Lens' FulfillmentCodeHookSettings (Prelude.Maybe FulfillmentUpdatesSpecification)
fulfillmentCodeHookSettings_fulfillmentUpdatesSpecification = Lens.lens (\FulfillmentCodeHookSettings' {fulfillmentUpdatesSpecification} -> fulfillmentUpdatesSpecification) (\s@FulfillmentCodeHookSettings' {} a -> s {fulfillmentUpdatesSpecification = a} :: FulfillmentCodeHookSettings)

-- | Indicates whether a Lambda function should be invoked to fulfill a
-- specific intent.
fulfillmentCodeHookSettings_enabled :: Lens.Lens' FulfillmentCodeHookSettings Prelude.Bool
fulfillmentCodeHookSettings_enabled = Lens.lens (\FulfillmentCodeHookSettings' {enabled} -> enabled) (\s@FulfillmentCodeHookSettings' {} a -> s {enabled = a} :: FulfillmentCodeHookSettings)

instance Core.FromJSON FulfillmentCodeHookSettings where
  parseJSON =
    Core.withObject
      "FulfillmentCodeHookSettings"
      ( \x ->
          FulfillmentCodeHookSettings'
            Prelude.<$> (x Core..:? "postFulfillmentStatusSpecification")
            Prelude.<*> (x Core..:? "fulfillmentUpdatesSpecification")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable FulfillmentCodeHookSettings

instance Prelude.NFData FulfillmentCodeHookSettings

instance Core.ToJSON FulfillmentCodeHookSettings where
  toJSON FulfillmentCodeHookSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("postFulfillmentStatusSpecification" Core..=)
              Prelude.<$> postFulfillmentStatusSpecification,
            ("fulfillmentUpdatesSpecification" Core..=)
              Prelude.<$> fulfillmentUpdatesSpecification,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
