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
-- Module      : Network.AWS.SESv2.Types.PinpointDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.PinpointDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
--
-- /See:/ 'newPinpointDestination' smart constructor.
data PinpointDestination = PinpointDestination'
  { -- | The Amazon Resource Name (ARN) of the Amazon Pinpoint project that you
    -- want to send email events to.
    applicationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PinpointDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'pinpointDestination_applicationArn' - The Amazon Resource Name (ARN) of the Amazon Pinpoint project that you
-- want to send email events to.
newPinpointDestination ::
  PinpointDestination
newPinpointDestination =
  PinpointDestination'
    { applicationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Pinpoint project that you
-- want to send email events to.
pinpointDestination_applicationArn :: Lens.Lens' PinpointDestination (Prelude.Maybe Prelude.Text)
pinpointDestination_applicationArn = Lens.lens (\PinpointDestination' {applicationArn} -> applicationArn) (\s@PinpointDestination' {} a -> s {applicationArn = a} :: PinpointDestination)

instance Core.FromJSON PinpointDestination where
  parseJSON =
    Core.withObject
      "PinpointDestination"
      ( \x ->
          PinpointDestination'
            Prelude.<$> (x Core..:? "ApplicationArn")
      )

instance Prelude.Hashable PinpointDestination

instance Prelude.NFData PinpointDestination

instance Core.ToJSON PinpointDestination where
  toJSON PinpointDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ApplicationArn" Core..=)
              Prelude.<$> applicationArn
          ]
      )
