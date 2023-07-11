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
-- Module      : Amazonka.SESV2.Types.PinpointDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.PinpointDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines an Amazon Pinpoint project destination for email
-- events. You can send email event data to a Amazon Pinpoint project to
-- view metrics using the Transactional Messaging dashboards that are built
-- in to Amazon Pinpoint. For more information, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/analytics-transactional-messages.html Transactional Messaging Charts>
-- in the /Amazon Pinpoint User Guide/.
--
-- /See:/ 'newPinpointDestination' smart constructor.
data PinpointDestination = PinpointDestination'
  { -- | The Amazon Resource Name (ARN) of the Amazon Pinpoint project to send
    -- email events to.
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
-- 'applicationArn', 'pinpointDestination_applicationArn' - The Amazon Resource Name (ARN) of the Amazon Pinpoint project to send
-- email events to.
newPinpointDestination ::
  PinpointDestination
newPinpointDestination =
  PinpointDestination'
    { applicationArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Pinpoint project to send
-- email events to.
pinpointDestination_applicationArn :: Lens.Lens' PinpointDestination (Prelude.Maybe Prelude.Text)
pinpointDestination_applicationArn = Lens.lens (\PinpointDestination' {applicationArn} -> applicationArn) (\s@PinpointDestination' {} a -> s {applicationArn = a} :: PinpointDestination)

instance Data.FromJSON PinpointDestination where
  parseJSON =
    Data.withObject
      "PinpointDestination"
      ( \x ->
          PinpointDestination'
            Prelude.<$> (x Data..:? "ApplicationArn")
      )

instance Prelude.Hashable PinpointDestination where
  hashWithSalt _salt PinpointDestination' {..} =
    _salt `Prelude.hashWithSalt` applicationArn

instance Prelude.NFData PinpointDestination where
  rnf PinpointDestination' {..} =
    Prelude.rnf applicationArn

instance Data.ToJSON PinpointDestination where
  toJSON PinpointDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationArn" Data..=)
              Prelude.<$> applicationArn
          ]
      )
