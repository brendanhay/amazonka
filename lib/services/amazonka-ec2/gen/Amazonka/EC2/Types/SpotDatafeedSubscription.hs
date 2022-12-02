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
-- Module      : Amazonka.EC2.Types.SpotDatafeedSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotDatafeedSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DatafeedSubscriptionState
import Amazonka.EC2.Types.SpotInstanceStateFault
import qualified Amazonka.Prelude as Prelude

-- | Describes the data feed for a Spot Instance.
--
-- /See:/ 'newSpotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { -- | The Amazon Web Services account ID of the account.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket where the Spot Instance data feed is
    -- located.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The state of the Spot Instance data feed subscription.
    state :: Prelude.Maybe DatafeedSubscriptionState,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Prelude.Maybe SpotInstanceStateFault,
    -- | The prefix for the data feed files.
    prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotDatafeedSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'spotDatafeedSubscription_ownerId' - The Amazon Web Services account ID of the account.
--
-- 'bucket', 'spotDatafeedSubscription_bucket' - The name of the Amazon S3 bucket where the Spot Instance data feed is
-- located.
--
-- 'state', 'spotDatafeedSubscription_state' - The state of the Spot Instance data feed subscription.
--
-- 'fault', 'spotDatafeedSubscription_fault' - The fault codes for the Spot Instance request, if any.
--
-- 'prefix', 'spotDatafeedSubscription_prefix' - The prefix for the data feed files.
newSpotDatafeedSubscription ::
  SpotDatafeedSubscription
newSpotDatafeedSubscription =
  SpotDatafeedSubscription'
    { ownerId =
        Prelude.Nothing,
      bucket = Prelude.Nothing,
      state = Prelude.Nothing,
      fault = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the account.
spotDatafeedSubscription_ownerId :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_ownerId = Lens.lens (\SpotDatafeedSubscription' {ownerId} -> ownerId) (\s@SpotDatafeedSubscription' {} a -> s {ownerId = a} :: SpotDatafeedSubscription)

-- | The name of the Amazon S3 bucket where the Spot Instance data feed is
-- located.
spotDatafeedSubscription_bucket :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_bucket = Lens.lens (\SpotDatafeedSubscription' {bucket} -> bucket) (\s@SpotDatafeedSubscription' {} a -> s {bucket = a} :: SpotDatafeedSubscription)

-- | The state of the Spot Instance data feed subscription.
spotDatafeedSubscription_state :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe DatafeedSubscriptionState)
spotDatafeedSubscription_state = Lens.lens (\SpotDatafeedSubscription' {state} -> state) (\s@SpotDatafeedSubscription' {} a -> s {state = a} :: SpotDatafeedSubscription)

-- | The fault codes for the Spot Instance request, if any.
spotDatafeedSubscription_fault :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe SpotInstanceStateFault)
spotDatafeedSubscription_fault = Lens.lens (\SpotDatafeedSubscription' {fault} -> fault) (\s@SpotDatafeedSubscription' {} a -> s {fault = a} :: SpotDatafeedSubscription)

-- | The prefix for the data feed files.
spotDatafeedSubscription_prefix :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_prefix = Lens.lens (\SpotDatafeedSubscription' {prefix} -> prefix) (\s@SpotDatafeedSubscription' {} a -> s {prefix = a} :: SpotDatafeedSubscription)

instance Data.FromXML SpotDatafeedSubscription where
  parseXML x =
    SpotDatafeedSubscription'
      Prelude.<$> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "bucket")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "fault")
      Prelude.<*> (x Data..@? "prefix")

instance Prelude.Hashable SpotDatafeedSubscription where
  hashWithSalt _salt SpotDatafeedSubscription' {..} =
    _salt `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` fault
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData SpotDatafeedSubscription where
  rnf SpotDatafeedSubscription' {..} =
    Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf fault
      `Prelude.seq` Prelude.rnf prefix
