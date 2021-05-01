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
-- Module      : Network.AWS.EC2.Types.SpotDatafeedSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotDatafeedSubscription where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DatafeedSubscriptionState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the data feed for a Spot Instance.
--
-- /See:/ 'newSpotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { -- | The AWS account ID of the account.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the data feed files.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Prelude.Maybe SpotInstanceStateFault,
    -- | The state of the Spot Instance data feed subscription.
    state :: Prelude.Maybe DatafeedSubscriptionState,
    -- | The name of the Amazon S3 bucket where the Spot Instance data feed is
    -- located.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpotDatafeedSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'spotDatafeedSubscription_ownerId' - The AWS account ID of the account.
--
-- 'prefix', 'spotDatafeedSubscription_prefix' - The prefix for the data feed files.
--
-- 'fault', 'spotDatafeedSubscription_fault' - The fault codes for the Spot Instance request, if any.
--
-- 'state', 'spotDatafeedSubscription_state' - The state of the Spot Instance data feed subscription.
--
-- 'bucket', 'spotDatafeedSubscription_bucket' - The name of the Amazon S3 bucket where the Spot Instance data feed is
-- located.
newSpotDatafeedSubscription ::
  SpotDatafeedSubscription
newSpotDatafeedSubscription =
  SpotDatafeedSubscription'
    { ownerId =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      fault = Prelude.Nothing,
      state = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | The AWS account ID of the account.
spotDatafeedSubscription_ownerId :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_ownerId = Lens.lens (\SpotDatafeedSubscription' {ownerId} -> ownerId) (\s@SpotDatafeedSubscription' {} a -> s {ownerId = a} :: SpotDatafeedSubscription)

-- | The prefix for the data feed files.
spotDatafeedSubscription_prefix :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_prefix = Lens.lens (\SpotDatafeedSubscription' {prefix} -> prefix) (\s@SpotDatafeedSubscription' {} a -> s {prefix = a} :: SpotDatafeedSubscription)

-- | The fault codes for the Spot Instance request, if any.
spotDatafeedSubscription_fault :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe SpotInstanceStateFault)
spotDatafeedSubscription_fault = Lens.lens (\SpotDatafeedSubscription' {fault} -> fault) (\s@SpotDatafeedSubscription' {} a -> s {fault = a} :: SpotDatafeedSubscription)

-- | The state of the Spot Instance data feed subscription.
spotDatafeedSubscription_state :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe DatafeedSubscriptionState)
spotDatafeedSubscription_state = Lens.lens (\SpotDatafeedSubscription' {state} -> state) (\s@SpotDatafeedSubscription' {} a -> s {state = a} :: SpotDatafeedSubscription)

-- | The name of the Amazon S3 bucket where the Spot Instance data feed is
-- located.
spotDatafeedSubscription_bucket :: Lens.Lens' SpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
spotDatafeedSubscription_bucket = Lens.lens (\SpotDatafeedSubscription' {bucket} -> bucket) (\s@SpotDatafeedSubscription' {} a -> s {bucket = a} :: SpotDatafeedSubscription)

instance Prelude.FromXML SpotDatafeedSubscription where
  parseXML x =
    SpotDatafeedSubscription'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "prefix")
      Prelude.<*> (x Prelude..@? "fault")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "bucket")

instance Prelude.Hashable SpotDatafeedSubscription

instance Prelude.NFData SpotDatafeedSubscription
