{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlan where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import Network.AWS.MediaConvert.Types.ReservationPlanStatus
import Network.AWS.Prelude

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'reservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { _rpStatus ::
      !(Maybe ReservationPlanStatus),
    _rpExpiresAt :: !(Maybe POSIX),
    _rpPurchasedAt :: !(Maybe POSIX),
    _rpCommitment :: !(Maybe Commitment),
    _rpReservedSlots :: !(Maybe Int),
    _rpRenewalType :: !(Maybe RenewalType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpStatus' - Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
--
-- * 'rpExpiresAt' - The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
--
-- * 'rpPurchasedAt' - The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
--
-- * 'rpCommitment' - The length of the term of your reserved queue pricing plan commitment.
--
-- * 'rpReservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
--
-- * 'rpRenewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
reservationPlan ::
  ReservationPlan
reservationPlan =
  ReservationPlan'
    { _rpStatus = Nothing,
      _rpExpiresAt = Nothing,
      _rpPurchasedAt = Nothing,
      _rpCommitment = Nothing,
      _rpReservedSlots = Nothing,
      _rpRenewalType = Nothing
    }

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
rpStatus :: Lens' ReservationPlan (Maybe ReservationPlanStatus)
rpStatus = lens _rpStatus (\s a -> s {_rpStatus = a})

-- | The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
rpExpiresAt :: Lens' ReservationPlan (Maybe UTCTime)
rpExpiresAt = lens _rpExpiresAt (\s a -> s {_rpExpiresAt = a}) . mapping _Time

-- | The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
rpPurchasedAt :: Lens' ReservationPlan (Maybe UTCTime)
rpPurchasedAt = lens _rpPurchasedAt (\s a -> s {_rpPurchasedAt = a}) . mapping _Time

-- | The length of the term of your reserved queue pricing plan commitment.
rpCommitment :: Lens' ReservationPlan (Maybe Commitment)
rpCommitment = lens _rpCommitment (\s a -> s {_rpCommitment = a})

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
rpReservedSlots :: Lens' ReservationPlan (Maybe Int)
rpReservedSlots = lens _rpReservedSlots (\s a -> s {_rpReservedSlots = a})

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
rpRenewalType :: Lens' ReservationPlan (Maybe RenewalType)
rpRenewalType = lens _rpRenewalType (\s a -> s {_rpRenewalType = a})

instance FromJSON ReservationPlan where
  parseJSON =
    withObject
      "ReservationPlan"
      ( \x ->
          ReservationPlan'
            <$> (x .:? "status")
            <*> (x .:? "expiresAt")
            <*> (x .:? "purchasedAt")
            <*> (x .:? "commitment")
            <*> (x .:? "reservedSlots")
            <*> (x .:? "renewalType")
      )

instance Hashable ReservationPlan

instance NFData ReservationPlan
