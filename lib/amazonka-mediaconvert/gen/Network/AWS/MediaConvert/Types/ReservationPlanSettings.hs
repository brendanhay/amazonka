{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import Network.AWS.Prelude

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'reservationPlanSettings' smart constructor.
data ReservationPlanSettings = ReservationPlanSettings'
  { _rpsCommitment ::
      !Commitment,
    _rpsReservedSlots :: !Int,
    _rpsRenewalType :: !RenewalType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationPlanSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsCommitment' - The length of the term of your reserved queue pricing plan commitment.
--
-- * 'rpsReservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
--
-- * 'rpsRenewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
reservationPlanSettings ::
  -- | 'rpsCommitment'
  Commitment ->
  -- | 'rpsReservedSlots'
  Int ->
  -- | 'rpsRenewalType'
  RenewalType ->
  ReservationPlanSettings
reservationPlanSettings pCommitment_ pReservedSlots_ pRenewalType_ =
  ReservationPlanSettings'
    { _rpsCommitment = pCommitment_,
      _rpsReservedSlots = pReservedSlots_,
      _rpsRenewalType = pRenewalType_
    }

-- | The length of the term of your reserved queue pricing plan commitment.
rpsCommitment :: Lens' ReservationPlanSettings Commitment
rpsCommitment = lens _rpsCommitment (\s a -> s {_rpsCommitment = a})

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
rpsReservedSlots :: Lens' ReservationPlanSettings Int
rpsReservedSlots = lens _rpsReservedSlots (\s a -> s {_rpsReservedSlots = a})

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
rpsRenewalType :: Lens' ReservationPlanSettings RenewalType
rpsRenewalType = lens _rpsRenewalType (\s a -> s {_rpsRenewalType = a})

instance Hashable ReservationPlanSettings

instance NFData ReservationPlanSettings

instance ToJSON ReservationPlanSettings where
  toJSON ReservationPlanSettings' {..} =
    object
      ( catMaybes
          [ Just ("commitment" .= _rpsCommitment),
            Just ("reservedSlots" .= _rpsReservedSlots),
            Just ("renewalType" .= _rpsRenewalType)
          ]
      )
