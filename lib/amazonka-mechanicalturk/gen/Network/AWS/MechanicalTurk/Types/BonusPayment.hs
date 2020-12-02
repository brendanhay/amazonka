{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.BonusPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.BonusPayment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a Bonus payment paid to a Worker.
--
--
--
-- /See:/ 'bonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { _bpReason :: !(Maybe Text),
    _bpGrantTime :: !(Maybe POSIX),
    _bpWorkerId :: !(Maybe Text),
    _bpAssignmentId :: !(Maybe Text),
    _bpBonusAmount :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BonusPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpReason' - The Reason text given when the bonus was granted, if any.
--
-- * 'bpGrantTime' - The date and time of when the bonus was granted.
--
-- * 'bpWorkerId' - The ID of the Worker to whom the bonus was paid.
--
-- * 'bpAssignmentId' - The ID of the assignment associated with this bonus payment.
--
-- * 'bpBonusAmount' - Undocumented member.
bonusPayment ::
  BonusPayment
bonusPayment =
  BonusPayment'
    { _bpReason = Nothing,
      _bpGrantTime = Nothing,
      _bpWorkerId = Nothing,
      _bpAssignmentId = Nothing,
      _bpBonusAmount = Nothing
    }

-- | The Reason text given when the bonus was granted, if any.
bpReason :: Lens' BonusPayment (Maybe Text)
bpReason = lens _bpReason (\s a -> s {_bpReason = a})

-- | The date and time of when the bonus was granted.
bpGrantTime :: Lens' BonusPayment (Maybe UTCTime)
bpGrantTime = lens _bpGrantTime (\s a -> s {_bpGrantTime = a}) . mapping _Time

-- | The ID of the Worker to whom the bonus was paid.
bpWorkerId :: Lens' BonusPayment (Maybe Text)
bpWorkerId = lens _bpWorkerId (\s a -> s {_bpWorkerId = a})

-- | The ID of the assignment associated with this bonus payment.
bpAssignmentId :: Lens' BonusPayment (Maybe Text)
bpAssignmentId = lens _bpAssignmentId (\s a -> s {_bpAssignmentId = a})

-- | Undocumented member.
bpBonusAmount :: Lens' BonusPayment (Maybe Text)
bpBonusAmount = lens _bpBonusAmount (\s a -> s {_bpBonusAmount = a})

instance FromJSON BonusPayment where
  parseJSON =
    withObject
      "BonusPayment"
      ( \x ->
          BonusPayment'
            <$> (x .:? "Reason")
            <*> (x .:? "GrantTime")
            <*> (x .:? "WorkerId")
            <*> (x .:? "AssignmentId")
            <*> (x .:? "BonusAmount")
      )

instance Hashable BonusPayment

instance NFData BonusPayment
