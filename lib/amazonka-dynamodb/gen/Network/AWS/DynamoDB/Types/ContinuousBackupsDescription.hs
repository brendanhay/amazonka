{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContinuousBackupsDescription where

import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the continuous backups and point in time recovery settings on the table.
--
--
--
-- /See:/ 'continuousBackupsDescription' smart constructor.
data ContinuousBackupsDescription = ContinuousBackupsDescription'
  { _cbdPointInTimeRecoveryDescription ::
      !( Maybe
           PointInTimeRecoveryDescription
       ),
    _cbdContinuousBackupsStatus ::
      !ContinuousBackupsStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContinuousBackupsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbdPointInTimeRecoveryDescription' - The description of the point in time recovery settings applied to the table.
--
-- * 'cbdContinuousBackupsStatus' - @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
continuousBackupsDescription ::
  -- | 'cbdContinuousBackupsStatus'
  ContinuousBackupsStatus ->
  ContinuousBackupsDescription
continuousBackupsDescription pContinuousBackupsStatus_ =
  ContinuousBackupsDescription'
    { _cbdPointInTimeRecoveryDescription =
        Nothing,
      _cbdContinuousBackupsStatus = pContinuousBackupsStatus_
    }

-- | The description of the point in time recovery settings applied to the table.
cbdPointInTimeRecoveryDescription :: Lens' ContinuousBackupsDescription (Maybe PointInTimeRecoveryDescription)
cbdPointInTimeRecoveryDescription = lens _cbdPointInTimeRecoveryDescription (\s a -> s {_cbdPointInTimeRecoveryDescription = a})

-- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
cbdContinuousBackupsStatus :: Lens' ContinuousBackupsDescription ContinuousBackupsStatus
cbdContinuousBackupsStatus = lens _cbdContinuousBackupsStatus (\s a -> s {_cbdContinuousBackupsStatus = a})

instance FromJSON ContinuousBackupsDescription where
  parseJSON =
    withObject
      "ContinuousBackupsDescription"
      ( \x ->
          ContinuousBackupsDescription'
            <$> (x .:? "PointInTimeRecoveryDescription")
            <*> (x .: "ContinuousBackupsStatus")
      )

instance Hashable ContinuousBackupsDescription

instance NFData ContinuousBackupsDescription
