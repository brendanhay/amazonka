{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigExportDeliveryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigExportDeliveryInfo where

import Network.AWS.Config.Types.DeliveryStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.
--
--
--
-- /See:/ 'configExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
  { _cediLastErrorCode ::
      !(Maybe Text),
    _cediLastAttemptTime :: !(Maybe POSIX),
    _cediLastSuccessfulTime :: !(Maybe POSIX),
    _cediLastStatus ::
      !(Maybe DeliveryStatus),
    _cediLastErrorMessage :: !(Maybe Text),
    _cediNextDeliveryTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigExportDeliveryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cediLastErrorCode' - The error code from the last attempted delivery.
--
-- * 'cediLastAttemptTime' - The time of the last attempted delivery.
--
-- * 'cediLastSuccessfulTime' - The time of the last successful delivery.
--
-- * 'cediLastStatus' - Status of the last attempted delivery.
--
-- * 'cediLastErrorMessage' - The error message from the last attempted delivery.
--
-- * 'cediNextDeliveryTime' - The time that the next delivery occurs.
configExportDeliveryInfo ::
  ConfigExportDeliveryInfo
configExportDeliveryInfo =
  ConfigExportDeliveryInfo'
    { _cediLastErrorCode = Nothing,
      _cediLastAttemptTime = Nothing,
      _cediLastSuccessfulTime = Nothing,
      _cediLastStatus = Nothing,
      _cediLastErrorMessage = Nothing,
      _cediNextDeliveryTime = Nothing
    }

-- | The error code from the last attempted delivery.
cediLastErrorCode :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorCode = lens _cediLastErrorCode (\s a -> s {_cediLastErrorCode = a})

-- | The time of the last attempted delivery.
cediLastAttemptTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastAttemptTime = lens _cediLastAttemptTime (\s a -> s {_cediLastAttemptTime = a}) . mapping _Time

-- | The time of the last successful delivery.
cediLastSuccessfulTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastSuccessfulTime = lens _cediLastSuccessfulTime (\s a -> s {_cediLastSuccessfulTime = a}) . mapping _Time

-- | Status of the last attempted delivery.
cediLastStatus :: Lens' ConfigExportDeliveryInfo (Maybe DeliveryStatus)
cediLastStatus = lens _cediLastStatus (\s a -> s {_cediLastStatus = a})

-- | The error message from the last attempted delivery.
cediLastErrorMessage :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorMessage = lens _cediLastErrorMessage (\s a -> s {_cediLastErrorMessage = a})

-- | The time that the next delivery occurs.
cediNextDeliveryTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediNextDeliveryTime = lens _cediNextDeliveryTime (\s a -> s {_cediNextDeliveryTime = a}) . mapping _Time

instance FromJSON ConfigExportDeliveryInfo where
  parseJSON =
    withObject
      "ConfigExportDeliveryInfo"
      ( \x ->
          ConfigExportDeliveryInfo'
            <$> (x .:? "lastErrorCode")
            <*> (x .:? "lastAttemptTime")
            <*> (x .:? "lastSuccessfulTime")
            <*> (x .:? "lastStatus")
            <*> (x .:? "lastErrorMessage")
            <*> (x .:? "nextDeliveryTime")
      )

instance Hashable ConfigExportDeliveryInfo

instance NFData ConfigExportDeliveryInfo
