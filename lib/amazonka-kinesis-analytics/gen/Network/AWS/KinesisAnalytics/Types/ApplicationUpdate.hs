{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationUpdate where

import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes updates to apply to an existing Amazon Kinesis Analytics application.
--
--
--
-- /See:/ 'applicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { _auReferenceDataSourceUpdates ::
      !(Maybe [ReferenceDataSourceUpdate]),
    _auInputUpdates :: !(Maybe [InputUpdate]),
    _auCloudWatchLoggingOptionUpdates ::
      !(Maybe [CloudWatchLoggingOptionUpdate]),
    _auOutputUpdates :: !(Maybe [OutputUpdate]),
    _auApplicationCodeUpdate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auReferenceDataSourceUpdates' - Describes application reference data source updates.
--
-- * 'auInputUpdates' - Describes application input configuration updates.
--
-- * 'auCloudWatchLoggingOptionUpdates' - Describes application CloudWatch logging option updates.
--
-- * 'auOutputUpdates' - Describes application output configuration updates.
--
-- * 'auApplicationCodeUpdate' - Describes application code updates.
applicationUpdate ::
  ApplicationUpdate
applicationUpdate =
  ApplicationUpdate'
    { _auReferenceDataSourceUpdates = Nothing,
      _auInputUpdates = Nothing,
      _auCloudWatchLoggingOptionUpdates = Nothing,
      _auOutputUpdates = Nothing,
      _auApplicationCodeUpdate = Nothing
    }

-- | Describes application reference data source updates.
auReferenceDataSourceUpdates :: Lens' ApplicationUpdate [ReferenceDataSourceUpdate]
auReferenceDataSourceUpdates = lens _auReferenceDataSourceUpdates (\s a -> s {_auReferenceDataSourceUpdates = a}) . _Default . _Coerce

-- | Describes application input configuration updates.
auInputUpdates :: Lens' ApplicationUpdate [InputUpdate]
auInputUpdates = lens _auInputUpdates (\s a -> s {_auInputUpdates = a}) . _Default . _Coerce

-- | Describes application CloudWatch logging option updates.
auCloudWatchLoggingOptionUpdates :: Lens' ApplicationUpdate [CloudWatchLoggingOptionUpdate]
auCloudWatchLoggingOptionUpdates = lens _auCloudWatchLoggingOptionUpdates (\s a -> s {_auCloudWatchLoggingOptionUpdates = a}) . _Default . _Coerce

-- | Describes application output configuration updates.
auOutputUpdates :: Lens' ApplicationUpdate [OutputUpdate]
auOutputUpdates = lens _auOutputUpdates (\s a -> s {_auOutputUpdates = a}) . _Default . _Coerce

-- | Describes application code updates.
auApplicationCodeUpdate :: Lens' ApplicationUpdate (Maybe Text)
auApplicationCodeUpdate = lens _auApplicationCodeUpdate (\s a -> s {_auApplicationCodeUpdate = a})

instance Hashable ApplicationUpdate

instance NFData ApplicationUpdate

instance ToJSON ApplicationUpdate where
  toJSON ApplicationUpdate' {..} =
    object
      ( catMaybes
          [ ("ReferenceDataSourceUpdates" .=)
              <$> _auReferenceDataSourceUpdates,
            ("InputUpdates" .=) <$> _auInputUpdates,
            ("CloudWatchLoggingOptionUpdates" .=)
              <$> _auCloudWatchLoggingOptionUpdates,
            ("OutputUpdates" .=) <$> _auOutputUpdates,
            ("ApplicationCodeUpdate" .=) <$> _auApplicationCodeUpdate
          ]
      )
