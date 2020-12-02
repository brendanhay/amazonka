{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionStatus where

import Network.AWS.ElasticSearch.Types.OptionState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the current status of the entity.
--
--
--
-- /See:/ 'optionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { _osPendingDeletion ::
      !(Maybe Bool),
    _osUpdateVersion :: !(Maybe Nat),
    _osCreationDate :: !POSIX,
    _osUpdateDate :: !POSIX,
    _osState :: !OptionState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osPendingDeletion' - Indicates whether the Elasticsearch domain is being deleted.
--
-- * 'osUpdateVersion' - Specifies the latest version for the entity.
--
-- * 'osCreationDate' - Timestamp which tells the creation date for the entity.
--
-- * 'osUpdateDate' - Timestamp which tells the last updated time for the entity.
--
-- * 'osState' - Provides the @OptionState@ for the Elasticsearch domain.
optionStatus ::
  -- | 'osCreationDate'
  UTCTime ->
  -- | 'osUpdateDate'
  UTCTime ->
  -- | 'osState'
  OptionState ->
  OptionStatus
optionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { _osPendingDeletion = Nothing,
      _osUpdateVersion = Nothing,
      _osCreationDate = _Time # pCreationDate_,
      _osUpdateDate = _Time # pUpdateDate_,
      _osState = pState_
    }

-- | Indicates whether the Elasticsearch domain is being deleted.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion = lens _osPendingDeletion (\s a -> s {_osPendingDeletion = a})

-- | Specifies the latest version for the entity.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\s a -> s {_osUpdateVersion = a}) . mapping _Nat

-- | Timestamp which tells the creation date for the entity.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\s a -> s {_osCreationDate = a}) . _Time

-- | Timestamp which tells the last updated time for the entity.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\s a -> s {_osUpdateDate = a}) . _Time

-- | Provides the @OptionState@ for the Elasticsearch domain.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\s a -> s {_osState = a})

instance FromJSON OptionStatus where
  parseJSON =
    withObject
      "OptionStatus"
      ( \x ->
          OptionStatus'
            <$> (x .:? "PendingDeletion")
            <*> (x .:? "UpdateVersion")
            <*> (x .: "CreationDate")
            <*> (x .: "UpdateDate")
            <*> (x .: "State")
      )

instance Hashable OptionStatus

instance NFData OptionStatus
