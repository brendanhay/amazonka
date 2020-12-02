{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.OptionStatus where

import Network.AWS.CloudSearch.Types.OptionState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of domain configuration option.
--
--
--
-- /See:/ 'optionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { _osPendingDeletion ::
      !(Maybe Bool),
    _osUpdateVersion :: !(Maybe Nat),
    _osCreationDate :: !ISO8601,
    _osUpdateDate :: !ISO8601,
    _osState :: !OptionState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osPendingDeletion' - Indicates that the option will be deleted once processing is complete.
--
-- * 'osUpdateVersion' - A unique integer that indicates when this option was last updated.
--
-- * 'osCreationDate' - A timestamp for when this option was created.
--
-- * 'osUpdateDate' - A timestamp for when this option was last updated.
--
-- * 'osState' - The state of processing a change to an option. Possible values:     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * @Processing@ : the option's latest value is in the process of being activated.     * @Active@ : the option's latest value is completely deployed.    * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
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

-- | Indicates that the option will be deleted once processing is complete.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion = lens _osPendingDeletion (\s a -> s {_osPendingDeletion = a})

-- | A unique integer that indicates when this option was last updated.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\s a -> s {_osUpdateVersion = a}) . mapping _Nat

-- | A timestamp for when this option was created.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\s a -> s {_osCreationDate = a}) . _Time

-- | A timestamp for when this option was last updated.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\s a -> s {_osUpdateDate = a}) . _Time

-- | The state of processing a change to an option. Possible values:     * @RequiresIndexDocuments@ : the option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * @Processing@ : the option's latest value is in the process of being activated.     * @Active@ : the option's latest value is completely deployed.    * @FailedToValidate@ : the option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\s a -> s {_osState = a})

instance FromXML OptionStatus where
  parseXML x =
    OptionStatus'
      <$> (x .@? "PendingDeletion")
      <*> (x .@? "UpdateVersion")
      <*> (x .@ "CreationDate")
      <*> (x .@ "UpdateDate")
      <*> (x .@ "State")

instance Hashable OptionStatus

instance NFData OptionStatus
