{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationStatusName

-- | Describes an association status.
--
--
--
-- /See:/ 'associationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { _asAdditionalInfo ::
      !(Maybe Text),
    _asDate :: !POSIX,
    _asName :: !AssociationStatusName,
    _asMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAdditionalInfo' - A user-defined string.
--
-- * 'asDate' - The date when the status changed.
--
-- * 'asName' - The status.
--
-- * 'asMessage' - The reason for the status.
associationStatus ::
  -- | 'asDate'
  UTCTime ->
  -- | 'asName'
  AssociationStatusName ->
  -- | 'asMessage'
  Text ->
  AssociationStatus
associationStatus pDate_ pName_ pMessage_ =
  AssociationStatus'
    { _asAdditionalInfo = Nothing,
      _asDate = _Time # pDate_,
      _asName = pName_,
      _asMessage = pMessage_
    }

-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\s a -> s {_asAdditionalInfo = a})

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\s a -> s {_asDate = a}) . _Time

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\s a -> s {_asName = a})

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\s a -> s {_asMessage = a})

instance FromJSON AssociationStatus where
  parseJSON =
    withObject
      "AssociationStatus"
      ( \x ->
          AssociationStatus'
            <$> (x .:? "AdditionalInfo")
            <*> (x .: "Date")
            <*> (x .: "Name")
            <*> (x .: "Message")
      )

instance Hashable AssociationStatus

instance NFData AssociationStatus

instance ToJSON AssociationStatus where
  toJSON AssociationStatus' {..} =
    object
      ( catMaybes
          [ ("AdditionalInfo" .=) <$> _asAdditionalInfo,
            Just ("Date" .= _asDate),
            Just ("Name" .= _asName),
            Just ("Message" .= _asMessage)
          ]
      )
