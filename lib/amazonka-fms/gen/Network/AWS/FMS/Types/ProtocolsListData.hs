{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An AWS Firewall Manager protocols list.
--
--
--
-- /See:/ 'protocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { _pldListUpdateToken ::
      !(Maybe Text),
    _pldListId :: !(Maybe Text),
    _pldLastUpdateTime :: !(Maybe POSIX),
    _pldPreviousProtocolsList ::
      !(Maybe (Map Text ([Text]))),
    _pldCreateTime :: !(Maybe POSIX),
    _pldListName :: !Text,
    _pldProtocolsList :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtocolsListData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pldListUpdateToken' - A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
--
-- * 'pldListId' - The ID of the AWS Firewall Manager protocols list.
--
-- * 'pldLastUpdateTime' - The time that the AWS Firewall Manager protocols list was last updated.
--
-- * 'pldPreviousProtocolsList' - A map of previous version numbers to their corresponding protocol arrays.
--
-- * 'pldCreateTime' - The time that the AWS Firewall Manager protocols list was created.
--
-- * 'pldListName' - The name of the AWS Firewall Manager protocols list.
--
-- * 'pldProtocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
protocolsListData ::
  -- | 'pldListName'
  Text ->
  ProtocolsListData
protocolsListData pListName_ =
  ProtocolsListData'
    { _pldListUpdateToken = Nothing,
      _pldListId = Nothing,
      _pldLastUpdateTime = Nothing,
      _pldPreviousProtocolsList = Nothing,
      _pldCreateTime = Nothing,
      _pldListName = pListName_,
      _pldProtocolsList = mempty
    }

-- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
pldListUpdateToken :: Lens' ProtocolsListData (Maybe Text)
pldListUpdateToken = lens _pldListUpdateToken (\s a -> s {_pldListUpdateToken = a})

-- | The ID of the AWS Firewall Manager protocols list.
pldListId :: Lens' ProtocolsListData (Maybe Text)
pldListId = lens _pldListId (\s a -> s {_pldListId = a})

-- | The time that the AWS Firewall Manager protocols list was last updated.
pldLastUpdateTime :: Lens' ProtocolsListData (Maybe UTCTime)
pldLastUpdateTime = lens _pldLastUpdateTime (\s a -> s {_pldLastUpdateTime = a}) . mapping _Time

-- | A map of previous version numbers to their corresponding protocol arrays.
pldPreviousProtocolsList :: Lens' ProtocolsListData (HashMap Text ([Text]))
pldPreviousProtocolsList = lens _pldPreviousProtocolsList (\s a -> s {_pldPreviousProtocolsList = a}) . _Default . _Map

-- | The time that the AWS Firewall Manager protocols list was created.
pldCreateTime :: Lens' ProtocolsListData (Maybe UTCTime)
pldCreateTime = lens _pldCreateTime (\s a -> s {_pldCreateTime = a}) . mapping _Time

-- | The name of the AWS Firewall Manager protocols list.
pldListName :: Lens' ProtocolsListData Text
pldListName = lens _pldListName (\s a -> s {_pldListName = a})

-- | An array of protocols in the AWS Firewall Manager protocols list.
pldProtocolsList :: Lens' ProtocolsListData [Text]
pldProtocolsList = lens _pldProtocolsList (\s a -> s {_pldProtocolsList = a}) . _Coerce

instance FromJSON ProtocolsListData where
  parseJSON =
    withObject
      "ProtocolsListData"
      ( \x ->
          ProtocolsListData'
            <$> (x .:? "ListUpdateToken")
            <*> (x .:? "ListId")
            <*> (x .:? "LastUpdateTime")
            <*> (x .:? "PreviousProtocolsList" .!= mempty)
            <*> (x .:? "CreateTime")
            <*> (x .: "ListName")
            <*> (x .:? "ProtocolsList" .!= mempty)
      )

instance Hashable ProtocolsListData

instance NFData ProtocolsListData

instance ToJSON ProtocolsListData where
  toJSON ProtocolsListData' {..} =
    object
      ( catMaybes
          [ ("ListUpdateToken" .=) <$> _pldListUpdateToken,
            ("ListId" .=) <$> _pldListId,
            ("LastUpdateTime" .=) <$> _pldLastUpdateTime,
            ("PreviousProtocolsList" .=) <$> _pldPreviousProtocolsList,
            ("CreateTime" .=) <$> _pldCreateTime,
            Just ("ListName" .= _pldListName),
            Just ("ProtocolsList" .= _pldProtocolsList)
          ]
      )
