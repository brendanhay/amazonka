{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListDataSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the AWS Firewall Manager protocols list.
--
--
--
-- /See:/ 'protocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { _pldsProtocolsList ::
      !(Maybe [Text]),
    _pldsListARN :: !(Maybe Text),
    _pldsListId :: !(Maybe Text),
    _pldsListName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProtocolsListDataSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pldsProtocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
--
-- * 'pldsListARN' - The Amazon Resource Name (ARN) of the specified protocols list.
--
-- * 'pldsListId' - The ID of the specified protocols list.
--
-- * 'pldsListName' - The name of the specified protocols list.
protocolsListDataSummary ::
  ProtocolsListDataSummary
protocolsListDataSummary =
  ProtocolsListDataSummary'
    { _pldsProtocolsList = Nothing,
      _pldsListARN = Nothing,
      _pldsListId = Nothing,
      _pldsListName = Nothing
    }

-- | An array of protocols in the AWS Firewall Manager protocols list.
pldsProtocolsList :: Lens' ProtocolsListDataSummary [Text]
pldsProtocolsList = lens _pldsProtocolsList (\s a -> s {_pldsProtocolsList = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the specified protocols list.
pldsListARN :: Lens' ProtocolsListDataSummary (Maybe Text)
pldsListARN = lens _pldsListARN (\s a -> s {_pldsListARN = a})

-- | The ID of the specified protocols list.
pldsListId :: Lens' ProtocolsListDataSummary (Maybe Text)
pldsListId = lens _pldsListId (\s a -> s {_pldsListId = a})

-- | The name of the specified protocols list.
pldsListName :: Lens' ProtocolsListDataSummary (Maybe Text)
pldsListName = lens _pldsListName (\s a -> s {_pldsListName = a})

instance FromJSON ProtocolsListDataSummary where
  parseJSON =
    withObject
      "ProtocolsListDataSummary"
      ( \x ->
          ProtocolsListDataSummary'
            <$> (x .:? "ProtocolsList" .!= mempty)
            <*> (x .:? "ListArn")
            <*> (x .:? "ListId")
            <*> (x .:? "ListName")
      )

instance Hashable ProtocolsListDataSummary

instance NFData ProtocolsListDataSummary
