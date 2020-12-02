{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attachment where

import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a container instance or task attachment.
--
--
--
-- /See:/ 'attachment' smart constructor.
data Attachment = Attachment'
  { _aStatus :: !(Maybe Text),
    _aDetails :: !(Maybe [KeyValuePair]),
    _aId :: !(Maybe Text),
    _aType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aStatus' - The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
--
-- * 'aDetails' - Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
--
-- * 'aId' - The unique identifier for the attachment.
--
-- * 'aType' - The type of the attachment, such as @ElasticNetworkInterface@ .
attachment ::
  Attachment
attachment =
  Attachment'
    { _aStatus = Nothing,
      _aDetails = Nothing,
      _aId = Nothing,
      _aType = Nothing
    }

-- | The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
aStatus :: Lens' Attachment (Maybe Text)
aStatus = lens _aStatus (\s a -> s {_aStatus = a})

-- | Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
aDetails :: Lens' Attachment [KeyValuePair]
aDetails = lens _aDetails (\s a -> s {_aDetails = a}) . _Default . _Coerce

-- | The unique identifier for the attachment.
aId :: Lens' Attachment (Maybe Text)
aId = lens _aId (\s a -> s {_aId = a})

-- | The type of the attachment, such as @ElasticNetworkInterface@ .
aType :: Lens' Attachment (Maybe Text)
aType = lens _aType (\s a -> s {_aType = a})

instance FromJSON Attachment where
  parseJSON =
    withObject
      "Attachment"
      ( \x ->
          Attachment'
            <$> (x .:? "status")
            <*> (x .:? "details" .!= mempty)
            <*> (x .:? "id")
            <*> (x .:? "type")
      )

instance Hashable Attachment

instance NFData Attachment
