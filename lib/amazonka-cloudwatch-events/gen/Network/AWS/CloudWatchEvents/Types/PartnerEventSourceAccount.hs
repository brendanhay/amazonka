{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount where

import Network.AWS.CloudWatchEvents.Types.EventSourceState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS account that a partner event source has been offered to.
--
--
--
-- /See:/ 'partnerEventSourceAccount' smart constructor.
data PartnerEventSourceAccount = PartnerEventSourceAccount'
  { _pesaCreationTime ::
      !(Maybe POSIX),
    _pesaState :: !(Maybe EventSourceState),
    _pesaAccount :: !(Maybe Text),
    _pesaExpirationTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartnerEventSourceAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pesaCreationTime' - The date and time the event source was created.
--
-- * 'pesaState' - The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
--
-- * 'pesaAccount' - The AWS account ID that the partner event source was offered to.
--
-- * 'pesaExpirationTime' - The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
partnerEventSourceAccount ::
  PartnerEventSourceAccount
partnerEventSourceAccount =
  PartnerEventSourceAccount'
    { _pesaCreationTime = Nothing,
      _pesaState = Nothing,
      _pesaAccount = Nothing,
      _pesaExpirationTime = Nothing
    }

-- | The date and time the event source was created.
pesaCreationTime :: Lens' PartnerEventSourceAccount (Maybe UTCTime)
pesaCreationTime = lens _pesaCreationTime (\s a -> s {_pesaCreationTime = a}) . mapping _Time

-- | The state of the event source. If it is ACTIVE, you have already created a matching event bus for this event source, and that event bus is active. If it is PENDING, either you haven't yet created a matching event bus, or that event bus is deactivated. If it is DELETED, you have created a matching event bus, but the event source has since been deleted.
pesaState :: Lens' PartnerEventSourceAccount (Maybe EventSourceState)
pesaState = lens _pesaState (\s a -> s {_pesaState = a})

-- | The AWS account ID that the partner event source was offered to.
pesaAccount :: Lens' PartnerEventSourceAccount (Maybe Text)
pesaAccount = lens _pesaAccount (\s a -> s {_pesaAccount = a})

-- | The date and time that the event source will expire, if the AWS account doesn't create a matching event bus for it.
pesaExpirationTime :: Lens' PartnerEventSourceAccount (Maybe UTCTime)
pesaExpirationTime = lens _pesaExpirationTime (\s a -> s {_pesaExpirationTime = a}) . mapping _Time

instance FromJSON PartnerEventSourceAccount where
  parseJSON =
    withObject
      "PartnerEventSourceAccount"
      ( \x ->
          PartnerEventSourceAccount'
            <$> (x .:? "CreationTime")
            <*> (x .:? "State")
            <*> (x .:? "Account")
            <*> (x .:? "ExpirationTime")
      )

instance Hashable PartnerEventSourceAccount

instance NFData PartnerEventSourceAccount
