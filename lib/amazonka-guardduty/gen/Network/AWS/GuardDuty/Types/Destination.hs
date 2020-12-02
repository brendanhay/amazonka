{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Destination where

import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.PublishingStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the publishing destination, including the ID, type, and status.
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dDestinationId :: !Text,
    _dDestinationType :: !DestinationType,
    _dStatus :: !PublishingStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDestinationId' - The unique ID of the publishing destination.
--
-- * 'dDestinationType' - The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- * 'dStatus' - The status of the publishing destination.
destination ::
  -- | 'dDestinationId'
  Text ->
  -- | 'dDestinationType'
  DestinationType ->
  -- | 'dStatus'
  PublishingStatus ->
  Destination
destination pDestinationId_ pDestinationType_ pStatus_ =
  Destination'
    { _dDestinationId = pDestinationId_,
      _dDestinationType = pDestinationType_,
      _dStatus = pStatus_
    }

-- | The unique ID of the publishing destination.
dDestinationId :: Lens' Destination Text
dDestinationId = lens _dDestinationId (\s a -> s {_dDestinationId = a})

-- | The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
dDestinationType :: Lens' Destination DestinationType
dDestinationType = lens _dDestinationType (\s a -> s {_dDestinationType = a})

-- | The status of the publishing destination.
dStatus :: Lens' Destination PublishingStatus
dStatus = lens _dStatus (\s a -> s {_dStatus = a})

instance FromJSON Destination where
  parseJSON =
    withObject
      "Destination"
      ( \x ->
          Destination'
            <$> (x .: "destinationId")
            <*> (x .: "destinationType")
            <*> (x .: "status")
      )

instance Hashable Destination

instance NFData Destination
