{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Destination where

import Network.AWS.IoT.Types.S3Destination
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the location of the updated firmware.
--
--
--
-- /See:/ 'destination' smart constructor.
newtype Destination = Destination'
  { _dS3Destination ::
      Maybe S3Destination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dS3Destination' - Describes the location in S3 of the updated firmware.
destination ::
  Destination
destination = Destination' {_dS3Destination = Nothing}

-- | Describes the location in S3 of the updated firmware.
dS3Destination :: Lens' Destination (Maybe S3Destination)
dS3Destination = lens _dS3Destination (\s a -> s {_dS3Destination = a})

instance FromJSON Destination where
  parseJSON =
    withObject
      "Destination"
      (\x -> Destination' <$> (x .:? "s3Destination"))

instance Hashable Destination

instance NFData Destination

instance ToJSON Destination where
  toJSON Destination' {..} =
    object (catMaybes [("s3Destination" .=) <$> _dS3Destination])
