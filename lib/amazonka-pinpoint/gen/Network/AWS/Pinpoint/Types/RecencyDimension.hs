{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecencyDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecencyDimension where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Duration
import Network.AWS.Pinpoint.Types.RecencyType
import Network.AWS.Prelude

-- | Specifies criteria for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
--
--
-- /See:/ 'recencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
  { _rdDuration :: !Duration,
    _rdRecencyType :: !RecencyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecencyDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdDuration' - The duration to use when determining whether an endpoint is active or inactive.
--
-- * 'rdRecencyType' - The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
recencyDimension ::
  -- | 'rdDuration'
  Duration ->
  -- | 'rdRecencyType'
  RecencyType ->
  RecencyDimension
recencyDimension pDuration_ pRecencyType_ =
  RecencyDimension'
    { _rdDuration = pDuration_,
      _rdRecencyType = pRecencyType_
    }

-- | The duration to use when determining whether an endpoint is active or inactive.
rdDuration :: Lens' RecencyDimension Duration
rdDuration = lens _rdDuration (\s a -> s {_rdDuration = a})

-- | The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
rdRecencyType :: Lens' RecencyDimension RecencyType
rdRecencyType = lens _rdRecencyType (\s a -> s {_rdRecencyType = a})

instance FromJSON RecencyDimension where
  parseJSON =
    withObject
      "RecencyDimension"
      ( \x ->
          RecencyDimension' <$> (x .: "Duration") <*> (x .: "RecencyType")
      )

instance Hashable RecencyDimension

instance NFData RecencyDimension

instance ToJSON RecencyDimension where
  toJSON RecencyDimension' {..} =
    object
      ( catMaybes
          [ Just ("Duration" .= _rdDuration),
            Just ("RecencyType" .= _rdRecencyType)
          ]
      )
