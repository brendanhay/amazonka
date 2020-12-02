{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.TimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
--
-- Only fragments with a start timestamp greater than or equal to the given start time and less than or equal to the end time are returned. For example, if a stream contains fragments with the following start timestamps:
--
--     * 00:00:00
--
--     * 00:00:02
--
--     * 00:00:04
--
--     * 00:00:06
--
--
--
-- A fragment selector range with a start time of 00:00:01 and end time of 00:00:04 would return the fragments with start times of 00:00:02 and 00:00:04.
--
--
-- /See:/ 'fragmentSelector' smart constructor.
data FragmentSelector = FragmentSelector'
  { _fsFragmentSelectorType ::
      !FragmentSelectorType,
    _fsTimestampRange :: !TimestampRange
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsFragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
--
-- * 'fsTimestampRange' - The range of timestamps to return.
fragmentSelector ::
  -- | 'fsFragmentSelectorType'
  FragmentSelectorType ->
  -- | 'fsTimestampRange'
  TimestampRange ->
  FragmentSelector
fragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  FragmentSelector'
    { _fsFragmentSelectorType =
        pFragmentSelectorType_,
      _fsTimestampRange = pTimestampRange_
    }

-- | The origin of the timestamps to use (Server or Producer).
fsFragmentSelectorType :: Lens' FragmentSelector FragmentSelectorType
fsFragmentSelectorType = lens _fsFragmentSelectorType (\s a -> s {_fsFragmentSelectorType = a})

-- | The range of timestamps to return.
fsTimestampRange :: Lens' FragmentSelector TimestampRange
fsTimestampRange = lens _fsTimestampRange (\s a -> s {_fsTimestampRange = a})

instance Hashable FragmentSelector

instance NFData FragmentSelector

instance ToJSON FragmentSelector where
  toJSON FragmentSelector' {..} =
    object
      ( catMaybes
          [ Just ("FragmentSelectorType" .= _fsFragmentSelectorType),
            Just ("TimestampRange" .= _fsTimestampRange)
          ]
      )
