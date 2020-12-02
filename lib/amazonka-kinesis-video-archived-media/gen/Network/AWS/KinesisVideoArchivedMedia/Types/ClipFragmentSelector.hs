{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector where

import Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType
import Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
--
-- Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the clip will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned.
--
--
-- /See:/ 'clipFragmentSelector' smart constructor.
data ClipFragmentSelector = ClipFragmentSelector'
  { _cfsFragmentSelectorType ::
      !ClipFragmentSelectorType,
    _cfsTimestampRange :: !ClipTimestampRange
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClipFragmentSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsFragmentSelectorType' - The origin of the timestamps to use (Server or Producer).
--
-- * 'cfsTimestampRange' - The range of timestamps to return.
clipFragmentSelector ::
  -- | 'cfsFragmentSelectorType'
  ClipFragmentSelectorType ->
  -- | 'cfsTimestampRange'
  ClipTimestampRange ->
  ClipFragmentSelector
clipFragmentSelector pFragmentSelectorType_ pTimestampRange_ =
  ClipFragmentSelector'
    { _cfsFragmentSelectorType =
        pFragmentSelectorType_,
      _cfsTimestampRange = pTimestampRange_
    }

-- | The origin of the timestamps to use (Server or Producer).
cfsFragmentSelectorType :: Lens' ClipFragmentSelector ClipFragmentSelectorType
cfsFragmentSelectorType = lens _cfsFragmentSelectorType (\s a -> s {_cfsFragmentSelectorType = a})

-- | The range of timestamps to return.
cfsTimestampRange :: Lens' ClipFragmentSelector ClipTimestampRange
cfsTimestampRange = lens _cfsTimestampRange (\s a -> s {_cfsTimestampRange = a})

instance Hashable ClipFragmentSelector

instance NFData ClipFragmentSelector

instance ToJSON ClipFragmentSelector where
  toJSON ClipFragmentSelector' {..} =
    object
      ( catMaybes
          [ Just ("FragmentSelectorType" .= _cfsFragmentSelectorType),
            Just ("TimestampRange" .= _cfsTimestampRange)
          ]
      )
