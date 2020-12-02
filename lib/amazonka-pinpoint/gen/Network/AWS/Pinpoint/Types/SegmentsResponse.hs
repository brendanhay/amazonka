{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentsResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.SegmentResponse
import Network.AWS.Prelude

-- | Provides information about all the segments that are associated with an application.
--
--
--
-- /See:/ 'segmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
  { _sNextToken ::
      !(Maybe Text),
    _sItem :: ![SegmentResponse]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- * 'sItem' - An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
segmentsResponse ::
  SegmentsResponse
segmentsResponse =
  SegmentsResponse' {_sNextToken = Nothing, _sItem = mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
sNextToken :: Lens' SegmentsResponse (Maybe Text)
sNextToken = lens _sNextToken (\s a -> s {_sNextToken = a})

-- | An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
sItem :: Lens' SegmentsResponse [SegmentResponse]
sItem = lens _sItem (\s a -> s {_sItem = a}) . _Coerce

instance FromJSON SegmentsResponse where
  parseJSON =
    withObject
      "SegmentsResponse"
      ( \x ->
          SegmentsResponse'
            <$> (x .:? "NextToken") <*> (x .:? "Item" .!= mempty)
      )

instance Hashable SegmentsResponse

instance NFData SegmentsResponse
