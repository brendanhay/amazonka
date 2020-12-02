{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteSegmentRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteSegmentRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroupList
import Network.AWS.Prelude

-- | Specifies the configuration, dimension, and other settings for a segment. A WriteSegmentRequest object can include a Dimensions object or a SegmentGroups object, but not both.
--
--
--
-- /See:/ 'writeSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { _wsrSegmentGroups ::
      !(Maybe SegmentGroupList),
    _wsrName :: !(Maybe Text),
    _wsrDimensions :: !(Maybe SegmentDimensions),
    _wsrTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WriteSegmentRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsrSegmentGroups' - The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
--
-- * 'wsrName' - The name of the segment.
--
-- * 'wsrDimensions' - The criteria that define the dimensions for the segment.
--
-- * 'wsrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
writeSegmentRequest ::
  WriteSegmentRequest
writeSegmentRequest =
  WriteSegmentRequest'
    { _wsrSegmentGroups = Nothing,
      _wsrName = Nothing,
      _wsrDimensions = Nothing,
      _wsrTags = Nothing
    }

-- | The segment group to use and the dimensions to apply to the group's base segments in order to build the segment. A segment group can consist of zero or more base segments. Your request can include only one segment group.
wsrSegmentGroups :: Lens' WriteSegmentRequest (Maybe SegmentGroupList)
wsrSegmentGroups = lens _wsrSegmentGroups (\s a -> s {_wsrSegmentGroups = a})

-- | The name of the segment.
wsrName :: Lens' WriteSegmentRequest (Maybe Text)
wsrName = lens _wsrName (\s a -> s {_wsrName = a})

-- | The criteria that define the dimensions for the segment.
wsrDimensions :: Lens' WriteSegmentRequest (Maybe SegmentDimensions)
wsrDimensions = lens _wsrDimensions (\s a -> s {_wsrDimensions = a})

-- | A string-to-string map of key-value pairs that defines the tags to associate with the segment. Each tag consists of a required tag key and an associated tag value.
wsrTags :: Lens' WriteSegmentRequest (HashMap Text (Text))
wsrTags = lens _wsrTags (\s a -> s {_wsrTags = a}) . _Default . _Map

instance Hashable WriteSegmentRequest

instance NFData WriteSegmentRequest

instance ToJSON WriteSegmentRequest where
  toJSON WriteSegmentRequest' {..} =
    object
      ( catMaybes
          [ ("SegmentGroups" .=) <$> _wsrSegmentGroups,
            ("Name" .=) <$> _wsrName,
            ("Dimensions" .=) <$> _wsrDimensions,
            ("tags" .=) <$> _wsrTags
          ]
      )
