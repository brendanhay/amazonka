{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentGroupList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentGroupList where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Include
import Network.AWS.Pinpoint.Types.SegmentGroup
import Network.AWS.Prelude

-- | Specifies the settings that define the relationships between segment groups for a segment.
--
--
--
-- /See:/ 'segmentGroupList' smart constructor.
data SegmentGroupList = SegmentGroupList'
  { _sglInclude ::
      !(Maybe Include),
    _sglGroups :: !(Maybe [SegmentGroup])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentGroupList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sglInclude' - Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
--
-- * 'sglGroups' - An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
segmentGroupList ::
  SegmentGroupList
segmentGroupList =
  SegmentGroupList' {_sglInclude = Nothing, _sglGroups = Nothing}

-- | Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
sglInclude :: Lens' SegmentGroupList (Maybe Include)
sglInclude = lens _sglInclude (\s a -> s {_sglInclude = a})

-- | An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
sglGroups :: Lens' SegmentGroupList [SegmentGroup]
sglGroups = lens _sglGroups (\s a -> s {_sglGroups = a}) . _Default . _Coerce

instance FromJSON SegmentGroupList where
  parseJSON =
    withObject
      "SegmentGroupList"
      ( \x ->
          SegmentGroupList'
            <$> (x .:? "Include") <*> (x .:? "Groups" .!= mempty)
      )

instance Hashable SegmentGroupList

instance NFData SegmentGroupList

instance ToJSON SegmentGroupList where
  toJSON SegmentGroupList' {..} =
    object
      ( catMaybes
          [("Include" .=) <$> _sglInclude, ("Groups" .=) <$> _sglGroups]
      )
