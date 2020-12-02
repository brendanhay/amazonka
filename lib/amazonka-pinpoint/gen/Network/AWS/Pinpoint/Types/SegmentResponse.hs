{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroupList
import Network.AWS.Pinpoint.Types.SegmentImportResource
import Network.AWS.Pinpoint.Types.SegmentType
import Network.AWS.Prelude

-- | Provides information about the configuration, dimension, and other settings for a segment.
--
--
--
-- /See:/ 'segmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
  { _sLastModifiedDate ::
      !(Maybe Text),
    _sSegmentGroups :: !(Maybe SegmentGroupList),
    _sName :: !(Maybe Text),
    _sVersion :: !(Maybe Int),
    _sImportDefinition :: !(Maybe SegmentImportResource),
    _sDimensions :: !(Maybe SegmentDimensions),
    _sTags :: !(Maybe (Map Text (Text))),
    _sSegmentType :: !SegmentType,
    _sCreationDate :: !Text,
    _sId :: !Text,
    _sARN :: !Text,
    _sApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sLastModifiedDate' - The date and time when the segment was last modified.
--
-- * 'sSegmentGroups' - A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
--
-- * 'sName' - The name of the segment.
--
-- * 'sVersion' - The version number of the segment.
--
-- * 'sImportDefinition' - The settings for the import job that's associated with the segment.
--
-- * 'sDimensions' - The dimension settings for the segment.
--
-- * 'sTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
--
-- * 'sSegmentType' - The segment type. Valid values are:     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
--
-- * 'sCreationDate' - The date and time when the segment was created.
--
-- * 'sId' - The unique identifier for the segment.
--
-- * 'sARN' - The Amazon Resource Name (ARN) of the segment.
--
-- * 'sApplicationId' - The unique identifier for the application that the segment is associated with.
segmentResponse ::
  -- | 'sSegmentType'
  SegmentType ->
  -- | 'sCreationDate'
  Text ->
  -- | 'sId'
  Text ->
  -- | 'sARN'
  Text ->
  -- | 'sApplicationId'
  Text ->
  SegmentResponse
segmentResponse
  pSegmentType_
  pCreationDate_
  pId_
  pARN_
  pApplicationId_ =
    SegmentResponse'
      { _sLastModifiedDate = Nothing,
        _sSegmentGroups = Nothing,
        _sName = Nothing,
        _sVersion = Nothing,
        _sImportDefinition = Nothing,
        _sDimensions = Nothing,
        _sTags = Nothing,
        _sSegmentType = pSegmentType_,
        _sCreationDate = pCreationDate_,
        _sId = pId_,
        _sARN = pARN_,
        _sApplicationId = pApplicationId_
      }

-- | The date and time when the segment was last modified.
sLastModifiedDate :: Lens' SegmentResponse (Maybe Text)
sLastModifiedDate = lens _sLastModifiedDate (\s a -> s {_sLastModifiedDate = a})

-- | A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
sSegmentGroups :: Lens' SegmentResponse (Maybe SegmentGroupList)
sSegmentGroups = lens _sSegmentGroups (\s a -> s {_sSegmentGroups = a})

-- | The name of the segment.
sName :: Lens' SegmentResponse (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The version number of the segment.
sVersion :: Lens' SegmentResponse (Maybe Int)
sVersion = lens _sVersion (\s a -> s {_sVersion = a})

-- | The settings for the import job that's associated with the segment.
sImportDefinition :: Lens' SegmentResponse (Maybe SegmentImportResource)
sImportDefinition = lens _sImportDefinition (\s a -> s {_sImportDefinition = a})

-- | The dimension settings for the segment.
sDimensions :: Lens' SegmentResponse (Maybe SegmentDimensions)
sDimensions = lens _sDimensions (\s a -> s {_sDimensions = a})

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
sTags :: Lens' SegmentResponse (HashMap Text (Text))
sTags = lens _sTags (\s a -> s {_sTags = a}) . _Default . _Map

-- | The segment type. Valid values are:     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
sSegmentType :: Lens' SegmentResponse SegmentType
sSegmentType = lens _sSegmentType (\s a -> s {_sSegmentType = a})

-- | The date and time when the segment was created.
sCreationDate :: Lens' SegmentResponse Text
sCreationDate = lens _sCreationDate (\s a -> s {_sCreationDate = a})

-- | The unique identifier for the segment.
sId :: Lens' SegmentResponse Text
sId = lens _sId (\s a -> s {_sId = a})

-- | The Amazon Resource Name (ARN) of the segment.
sARN :: Lens' SegmentResponse Text
sARN = lens _sARN (\s a -> s {_sARN = a})

-- | The unique identifier for the application that the segment is associated with.
sApplicationId :: Lens' SegmentResponse Text
sApplicationId = lens _sApplicationId (\s a -> s {_sApplicationId = a})

instance FromJSON SegmentResponse where
  parseJSON =
    withObject
      "SegmentResponse"
      ( \x ->
          SegmentResponse'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "SegmentGroups")
            <*> (x .:? "Name")
            <*> (x .:? "Version")
            <*> (x .:? "ImportDefinition")
            <*> (x .:? "Dimensions")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "SegmentType")
            <*> (x .: "CreationDate")
            <*> (x .: "Id")
            <*> (x .: "Arn")
            <*> (x .: "ApplicationId")
      )

instance Hashable SegmentResponse

instance NFData SegmentResponse
