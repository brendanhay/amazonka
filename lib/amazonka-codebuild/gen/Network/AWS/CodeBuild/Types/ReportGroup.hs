{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroup where

import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportGroupStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A series of reports. Each report contains information about the results from running a series of test cases. You specify the test cases for a report group in the buildspec for a build project using one or more paths to the test case files.
--
--
--
-- /See:/ 'reportGroup' smart constructor.
data ReportGroup = ReportGroup'
  { _rgStatus ::
      !(Maybe ReportGroupStatusType),
    _rgArn :: !(Maybe Text),
    _rgCreated :: !(Maybe POSIX),
    _rgName :: !(Maybe Text),
    _rgType :: !(Maybe ReportType),
    _rgLastModified :: !(Maybe POSIX),
    _rgExportConfig :: !(Maybe ReportExportConfig),
    _rgTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReportGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgStatus' - Undocumented member.
--
-- * 'rgArn' - The ARN of a @ReportGroup@ .
--
-- * 'rgCreated' - The date and time this @ReportGroup@ was created.
--
-- * 'rgName' - The name of a @ReportGroup@ .
--
-- * 'rgType' - The type of the @ReportGroup@ . The one valid value is @TEST@ .
--
-- * 'rgLastModified' - The date and time this @ReportGroup@ was last modified.
--
-- * 'rgExportConfig' - Information about the destination where the raw data of this @ReportGroup@ is exported.
--
-- * 'rgTags' - A list of tag key and value pairs associated with this report group.  These tags are available for use by AWS services that support AWS CodeBuild report group tags.
reportGroup ::
  ReportGroup
reportGroup =
  ReportGroup'
    { _rgStatus = Nothing,
      _rgArn = Nothing,
      _rgCreated = Nothing,
      _rgName = Nothing,
      _rgType = Nothing,
      _rgLastModified = Nothing,
      _rgExportConfig = Nothing,
      _rgTags = Nothing
    }

-- | Undocumented member.
rgStatus :: Lens' ReportGroup (Maybe ReportGroupStatusType)
rgStatus = lens _rgStatus (\s a -> s {_rgStatus = a})

-- | The ARN of a @ReportGroup@ .
rgArn :: Lens' ReportGroup (Maybe Text)
rgArn = lens _rgArn (\s a -> s {_rgArn = a})

-- | The date and time this @ReportGroup@ was created.
rgCreated :: Lens' ReportGroup (Maybe UTCTime)
rgCreated = lens _rgCreated (\s a -> s {_rgCreated = a}) . mapping _Time

-- | The name of a @ReportGroup@ .
rgName :: Lens' ReportGroup (Maybe Text)
rgName = lens _rgName (\s a -> s {_rgName = a})

-- | The type of the @ReportGroup@ . The one valid value is @TEST@ .
rgType :: Lens' ReportGroup (Maybe ReportType)
rgType = lens _rgType (\s a -> s {_rgType = a})

-- | The date and time this @ReportGroup@ was last modified.
rgLastModified :: Lens' ReportGroup (Maybe UTCTime)
rgLastModified = lens _rgLastModified (\s a -> s {_rgLastModified = a}) . mapping _Time

-- | Information about the destination where the raw data of this @ReportGroup@ is exported.
rgExportConfig :: Lens' ReportGroup (Maybe ReportExportConfig)
rgExportConfig = lens _rgExportConfig (\s a -> s {_rgExportConfig = a})

-- | A list of tag key and value pairs associated with this report group.  These tags are available for use by AWS services that support AWS CodeBuild report group tags.
rgTags :: Lens' ReportGroup [Tag]
rgTags = lens _rgTags (\s a -> s {_rgTags = a}) . _Default . _Coerce

instance FromJSON ReportGroup where
  parseJSON =
    withObject
      "ReportGroup"
      ( \x ->
          ReportGroup'
            <$> (x .:? "status")
            <*> (x .:? "arn")
            <*> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "type")
            <*> (x .:? "lastModified")
            <*> (x .:? "exportConfig")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable ReportGroup

instance NFData ReportGroup
