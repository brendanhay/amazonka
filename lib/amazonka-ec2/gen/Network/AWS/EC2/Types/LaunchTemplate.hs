{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplate where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template.
--
--
--
-- /See:/ 'launchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { _ltLaunchTemplateName ::
      !(Maybe Text),
    _ltLatestVersionNumber :: !(Maybe Integer),
    _ltLaunchTemplateId :: !(Maybe Text),
    _ltCreatedBy :: !(Maybe Text),
    _ltDefaultVersionNumber :: !(Maybe Integer),
    _ltCreateTime :: !(Maybe ISO8601),
    _ltTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltLaunchTemplateName' - The name of the launch template.
--
-- * 'ltLatestVersionNumber' - The version number of the latest version of the launch template.
--
-- * 'ltLaunchTemplateId' - The ID of the launch template.
--
-- * 'ltCreatedBy' - The principal that created the launch template.
--
-- * 'ltDefaultVersionNumber' - The version number of the default version of the launch template.
--
-- * 'ltCreateTime' - The time launch template was created.
--
-- * 'ltTags' - The tags for the launch template.
launchTemplate ::
  LaunchTemplate
launchTemplate =
  LaunchTemplate'
    { _ltLaunchTemplateName = Nothing,
      _ltLatestVersionNumber = Nothing,
      _ltLaunchTemplateId = Nothing,
      _ltCreatedBy = Nothing,
      _ltDefaultVersionNumber = Nothing,
      _ltCreateTime = Nothing,
      _ltTags = Nothing
    }

-- | The name of the launch template.
ltLaunchTemplateName :: Lens' LaunchTemplate (Maybe Text)
ltLaunchTemplateName = lens _ltLaunchTemplateName (\s a -> s {_ltLaunchTemplateName = a})

-- | The version number of the latest version of the launch template.
ltLatestVersionNumber :: Lens' LaunchTemplate (Maybe Integer)
ltLatestVersionNumber = lens _ltLatestVersionNumber (\s a -> s {_ltLatestVersionNumber = a})

-- | The ID of the launch template.
ltLaunchTemplateId :: Lens' LaunchTemplate (Maybe Text)
ltLaunchTemplateId = lens _ltLaunchTemplateId (\s a -> s {_ltLaunchTemplateId = a})

-- | The principal that created the launch template.
ltCreatedBy :: Lens' LaunchTemplate (Maybe Text)
ltCreatedBy = lens _ltCreatedBy (\s a -> s {_ltCreatedBy = a})

-- | The version number of the default version of the launch template.
ltDefaultVersionNumber :: Lens' LaunchTemplate (Maybe Integer)
ltDefaultVersionNumber = lens _ltDefaultVersionNumber (\s a -> s {_ltDefaultVersionNumber = a})

-- | The time launch template was created.
ltCreateTime :: Lens' LaunchTemplate (Maybe UTCTime)
ltCreateTime = lens _ltCreateTime (\s a -> s {_ltCreateTime = a}) . mapping _Time

-- | The tags for the launch template.
ltTags :: Lens' LaunchTemplate [Tag]
ltTags = lens _ltTags (\s a -> s {_ltTags = a}) . _Default . _Coerce

instance FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      <$> (x .@? "launchTemplateName")
      <*> (x .@? "latestVersionNumber")
      <*> (x .@? "launchTemplateId")
      <*> (x .@? "createdBy")
      <*> (x .@? "defaultVersionNumber")
      <*> (x .@? "createTime")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LaunchTemplate

instance NFData LaunchTemplate
