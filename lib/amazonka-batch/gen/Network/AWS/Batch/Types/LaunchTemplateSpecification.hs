{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LaunchTemplateSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a launch template associated with a compute resource. You must specify either the launch template ID or launch template name in the request, but not both.
--
--
--
-- /See:/ 'launchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { _ltsLaunchTemplateName ::
      !(Maybe Text),
    _ltsLaunchTemplateId ::
      !(Maybe Text),
    _ltsVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsLaunchTemplateName' - The name of the launch template.
--
-- * 'ltsLaunchTemplateId' - The ID of the launch template.
--
-- * 'ltsVersion' - The version number of the launch template, @> Latest@ , or @> Default@ . If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used. Default: @> Default@ .
launchTemplateSpecification ::
  LaunchTemplateSpecification
launchTemplateSpecification =
  LaunchTemplateSpecification'
    { _ltsLaunchTemplateName = Nothing,
      _ltsLaunchTemplateId = Nothing,
      _ltsVersion = Nothing
    }

-- | The name of the launch template.
ltsLaunchTemplateName :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateName = lens _ltsLaunchTemplateName (\s a -> s {_ltsLaunchTemplateName = a})

-- | The ID of the launch template.
ltsLaunchTemplateId :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsLaunchTemplateId = lens _ltsLaunchTemplateId (\s a -> s {_ltsLaunchTemplateId = a})

-- | The version number of the launch template, @> Latest@ , or @> Default@ . If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used. Default: @> Default@ .
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\s a -> s {_ltsVersion = a})

instance FromJSON LaunchTemplateSpecification where
  parseJSON =
    withObject
      "LaunchTemplateSpecification"
      ( \x ->
          LaunchTemplateSpecification'
            <$> (x .:? "launchTemplateName")
            <*> (x .:? "launchTemplateId")
            <*> (x .:? "version")
      )

instance Hashable LaunchTemplateSpecification

instance NFData LaunchTemplateSpecification

instance ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    object
      ( catMaybes
          [ ("launchTemplateName" .=) <$> _ltsLaunchTemplateName,
            ("launchTemplateId" .=) <$> _ltsLaunchTemplateId,
            ("version" .=) <$> _ltsVersion
          ]
      )
