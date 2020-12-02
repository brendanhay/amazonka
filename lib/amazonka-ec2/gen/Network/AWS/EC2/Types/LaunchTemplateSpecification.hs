{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request, but not both.
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
-- * 'ltsVersion' - The version number of the launch template. Default: The default version for the launch template.
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

-- | The version number of the launch template. Default: The default version for the launch template.
ltsVersion :: Lens' LaunchTemplateSpecification (Maybe Text)
ltsVersion = lens _ltsVersion (\s a -> s {_ltsVersion = a})

instance Hashable LaunchTemplateSpecification

instance NFData LaunchTemplateSpecification

instance ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    mconcat
      [ "LaunchTemplateName" =: _ltsLaunchTemplateName,
        "LaunchTemplateId" =: _ltsLaunchTemplateId,
        "Version" =: _ltsVersion
      ]
