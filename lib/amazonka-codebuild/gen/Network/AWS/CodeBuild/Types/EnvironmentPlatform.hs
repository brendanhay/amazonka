{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentPlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentPlatform where

import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.PlatformType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of Docker images that are related by platform and are managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { _epPlatform ::
      !(Maybe PlatformType),
    _epLanguages :: !(Maybe [EnvironmentLanguage])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentPlatform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPlatform' - The platform's name.
--
-- * 'epLanguages' - The list of programming languages that are available for the specified platform.
environmentPlatform ::
  EnvironmentPlatform
environmentPlatform =
  EnvironmentPlatform'
    { _epPlatform = Nothing,
      _epLanguages = Nothing
    }

-- | The platform's name.
epPlatform :: Lens' EnvironmentPlatform (Maybe PlatformType)
epPlatform = lens _epPlatform (\s a -> s {_epPlatform = a})

-- | The list of programming languages that are available for the specified platform.
epLanguages :: Lens' EnvironmentPlatform [EnvironmentLanguage]
epLanguages = lens _epLanguages (\s a -> s {_epLanguages = a}) . _Default . _Coerce

instance FromJSON EnvironmentPlatform where
  parseJSON =
    withObject
      "EnvironmentPlatform"
      ( \x ->
          EnvironmentPlatform'
            <$> (x .:? "platform") <*> (x .:? "languages" .!= mempty)
      )

instance Hashable EnvironmentPlatform

instance NFData EnvironmentPlatform
