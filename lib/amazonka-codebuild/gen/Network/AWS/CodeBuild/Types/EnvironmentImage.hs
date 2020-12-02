{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { _eiVersions ::
      !(Maybe [Text]),
    _eiName :: !(Maybe Text),
    _eiDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiVersions' - A list of environment image versions.
--
-- * 'eiName' - The name of the Docker image.
--
-- * 'eiDescription' - The description of the Docker image.
environmentImage ::
  EnvironmentImage
environmentImage =
  EnvironmentImage'
    { _eiVersions = Nothing,
      _eiName = Nothing,
      _eiDescription = Nothing
    }

-- | A list of environment image versions.
eiVersions :: Lens' EnvironmentImage [Text]
eiVersions = lens _eiVersions (\s a -> s {_eiVersions = a}) . _Default . _Coerce

-- | The name of the Docker image.
eiName :: Lens' EnvironmentImage (Maybe Text)
eiName = lens _eiName (\s a -> s {_eiName = a})

-- | The description of the Docker image.
eiDescription :: Lens' EnvironmentImage (Maybe Text)
eiDescription = lens _eiDescription (\s a -> s {_eiDescription = a})

instance FromJSON EnvironmentImage where
  parseJSON =
    withObject
      "EnvironmentImage"
      ( \x ->
          EnvironmentImage'
            <$> (x .:? "versions" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "description")
      )

instance Hashable EnvironmentImage

instance NFData EnvironmentImage
