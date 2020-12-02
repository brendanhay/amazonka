{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectCache where

import Network.AWS.CodeBuild.Types.CacheMode
import Network.AWS.CodeBuild.Types.CacheType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the cache for the build project.
--
--
--
-- /See:/ 'projectCache' smart constructor.
data ProjectCache = ProjectCache'
  { _pcLocation :: !(Maybe Text),
    _pcModes :: !(Maybe [CacheMode]),
    _pcType :: !CacheType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectCache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcLocation' - Information about the cache location:      * @NO_CACHE@ or @LOCAL@ : This value is ignored.     * @S3@ : This is the S3 bucket name/prefix.
--
-- * 'pcModes' - An array of strings that specify the local cache modes. You can use one or more local cache modes at the same time. This is only used for @LOCAL@ cache types. Possible values are:     * LOCAL_SOURCE_CACHE    * Caches Git metadata for primary and secondary sources. After the cache is created, subsequent builds pull only the change between commits. This mode is a good choice for projects with a clean working directory and a source that is a large Git repository. If you choose this option and your project does not use a Git repository (GitHub, GitHub Enterprise, or Bitbucket), the option is ignored.      * LOCAL_DOCKER_LAYER_CACHE    * Caches existing Docker layers. This mode is a good choice for projects that build or pull large Docker images. It can prevent the performance issues caused by pulling large Docker images down from the network.      * LOCAL_CUSTOM_CACHE    * Caches directories you specify in the buildspec file. This mode is a good choice if your build scenario is not suited to one of the other three local cache modes. If you use a custom cache:      * Only directories can be specified for caching. You cannot specify individual files.      * Symlinks are used to reference cached directories.      * Cached directories are linked to your build before it downloads its project sources. Cached items are overridden if a source item has the same name. Directories are specified using cache paths in the buildspec file.
--
-- * 'pcType' - The type of cache used by the build project. Valid values include:     * @NO_CACHE@ : The build project does not use any cache.     * @S3@ : The build project reads and writes from and to S3.     * @LOCAL@ : The build project stores a cache locally on a build host that is only available to that build host.
projectCache ::
  -- | 'pcType'
  CacheType ->
  ProjectCache
projectCache pType_ =
  ProjectCache'
    { _pcLocation = Nothing,
      _pcModes = Nothing,
      _pcType = pType_
    }

-- | Information about the cache location:      * @NO_CACHE@ or @LOCAL@ : This value is ignored.     * @S3@ : This is the S3 bucket name/prefix.
pcLocation :: Lens' ProjectCache (Maybe Text)
pcLocation = lens _pcLocation (\s a -> s {_pcLocation = a})

-- | An array of strings that specify the local cache modes. You can use one or more local cache modes at the same time. This is only used for @LOCAL@ cache types. Possible values are:     * LOCAL_SOURCE_CACHE    * Caches Git metadata for primary and secondary sources. After the cache is created, subsequent builds pull only the change between commits. This mode is a good choice for projects with a clean working directory and a source that is a large Git repository. If you choose this option and your project does not use a Git repository (GitHub, GitHub Enterprise, or Bitbucket), the option is ignored.      * LOCAL_DOCKER_LAYER_CACHE    * Caches existing Docker layers. This mode is a good choice for projects that build or pull large Docker images. It can prevent the performance issues caused by pulling large Docker images down from the network.      * LOCAL_CUSTOM_CACHE    * Caches directories you specify in the buildspec file. This mode is a good choice if your build scenario is not suited to one of the other three local cache modes. If you use a custom cache:      * Only directories can be specified for caching. You cannot specify individual files.      * Symlinks are used to reference cached directories.      * Cached directories are linked to your build before it downloads its project sources. Cached items are overridden if a source item has the same name. Directories are specified using cache paths in the buildspec file.
pcModes :: Lens' ProjectCache [CacheMode]
pcModes = lens _pcModes (\s a -> s {_pcModes = a}) . _Default . _Coerce

-- | The type of cache used by the build project. Valid values include:     * @NO_CACHE@ : The build project does not use any cache.     * @S3@ : The build project reads and writes from and to S3.     * @LOCAL@ : The build project stores a cache locally on a build host that is only available to that build host.
pcType :: Lens' ProjectCache CacheType
pcType = lens _pcType (\s a -> s {_pcType = a})

instance FromJSON ProjectCache where
  parseJSON =
    withObject
      "ProjectCache"
      ( \x ->
          ProjectCache'
            <$> (x .:? "location")
            <*> (x .:? "modes" .!= mempty)
            <*> (x .: "type")
      )

instance Hashable ProjectCache

instance NFData ProjectCache

instance ToJSON ProjectCache where
  toJSON ProjectCache' {..} =
    object
      ( catMaybes
          [ ("location" .=) <$> _pcLocation,
            ("modes" .=) <$> _pcModes,
            Just ("type" .= _pcType)
          ]
      )
