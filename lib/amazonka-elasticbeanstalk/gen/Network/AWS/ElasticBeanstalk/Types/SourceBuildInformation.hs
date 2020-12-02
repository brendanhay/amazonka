{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation where

import Network.AWS.ElasticBeanstalk.Types.SourceRepository
import Network.AWS.ElasticBeanstalk.Types.SourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Location of the source code for an application version.
--
--
--
-- /See:/ 'sourceBuildInformation' smart constructor.
data SourceBuildInformation = SourceBuildInformation'
  { _sbiSourceType ::
      !SourceType,
    _sbiSourceRepository :: !SourceRepository,
    _sbiSourceLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceBuildInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbiSourceType' - The type of repository.     * @Git@      * @Zip@
--
-- * 'sbiSourceRepository' - Location where the repository is stored.     * @CodeCommit@      * @S3@
--
-- * 'sbiSourceLocation' - The location of the source code, as a formatted string, depending on the value of @SourceRepository@      * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
sourceBuildInformation ::
  -- | 'sbiSourceType'
  SourceType ->
  -- | 'sbiSourceRepository'
  SourceRepository ->
  -- | 'sbiSourceLocation'
  Text ->
  SourceBuildInformation
sourceBuildInformation
  pSourceType_
  pSourceRepository_
  pSourceLocation_ =
    SourceBuildInformation'
      { _sbiSourceType = pSourceType_,
        _sbiSourceRepository = pSourceRepository_,
        _sbiSourceLocation = pSourceLocation_
      }

-- | The type of repository.     * @Git@      * @Zip@
sbiSourceType :: Lens' SourceBuildInformation SourceType
sbiSourceType = lens _sbiSourceType (\s a -> s {_sbiSourceType = a})

-- | Location where the repository is stored.     * @CodeCommit@      * @S3@
sbiSourceRepository :: Lens' SourceBuildInformation SourceRepository
sbiSourceRepository = lens _sbiSourceRepository (\s a -> s {_sbiSourceRepository = a})

-- | The location of the source code, as a formatted string, depending on the value of @SourceRepository@      * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
sbiSourceLocation :: Lens' SourceBuildInformation Text
sbiSourceLocation = lens _sbiSourceLocation (\s a -> s {_sbiSourceLocation = a})

instance FromXML SourceBuildInformation where
  parseXML x =
    SourceBuildInformation'
      <$> (x .@ "SourceType")
      <*> (x .@ "SourceRepository")
      <*> (x .@ "SourceLocation")

instance Hashable SourceBuildInformation

instance NFData SourceBuildInformation

instance ToQuery SourceBuildInformation where
  toQuery SourceBuildInformation' {..} =
    mconcat
      [ "SourceType" =: _sbiSourceType,
        "SourceRepository" =: _sbiSourceRepository,
        "SourceLocation" =: _sbiSourceLocation
      ]
