{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageVersionHistory where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of a package version.
--
--
--
-- /See:/ 'packageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { _pvhCreatedAt ::
      !(Maybe POSIX),
    _pvhPackageVersion :: !(Maybe Text),
    _pvhCommitMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PackageVersionHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvhCreatedAt' - Timestamp which tells creation time of the package version.
--
-- * 'pvhPackageVersion' - Version of the package.
--
-- * 'pvhCommitMessage' - A message associated with the version.
packageVersionHistory ::
  PackageVersionHistory
packageVersionHistory =
  PackageVersionHistory'
    { _pvhCreatedAt = Nothing,
      _pvhPackageVersion = Nothing,
      _pvhCommitMessage = Nothing
    }

-- | Timestamp which tells creation time of the package version.
pvhCreatedAt :: Lens' PackageVersionHistory (Maybe UTCTime)
pvhCreatedAt = lens _pvhCreatedAt (\s a -> s {_pvhCreatedAt = a}) . mapping _Time

-- | Version of the package.
pvhPackageVersion :: Lens' PackageVersionHistory (Maybe Text)
pvhPackageVersion = lens _pvhPackageVersion (\s a -> s {_pvhPackageVersion = a})

-- | A message associated with the version.
pvhCommitMessage :: Lens' PackageVersionHistory (Maybe Text)
pvhCommitMessage = lens _pvhCommitMessage (\s a -> s {_pvhCommitMessage = a})

instance FromJSON PackageVersionHistory where
  parseJSON =
    withObject
      "PackageVersionHistory"
      ( \x ->
          PackageVersionHistory'
            <$> (x .:? "CreatedAt")
            <*> (x .:? "PackageVersion")
            <*> (x .:? "CommitMessage")
      )

instance Hashable PackageVersionHistory

instance NFData PackageVersionHistory
