{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The S3 location for importing the package specified as @S3BucketName@ and @S3Key@
--
--
--
-- /See:/ 'packageSource' smart constructor.
data PackageSource = PackageSource'
  { _psS3Key :: !(Maybe Text),
    _psS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PackageSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psS3Key' - Key (file name) of the package.
--
-- * 'psS3BucketName' - Name of the bucket containing the package.
packageSource ::
  PackageSource
packageSource =
  PackageSource' {_psS3Key = Nothing, _psS3BucketName = Nothing}

-- | Key (file name) of the package.
psS3Key :: Lens' PackageSource (Maybe Text)
psS3Key = lens _psS3Key (\s a -> s {_psS3Key = a})

-- | Name of the bucket containing the package.
psS3BucketName :: Lens' PackageSource (Maybe Text)
psS3BucketName = lens _psS3BucketName (\s a -> s {_psS3BucketName = a})

instance Hashable PackageSource

instance NFData PackageSource

instance ToJSON PackageSource where
  toJSON PackageSource' {..} =
    object
      ( catMaybes
          [ ("S3Key" .=) <$> _psS3Key,
            ("S3BucketName" .=) <$> _psS3BucketName
          ]
      )
