{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.CreatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a package for use with Amazon ES domains.
module Network.AWS.ElasticSearch.CreatePackage
  ( -- * Creating a Request
    createPackage,
    CreatePackage,

    -- * Request Lenses
    cpPackageDescription,
    cpPackageName,
    cpPackageType,
    cpPackageSource,

    -- * Destructuring the Response
    createPackageResponse,
    CreatePackageResponse,

    -- * Response Lenses
    cprsPackageDetails,
    cprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'CreatePackage' @ operation.
--
--
--
-- /See:/ 'createPackage' smart constructor.
data CreatePackage = CreatePackage'
  { _cpPackageDescription ::
      !(Maybe Text),
    _cpPackageName :: !Text,
    _cpPackageType :: !PackageType,
    _cpPackageSource :: !PackageSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPackageDescription' - Description of the package.
--
-- * 'cpPackageName' - Unique identifier for the package.
--
-- * 'cpPackageType' - Type of package. Currently supports only TXT-DICTIONARY.
--
-- * 'cpPackageSource' - The customer S3 location @PackageSource@ for importing the package.
createPackage ::
  -- | 'cpPackageName'
  Text ->
  -- | 'cpPackageType'
  PackageType ->
  -- | 'cpPackageSource'
  PackageSource ->
  CreatePackage
createPackage pPackageName_ pPackageType_ pPackageSource_ =
  CreatePackage'
    { _cpPackageDescription = Nothing,
      _cpPackageName = pPackageName_,
      _cpPackageType = pPackageType_,
      _cpPackageSource = pPackageSource_
    }

-- | Description of the package.
cpPackageDescription :: Lens' CreatePackage (Maybe Text)
cpPackageDescription = lens _cpPackageDescription (\s a -> s {_cpPackageDescription = a})

-- | Unique identifier for the package.
cpPackageName :: Lens' CreatePackage Text
cpPackageName = lens _cpPackageName (\s a -> s {_cpPackageName = a})

-- | Type of package. Currently supports only TXT-DICTIONARY.
cpPackageType :: Lens' CreatePackage PackageType
cpPackageType = lens _cpPackageType (\s a -> s {_cpPackageType = a})

-- | The customer S3 location @PackageSource@ for importing the package.
cpPackageSource :: Lens' CreatePackage PackageSource
cpPackageSource = lens _cpPackageSource (\s a -> s {_cpPackageSource = a})

instance AWSRequest CreatePackage where
  type Rs CreatePackage = CreatePackageResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          CreatePackageResponse'
            <$> (x .?> "PackageDetails") <*> (pure (fromEnum s))
      )

instance Hashable CreatePackage

instance NFData CreatePackage

instance ToHeaders CreatePackage where
  toHeaders = const mempty

instance ToJSON CreatePackage where
  toJSON CreatePackage' {..} =
    object
      ( catMaybes
          [ ("PackageDescription" .=) <$> _cpPackageDescription,
            Just ("PackageName" .= _cpPackageName),
            Just ("PackageType" .= _cpPackageType),
            Just ("PackageSource" .= _cpPackageSource)
          ]
      )

instance ToPath CreatePackage where
  toPath = const "/2015-01-01/packages"

instance ToQuery CreatePackage where
  toQuery = const mempty

-- | Container for response returned by @'CreatePackage' @ operation.
--
--
--
-- /See:/ 'createPackageResponse' smart constructor.
data CreatePackageResponse = CreatePackageResponse'
  { _cprsPackageDetails ::
      !(Maybe PackageDetails),
    _cprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPackageDetails' - Information about the package @PackageDetails@ .
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPackageResponse ::
  -- | 'cprsResponseStatus'
  Int ->
  CreatePackageResponse
createPackageResponse pResponseStatus_ =
  CreatePackageResponse'
    { _cprsPackageDetails = Nothing,
      _cprsResponseStatus = pResponseStatus_
    }

-- | Information about the package @PackageDetails@ .
cprsPackageDetails :: Lens' CreatePackageResponse (Maybe PackageDetails)
cprsPackageDetails = lens _cprsPackageDetails (\s a -> s {_cprsPackageDetails = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePackageResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\s a -> s {_cprsResponseStatus = a})

instance NFData CreatePackageResponse
