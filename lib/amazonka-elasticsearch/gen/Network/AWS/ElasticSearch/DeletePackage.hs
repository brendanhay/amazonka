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
-- Module      : Network.AWS.ElasticSearch.DeletePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the package.
module Network.AWS.ElasticSearch.DeletePackage
  ( -- * Creating a Request
    deletePackage,
    DeletePackage,

    -- * Request Lenses
    dPackageId,

    -- * Destructuring the Response
    deletePackageResponse,
    DeletePackageResponse,

    -- * Response Lenses
    delrsPackageDetails,
    delrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'DeletePackage' @ operation.
--
--
--
-- /See:/ 'deletePackage' smart constructor.
newtype DeletePackage = DeletePackage' {_dPackageId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPackageId' - Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
deletePackage ::
  -- | 'dPackageId'
  Text ->
  DeletePackage
deletePackage pPackageId_ =
  DeletePackage' {_dPackageId = pPackageId_}

-- | Internal ID of the package that you want to delete. Use @DescribePackages@ to find this value.
dPackageId :: Lens' DeletePackage Text
dPackageId = lens _dPackageId (\s a -> s {_dPackageId = a})

instance AWSRequest DeletePackage where
  type Rs DeletePackage = DeletePackageResponse
  request = delete elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          DeletePackageResponse'
            <$> (x .?> "PackageDetails") <*> (pure (fromEnum s))
      )

instance Hashable DeletePackage

instance NFData DeletePackage

instance ToHeaders DeletePackage where
  toHeaders = const mempty

instance ToPath DeletePackage where
  toPath DeletePackage' {..} =
    mconcat ["/2015-01-01/packages/", toBS _dPackageId]

instance ToQuery DeletePackage where
  toQuery = const mempty

-- | Container for response parameters to @'DeletePackage' @ operation.
--
--
--
-- /See:/ 'deletePackageResponse' smart constructor.
data DeletePackageResponse = DeletePackageResponse'
  { _delrsPackageDetails ::
      !(Maybe PackageDetails),
    _delrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsPackageDetails' - @PackageDetails@
--
-- * 'delrsResponseStatus' - -- | The response status code.
deletePackageResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeletePackageResponse
deletePackageResponse pResponseStatus_ =
  DeletePackageResponse'
    { _delrsPackageDetails = Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | @PackageDetails@
delrsPackageDetails :: Lens' DeletePackageResponse (Maybe PackageDetails)
delrsPackageDetails = lens _delrsPackageDetails (\s a -> s {_delrsPackageDetails = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeletePackageResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeletePackageResponse
