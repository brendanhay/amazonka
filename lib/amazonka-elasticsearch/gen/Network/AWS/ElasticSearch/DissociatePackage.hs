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
-- Module      : Network.AWS.ElasticSearch.DissociatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dissociates a package from the Amazon ES domain.
module Network.AWS.ElasticSearch.DissociatePackage
  ( -- * Creating a Request
    dissociatePackage,
    DissociatePackage,

    -- * Request Lenses
    dpPackageId,
    dpDomainName,

    -- * Destructuring the Response
    dissociatePackageResponse,
    DissociatePackageResponse,

    -- * Response Lenses
    disrsDomainPackageDetails,
    disrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'DissociatePackage' @ operation.
--
--
--
-- /See:/ 'dissociatePackage' smart constructor.
data DissociatePackage = DissociatePackage'
  { _dpPackageId :: !Text,
    _dpDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DissociatePackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPackageId' - Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- * 'dpDomainName' - Name of the domain that you want to associate the package with.
dissociatePackage ::
  -- | 'dpPackageId'
  Text ->
  -- | 'dpDomainName'
  Text ->
  DissociatePackage
dissociatePackage pPackageId_ pDomainName_ =
  DissociatePackage'
    { _dpPackageId = pPackageId_,
      _dpDomainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
dpPackageId :: Lens' DissociatePackage Text
dpPackageId = lens _dpPackageId (\s a -> s {_dpPackageId = a})

-- | Name of the domain that you want to associate the package with.
dpDomainName :: Lens' DissociatePackage Text
dpDomainName = lens _dpDomainName (\s a -> s {_dpDomainName = a})

instance AWSRequest DissociatePackage where
  type Rs DissociatePackage = DissociatePackageResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          DissociatePackageResponse'
            <$> (x .?> "DomainPackageDetails") <*> (pure (fromEnum s))
      )

instance Hashable DissociatePackage

instance NFData DissociatePackage

instance ToHeaders DissociatePackage where
  toHeaders = const mempty

instance ToJSON DissociatePackage where
  toJSON = const (Object mempty)

instance ToPath DissociatePackage where
  toPath DissociatePackage' {..} =
    mconcat
      [ "/2015-01-01/packages/dissociate/",
        toBS _dpPackageId,
        "/",
        toBS _dpDomainName
      ]

instance ToQuery DissociatePackage where
  toQuery = const mempty

-- | Container for response returned by @'DissociatePackage' @ operation.
--
--
--
-- /See:/ 'dissociatePackageResponse' smart constructor.
data DissociatePackageResponse = DissociatePackageResponse'
  { _disrsDomainPackageDetails ::
      !(Maybe DomainPackageDetails),
    _disrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DissociatePackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsDomainPackageDetails' - @DomainPackageDetails@
--
-- * 'disrsResponseStatus' - -- | The response status code.
dissociatePackageResponse ::
  -- | 'disrsResponseStatus'
  Int ->
  DissociatePackageResponse
dissociatePackageResponse pResponseStatus_ =
  DissociatePackageResponse'
    { _disrsDomainPackageDetails = Nothing,
      _disrsResponseStatus = pResponseStatus_
    }

-- | @DomainPackageDetails@
disrsDomainPackageDetails :: Lens' DissociatePackageResponse (Maybe DomainPackageDetails)
disrsDomainPackageDetails = lens _disrsDomainPackageDetails (\s a -> s {_disrsDomainPackageDetails = a})

-- | -- | The response status code.
disrsResponseStatus :: Lens' DissociatePackageResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\s a -> s {_disrsResponseStatus = a})

instance NFData DissociatePackageResponse
