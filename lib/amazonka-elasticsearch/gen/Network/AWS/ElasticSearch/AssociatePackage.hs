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
-- Module      : Network.AWS.ElasticSearch.AssociatePackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a package with an Amazon ES domain.
module Network.AWS.ElasticSearch.AssociatePackage
  ( -- * Creating a Request
    associatePackage,
    AssociatePackage,

    -- * Request Lenses
    apPackageId,
    apDomainName,

    -- * Destructuring the Response
    associatePackageResponse,
    AssociatePackageResponse,

    -- * Response Lenses
    aprsDomainPackageDetails,
    aprsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for request parameters to @'AssociatePackage' @ operation.
--
--
--
-- /See:/ 'associatePackage' smart constructor.
data AssociatePackage = AssociatePackage'
  { _apPackageId :: !Text,
    _apDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociatePackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPackageId' - Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
--
-- * 'apDomainName' - Name of the domain that you want to associate the package with.
associatePackage ::
  -- | 'apPackageId'
  Text ->
  -- | 'apDomainName'
  Text ->
  AssociatePackage
associatePackage pPackageId_ pDomainName_ =
  AssociatePackage'
    { _apPackageId = pPackageId_,
      _apDomainName = pDomainName_
    }

-- | Internal ID of the package that you want to associate with a domain. Use @DescribePackages@ to find this value.
apPackageId :: Lens' AssociatePackage Text
apPackageId = lens _apPackageId (\s a -> s {_apPackageId = a})

-- | Name of the domain that you want to associate the package with.
apDomainName :: Lens' AssociatePackage Text
apDomainName = lens _apDomainName (\s a -> s {_apDomainName = a})

instance AWSRequest AssociatePackage where
  type Rs AssociatePackage = AssociatePackageResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          AssociatePackageResponse'
            <$> (x .?> "DomainPackageDetails") <*> (pure (fromEnum s))
      )

instance Hashable AssociatePackage

instance NFData AssociatePackage

instance ToHeaders AssociatePackage where
  toHeaders = const mempty

instance ToJSON AssociatePackage where
  toJSON = const (Object mempty)

instance ToPath AssociatePackage where
  toPath AssociatePackage' {..} =
    mconcat
      [ "/2015-01-01/packages/associate/",
        toBS _apPackageId,
        "/",
        toBS _apDomainName
      ]

instance ToQuery AssociatePackage where
  toQuery = const mempty

-- | Container for response returned by @'AssociatePackage' @ operation.
--
--
--
-- /See:/ 'associatePackageResponse' smart constructor.
data AssociatePackageResponse = AssociatePackageResponse'
  { _aprsDomainPackageDetails ::
      !(Maybe DomainPackageDetails),
    _aprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociatePackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aprsDomainPackageDetails' - @DomainPackageDetails@
--
-- * 'aprsResponseStatus' - -- | The response status code.
associatePackageResponse ::
  -- | 'aprsResponseStatus'
  Int ->
  AssociatePackageResponse
associatePackageResponse pResponseStatus_ =
  AssociatePackageResponse'
    { _aprsDomainPackageDetails = Nothing,
      _aprsResponseStatus = pResponseStatus_
    }

-- | @DomainPackageDetails@
aprsDomainPackageDetails :: Lens' AssociatePackageResponse (Maybe DomainPackageDetails)
aprsDomainPackageDetails = lens _aprsDomainPackageDetails (\s a -> s {_aprsDomainPackageDetails = a})

-- | -- | The response status code.
aprsResponseStatus :: Lens' AssociatePackageResponse Int
aprsResponseStatus = lens _aprsResponseStatus (\s a -> s {_aprsResponseStatus = a})

instance NFData AssociatePackageResponse
