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
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the specified portfolio share operation. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.DescribePortfolioShareStatus
  ( -- * Creating a Request
    describePortfolioShareStatus,
    DescribePortfolioShareStatus,

    -- * Request Lenses
    dpssPortfolioShareToken,

    -- * Destructuring the Response
    describePortfolioShareStatusResponse,
    DescribePortfolioShareStatusResponse,

    -- * Response Lenses
    dpssrsStatus,
    dpssrsPortfolioShareToken,
    dpssrsShareDetails,
    dpssrsPortfolioId,
    dpssrsOrganizationNodeValue,
    dpssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'describePortfolioShareStatus' smart constructor.
newtype DescribePortfolioShareStatus = DescribePortfolioShareStatus'
  { _dpssPortfolioShareToken ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePortfolioShareStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpssPortfolioShareToken' - The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
describePortfolioShareStatus ::
  -- | 'dpssPortfolioShareToken'
  Text ->
  DescribePortfolioShareStatus
describePortfolioShareStatus pPortfolioShareToken_ =
  DescribePortfolioShareStatus'
    { _dpssPortfolioShareToken =
        pPortfolioShareToken_
    }

-- | The token for the portfolio share operation. This token is returned either by CreatePortfolioShare or by DeletePortfolioShare.
dpssPortfolioShareToken :: Lens' DescribePortfolioShareStatus Text
dpssPortfolioShareToken = lens _dpssPortfolioShareToken (\s a -> s {_dpssPortfolioShareToken = a})

instance AWSRequest DescribePortfolioShareStatus where
  type
    Rs DescribePortfolioShareStatus =
      DescribePortfolioShareStatusResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DescribePortfolioShareStatusResponse'
            <$> (x .?> "Status")
            <*> (x .?> "PortfolioShareToken")
            <*> (x .?> "ShareDetails")
            <*> (x .?> "PortfolioId")
            <*> (x .?> "OrganizationNodeValue")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribePortfolioShareStatus

instance NFData DescribePortfolioShareStatus

instance ToHeaders DescribePortfolioShareStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.DescribePortfolioShareStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribePortfolioShareStatus where
  toJSON DescribePortfolioShareStatus' {..} =
    object
      ( catMaybes
          [Just ("PortfolioShareToken" .= _dpssPortfolioShareToken)]
      )

instance ToPath DescribePortfolioShareStatus where
  toPath = const "/"

instance ToQuery DescribePortfolioShareStatus where
  toQuery = const mempty

-- | /See:/ 'describePortfolioShareStatusResponse' smart constructor.
data DescribePortfolioShareStatusResponse = DescribePortfolioShareStatusResponse'
  { _dpssrsStatus ::
      !( Maybe
           ShareStatus
       ),
    _dpssrsPortfolioShareToken ::
      !(Maybe Text),
    _dpssrsShareDetails ::
      !( Maybe
           ShareDetails
       ),
    _dpssrsPortfolioId ::
      !(Maybe Text),
    _dpssrsOrganizationNodeValue ::
      !(Maybe Text),
    _dpssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribePortfolioShareStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpssrsStatus' - Status of the portfolio share operation.
--
-- * 'dpssrsPortfolioShareToken' - The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
--
-- * 'dpssrsShareDetails' - Information about the portfolio share operation.
--
-- * 'dpssrsPortfolioId' - The portfolio identifier.
--
-- * 'dpssrsOrganizationNodeValue' - Organization node identifier. It can be either account id, organizational unit id or organization id.
--
-- * 'dpssrsResponseStatus' - -- | The response status code.
describePortfolioShareStatusResponse ::
  -- | 'dpssrsResponseStatus'
  Int ->
  DescribePortfolioShareStatusResponse
describePortfolioShareStatusResponse pResponseStatus_ =
  DescribePortfolioShareStatusResponse'
    { _dpssrsStatus = Nothing,
      _dpssrsPortfolioShareToken = Nothing,
      _dpssrsShareDetails = Nothing,
      _dpssrsPortfolioId = Nothing,
      _dpssrsOrganizationNodeValue = Nothing,
      _dpssrsResponseStatus = pResponseStatus_
    }

-- | Status of the portfolio share operation.
dpssrsStatus :: Lens' DescribePortfolioShareStatusResponse (Maybe ShareStatus)
dpssrsStatus = lens _dpssrsStatus (\s a -> s {_dpssrsStatus = a})

-- | The token for the portfolio share operation. For example, @share-6v24abcdefghi@ .
dpssrsPortfolioShareToken :: Lens' DescribePortfolioShareStatusResponse (Maybe Text)
dpssrsPortfolioShareToken = lens _dpssrsPortfolioShareToken (\s a -> s {_dpssrsPortfolioShareToken = a})

-- | Information about the portfolio share operation.
dpssrsShareDetails :: Lens' DescribePortfolioShareStatusResponse (Maybe ShareDetails)
dpssrsShareDetails = lens _dpssrsShareDetails (\s a -> s {_dpssrsShareDetails = a})

-- | The portfolio identifier.
dpssrsPortfolioId :: Lens' DescribePortfolioShareStatusResponse (Maybe Text)
dpssrsPortfolioId = lens _dpssrsPortfolioId (\s a -> s {_dpssrsPortfolioId = a})

-- | Organization node identifier. It can be either account id, organizational unit id or organization id.
dpssrsOrganizationNodeValue :: Lens' DescribePortfolioShareStatusResponse (Maybe Text)
dpssrsOrganizationNodeValue = lens _dpssrsOrganizationNodeValue (\s a -> s {_dpssrsOrganizationNodeValue = a})

-- | -- | The response status code.
dpssrsResponseStatus :: Lens' DescribePortfolioShareStatusResponse Int
dpssrsResponseStatus = lens _dpssrsResponseStatus (\s a -> s {_dpssrsResponseStatus = a})

instance NFData DescribePortfolioShareStatusResponse
