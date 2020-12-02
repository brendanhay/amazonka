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
-- Module      : Network.AWS.EC2.GetManagedPrefixListAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resources that are associated with the specified managed prefix list.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListAssociations
  ( -- * Creating a Request
    getManagedPrefixListAssociations,
    GetManagedPrefixListAssociations,

    -- * Request Lenses
    gmplaNextToken,
    gmplaDryRun,
    gmplaMaxResults,
    gmplaPrefixListId,

    -- * Destructuring the Response
    getManagedPrefixListAssociationsResponse,
    GetManagedPrefixListAssociationsResponse,

    -- * Response Lenses
    gmplarsNextToken,
    gmplarsPrefixListAssociations,
    gmplarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getManagedPrefixListAssociations' smart constructor.
data GetManagedPrefixListAssociations = GetManagedPrefixListAssociations'
  { _gmplaNextToken ::
      !(Maybe Text),
    _gmplaDryRun ::
      !(Maybe Bool),
    _gmplaMaxResults ::
      !(Maybe Nat),
    _gmplaPrefixListId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedPrefixListAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmplaNextToken' - The token for the next page of results.
--
-- * 'gmplaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gmplaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gmplaPrefixListId' - The ID of the prefix list.
getManagedPrefixListAssociations ::
  -- | 'gmplaPrefixListId'
  Text ->
  GetManagedPrefixListAssociations
getManagedPrefixListAssociations pPrefixListId_ =
  GetManagedPrefixListAssociations'
    { _gmplaNextToken = Nothing,
      _gmplaDryRun = Nothing,
      _gmplaMaxResults = Nothing,
      _gmplaPrefixListId = pPrefixListId_
    }

-- | The token for the next page of results.
gmplaNextToken :: Lens' GetManagedPrefixListAssociations (Maybe Text)
gmplaNextToken = lens _gmplaNextToken (\s a -> s {_gmplaNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gmplaDryRun :: Lens' GetManagedPrefixListAssociations (Maybe Bool)
gmplaDryRun = lens _gmplaDryRun (\s a -> s {_gmplaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gmplaMaxResults :: Lens' GetManagedPrefixListAssociations (Maybe Natural)
gmplaMaxResults = lens _gmplaMaxResults (\s a -> s {_gmplaMaxResults = a}) . mapping _Nat

-- | The ID of the prefix list.
gmplaPrefixListId :: Lens' GetManagedPrefixListAssociations Text
gmplaPrefixListId = lens _gmplaPrefixListId (\s a -> s {_gmplaPrefixListId = a})

instance AWSPager GetManagedPrefixListAssociations where
  page rq rs
    | stop (rs ^. gmplarsNextToken) = Nothing
    | stop (rs ^. gmplarsPrefixListAssociations) = Nothing
    | otherwise = Just $ rq & gmplaNextToken .~ rs ^. gmplarsNextToken

instance AWSRequest GetManagedPrefixListAssociations where
  type
    Rs GetManagedPrefixListAssociations =
      GetManagedPrefixListAssociationsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetManagedPrefixListAssociationsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "prefixListAssociationSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetManagedPrefixListAssociations

instance NFData GetManagedPrefixListAssociations

instance ToHeaders GetManagedPrefixListAssociations where
  toHeaders = const mempty

instance ToPath GetManagedPrefixListAssociations where
  toPath = const "/"

instance ToQuery GetManagedPrefixListAssociations where
  toQuery GetManagedPrefixListAssociations' {..} =
    mconcat
      [ "Action" =: ("GetManagedPrefixListAssociations" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _gmplaNextToken,
        "DryRun" =: _gmplaDryRun,
        "MaxResults" =: _gmplaMaxResults,
        "PrefixListId" =: _gmplaPrefixListId
      ]

-- | /See:/ 'getManagedPrefixListAssociationsResponse' smart constructor.
data GetManagedPrefixListAssociationsResponse = GetManagedPrefixListAssociationsResponse'
  { _gmplarsNextToken ::
      !( Maybe
           Text
       ),
    _gmplarsPrefixListAssociations ::
      !( Maybe
           [PrefixListAssociation]
       ),
    _gmplarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedPrefixListAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmplarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gmplarsPrefixListAssociations' - Information about the associations.
--
-- * 'gmplarsResponseStatus' - -- | The response status code.
getManagedPrefixListAssociationsResponse ::
  -- | 'gmplarsResponseStatus'
  Int ->
  GetManagedPrefixListAssociationsResponse
getManagedPrefixListAssociationsResponse pResponseStatus_ =
  GetManagedPrefixListAssociationsResponse'
    { _gmplarsNextToken =
        Nothing,
      _gmplarsPrefixListAssociations = Nothing,
      _gmplarsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gmplarsNextToken :: Lens' GetManagedPrefixListAssociationsResponse (Maybe Text)
gmplarsNextToken = lens _gmplarsNextToken (\s a -> s {_gmplarsNextToken = a})

-- | Information about the associations.
gmplarsPrefixListAssociations :: Lens' GetManagedPrefixListAssociationsResponse [PrefixListAssociation]
gmplarsPrefixListAssociations = lens _gmplarsPrefixListAssociations (\s a -> s {_gmplarsPrefixListAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
gmplarsResponseStatus :: Lens' GetManagedPrefixListAssociationsResponse Int
gmplarsResponseStatus = lens _gmplarsResponseStatus (\s a -> s {_gmplarsResponseStatus = a})

instance NFData GetManagedPrefixListAssociationsResponse
