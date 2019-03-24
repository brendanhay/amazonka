{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DescribeImagePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the permissions for shared AWS account IDs on a private image that you own.
--
--
module Network.AWS.AppStream.DescribeImagePermissions
    (
    -- * Creating a Request
      describeImagePermissions
    , DescribeImagePermissions
    -- * Request Lenses
    , dipsNextToken
    , dipsSharedAWSAccountIds
    , dipsMaxResults
    , dipsName

    -- * Destructuring the Response
    , describeImagePermissionsResponse
    , DescribeImagePermissionsResponse
    -- * Response Lenses
    , dipsrsSharedImagePermissionsList
    , dipsrsNextToken
    , dipsrsName
    , dipsrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImagePermissions' smart constructor.
data DescribeImagePermissions = DescribeImagePermissions'
  { _dipsNextToken           :: !(Maybe Text)
  , _dipsSharedAWSAccountIds :: !(Maybe (List1 Text))
  , _dipsMaxResults          :: !(Maybe Nat)
  , _dipsName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dipsSharedAWSAccountIds' - The 12-digit identifier of one or more AWS accounts with which the image is shared.
--
-- * 'dipsMaxResults' - The maximum size of each page of results.
--
-- * 'dipsName' - The name of the private image for which to describe permissions. The image must be one that you own.
describeImagePermissions
    :: Text -- ^ 'dipsName'
    -> DescribeImagePermissions
describeImagePermissions pName_ =
  DescribeImagePermissions'
    { _dipsNextToken = Nothing
    , _dipsSharedAWSAccountIds = Nothing
    , _dipsMaxResults = Nothing
    , _dipsName = pName_
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dipsNextToken :: Lens' DescribeImagePermissions (Maybe Text)
dipsNextToken = lens _dipsNextToken (\ s a -> s{_dipsNextToken = a})

-- | The 12-digit identifier of one or more AWS accounts with which the image is shared.
dipsSharedAWSAccountIds :: Lens' DescribeImagePermissions (Maybe (NonEmpty Text))
dipsSharedAWSAccountIds = lens _dipsSharedAWSAccountIds (\ s a -> s{_dipsSharedAWSAccountIds = a}) . mapping _List1

-- | The maximum size of each page of results.
dipsMaxResults :: Lens' DescribeImagePermissions (Maybe Natural)
dipsMaxResults = lens _dipsMaxResults (\ s a -> s{_dipsMaxResults = a}) . mapping _Nat

-- | The name of the private image for which to describe permissions. The image must be one that you own.
dipsName :: Lens' DescribeImagePermissions Text
dipsName = lens _dipsName (\ s a -> s{_dipsName = a})

instance AWSRequest DescribeImagePermissions where
        type Rs DescribeImagePermissions =
             DescribeImagePermissionsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImagePermissionsResponse' <$>
                   (x .?> "SharedImagePermissionsList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImagePermissions where

instance NFData DescribeImagePermissions where

instance ToHeaders DescribeImagePermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeImagePermissions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeImagePermissions where
        toJSON DescribeImagePermissions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dipsNextToken,
                  ("SharedAwsAccountIds" .=) <$>
                    _dipsSharedAWSAccountIds,
                  ("MaxResults" .=) <$> _dipsMaxResults,
                  Just ("Name" .= _dipsName)])

instance ToPath DescribeImagePermissions where
        toPath = const "/"

instance ToQuery DescribeImagePermissions where
        toQuery = const mempty

-- | /See:/ 'describeImagePermissionsResponse' smart constructor.
data DescribeImagePermissionsResponse = DescribeImagePermissionsResponse'
  { _dipsrsSharedImagePermissionsList :: !(Maybe [SharedImagePermissions])
  , _dipsrsNextToken                  :: !(Maybe Text)
  , _dipsrsName                       :: !(Maybe Text)
  , _dipsrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsrsSharedImagePermissionsList' - The permissions for a private image that you own.
--
-- * 'dipsrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'dipsrsName' - The name of the private image.
--
-- * 'dipsrsResponseStatus' - -- | The response status code.
describeImagePermissionsResponse
    :: Int -- ^ 'dipsrsResponseStatus'
    -> DescribeImagePermissionsResponse
describeImagePermissionsResponse pResponseStatus_ =
  DescribeImagePermissionsResponse'
    { _dipsrsSharedImagePermissionsList = Nothing
    , _dipsrsNextToken = Nothing
    , _dipsrsName = Nothing
    , _dipsrsResponseStatus = pResponseStatus_
    }


-- | The permissions for a private image that you own.
dipsrsSharedImagePermissionsList :: Lens' DescribeImagePermissionsResponse [SharedImagePermissions]
dipsrsSharedImagePermissionsList = lens _dipsrsSharedImagePermissionsList (\ s a -> s{_dipsrsSharedImagePermissionsList = a}) . _Default . _Coerce

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
dipsrsNextToken :: Lens' DescribeImagePermissionsResponse (Maybe Text)
dipsrsNextToken = lens _dipsrsNextToken (\ s a -> s{_dipsrsNextToken = a})

-- | The name of the private image.
dipsrsName :: Lens' DescribeImagePermissionsResponse (Maybe Text)
dipsrsName = lens _dipsrsName (\ s a -> s{_dipsrsName = a})

-- | -- | The response status code.
dipsrsResponseStatus :: Lens' DescribeImagePermissionsResponse Int
dipsrsResponseStatus = lens _dipsrsResponseStatus (\ s a -> s{_dipsrsResponseStatus = a})

instance NFData DescribeImagePermissionsResponse
         where
