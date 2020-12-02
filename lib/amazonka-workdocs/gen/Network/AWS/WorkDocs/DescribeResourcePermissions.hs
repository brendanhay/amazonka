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
-- Module      : Network.AWS.WorkDocs.DescribeResourcePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a specified resource.
--
--
module Network.AWS.WorkDocs.DescribeResourcePermissions
    (
    -- * Creating a Request
      describeResourcePermissions
    , DescribeResourcePermissions
    -- * Request Lenses
    , drpPrincipalId
    , drpAuthenticationToken
    , drpMarker
    , drpLimit
    , drpResourceId

    -- * Destructuring the Response
    , describeResourcePermissionsResponse
    , DescribeResourcePermissionsResponse
    -- * Response Lenses
    , drprsPrincipals
    , drprsMarker
    , drprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeResourcePermissions' smart constructor.
data DescribeResourcePermissions = DescribeResourcePermissions'
  { _drpPrincipalId         :: !(Maybe Text)
  , _drpAuthenticationToken :: !(Maybe (Sensitive Text))
  , _drpMarker              :: !(Maybe Text)
  , _drpLimit               :: !(Maybe Nat)
  , _drpResourceId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourcePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpPrincipalId' - The ID of the principal to filter permissions by.
--
-- * 'drpAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'drpMarker' - The marker for the next set of results. (You received this marker from a previous call)
--
-- * 'drpLimit' - The maximum number of items to return with this call.
--
-- * 'drpResourceId' - The ID of the resource.
describeResourcePermissions
    :: Text -- ^ 'drpResourceId'
    -> DescribeResourcePermissions
describeResourcePermissions pResourceId_ =
  DescribeResourcePermissions'
    { _drpPrincipalId = Nothing
    , _drpAuthenticationToken = Nothing
    , _drpMarker = Nothing
    , _drpLimit = Nothing
    , _drpResourceId = pResourceId_
    }


-- | The ID of the principal to filter permissions by.
drpPrincipalId :: Lens' DescribeResourcePermissions (Maybe Text)
drpPrincipalId = lens _drpPrincipalId (\ s a -> s{_drpPrincipalId = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
drpAuthenticationToken :: Lens' DescribeResourcePermissions (Maybe Text)
drpAuthenticationToken = lens _drpAuthenticationToken (\ s a -> s{_drpAuthenticationToken = a}) . mapping _Sensitive

-- | The marker for the next set of results. (You received this marker from a previous call)
drpMarker :: Lens' DescribeResourcePermissions (Maybe Text)
drpMarker = lens _drpMarker (\ s a -> s{_drpMarker = a})

-- | The maximum number of items to return with this call.
drpLimit :: Lens' DescribeResourcePermissions (Maybe Natural)
drpLimit = lens _drpLimit (\ s a -> s{_drpLimit = a}) . mapping _Nat

-- | The ID of the resource.
drpResourceId :: Lens' DescribeResourcePermissions Text
drpResourceId = lens _drpResourceId (\ s a -> s{_drpResourceId = a})

instance AWSRequest DescribeResourcePermissions where
        type Rs DescribeResourcePermissions =
             DescribeResourcePermissionsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourcePermissionsResponse' <$>
                   (x .?> "Principals" .!@ mempty) <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeResourcePermissions where

instance NFData DescribeResourcePermissions where

instance ToHeaders DescribeResourcePermissions where
        toHeaders DescribeResourcePermissions'{..}
          = mconcat
              ["Authentication" =# _drpAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeResourcePermissions where
        toPath DescribeResourcePermissions'{..}
          = mconcat
              ["/api/v1/resources/", toBS _drpResourceId,
               "/permissions"]

instance ToQuery DescribeResourcePermissions where
        toQuery DescribeResourcePermissions'{..}
          = mconcat
              ["principalId" =: _drpPrincipalId,
               "marker" =: _drpMarker, "limit" =: _drpLimit]

-- | /See:/ 'describeResourcePermissionsResponse' smart constructor.
data DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse'
  { _drprsPrincipals     :: !(Maybe [Principal])
  , _drprsMarker         :: !(Maybe Text)
  , _drprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourcePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsPrincipals' - The principals.
--
-- * 'drprsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'drprsResponseStatus' - -- | The response status code.
describeResourcePermissionsResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DescribeResourcePermissionsResponse
describeResourcePermissionsResponse pResponseStatus_ =
  DescribeResourcePermissionsResponse'
    { _drprsPrincipals = Nothing
    , _drprsMarker = Nothing
    , _drprsResponseStatus = pResponseStatus_
    }


-- | The principals.
drprsPrincipals :: Lens' DescribeResourcePermissionsResponse [Principal]
drprsPrincipals = lens _drprsPrincipals (\ s a -> s{_drprsPrincipals = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
drprsMarker :: Lens' DescribeResourcePermissionsResponse (Maybe Text)
drprsMarker = lens _drprsMarker (\ s a -> s{_drprsMarker = a})

-- | -- | The response status code.
drprsResponseStatus :: Lens' DescribeResourcePermissionsResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a})

instance NFData DescribeResourcePermissionsResponse
         where
