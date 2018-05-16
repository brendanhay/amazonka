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
-- Module      : Network.AWS.CloudWatch.DeleteDashboards
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all dashboards that you specify. You may specify up to 100 dashboards to delete. If there is an error during this call, no dashboards are deleted.
--
--
module Network.AWS.CloudWatch.DeleteDashboards
    (
    -- * Creating a Request
      deleteDashboards
    , DeleteDashboards
    -- * Request Lenses
    , ddDashboardNames

    -- * Destructuring the Response
    , deleteDashboardsResponse
    , DeleteDashboardsResponse
    -- * Response Lenses
    , ddrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDashboards' smart constructor.
newtype DeleteDashboards = DeleteDashboards'
  { _ddDashboardNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDashboards' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDashboardNames' - The dashboards to be deleted. This parameter is required.
deleteDashboards
    :: DeleteDashboards
deleteDashboards = DeleteDashboards' {_ddDashboardNames = mempty}


-- | The dashboards to be deleted. This parameter is required.
ddDashboardNames :: Lens' DeleteDashboards [Text]
ddDashboardNames = lens _ddDashboardNames (\ s a -> s{_ddDashboardNames = a}) . _Coerce

instance AWSRequest DeleteDashboards where
        type Rs DeleteDashboards = DeleteDashboardsResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "DeleteDashboardsResult"
              (\ s h x ->
                 DeleteDashboardsResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDashboards where

instance NFData DeleteDashboards where

instance ToHeaders DeleteDashboards where
        toHeaders = const mempty

instance ToPath DeleteDashboards where
        toPath = const "/"

instance ToQuery DeleteDashboards where
        toQuery DeleteDashboards'{..}
          = mconcat
              ["Action" =: ("DeleteDashboards" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "DashboardNames" =:
                 toQueryList "member" _ddDashboardNames]

-- | /See:/ 'deleteDashboardsResponse' smart constructor.
newtype DeleteDashboardsResponse = DeleteDashboardsResponse'
  { _ddrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDashboardsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsResponseStatus' - -- | The response status code.
deleteDashboardsResponse
    :: Int -- ^ 'ddrsResponseStatus'
    -> DeleteDashboardsResponse
deleteDashboardsResponse pResponseStatus_ =
  DeleteDashboardsResponse' {_ddrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddrsResponseStatus :: Lens' DeleteDashboardsResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\ s a -> s{_ddrsResponseStatus = a})

instance NFData DeleteDashboardsResponse where
