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
-- Module      : Network.AWS.CloudWatch.GetDashboard
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details of the dashboard that you specify.
--
--
-- To copy an existing dashboard, use @GetDashboard@ , and then use the data returned within @DashboardBody@ as the template for the new dashboard when you call @PutDashboard@ to create the copy.
--
module Network.AWS.CloudWatch.GetDashboard
    (
    -- * Creating a Request
      getDashboard
    , GetDashboard
    -- * Request Lenses
    , gdDashboardName

    -- * Destructuring the Response
    , getDashboardResponse
    , GetDashboardResponse
    -- * Response Lenses
    , gdrsDashboardName
    , gdrsDashboardBody
    , gdrsDashboardARN
    , gdrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDashboard' smart constructor.
newtype GetDashboard = GetDashboard'
  { _gdDashboardName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDashboard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdDashboardName' - The name of the dashboard to be described.
getDashboard
    :: Text -- ^ 'gdDashboardName'
    -> GetDashboard
getDashboard pDashboardName_ =
  GetDashboard' {_gdDashboardName = pDashboardName_}


-- | The name of the dashboard to be described.
gdDashboardName :: Lens' GetDashboard Text
gdDashboardName = lens _gdDashboardName (\ s a -> s{_gdDashboardName = a})

instance AWSRequest GetDashboard where
        type Rs GetDashboard = GetDashboardResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "GetDashboardResult"
              (\ s h x ->
                 GetDashboardResponse' <$>
                   (x .@? "DashboardName") <*> (x .@? "DashboardBody")
                     <*> (x .@? "DashboardArn")
                     <*> (pure (fromEnum s)))

instance Hashable GetDashboard where

instance NFData GetDashboard where

instance ToHeaders GetDashboard where
        toHeaders = const mempty

instance ToPath GetDashboard where
        toPath = const "/"

instance ToQuery GetDashboard where
        toQuery GetDashboard'{..}
          = mconcat
              ["Action" =: ("GetDashboard" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "DashboardName" =: _gdDashboardName]

-- | /See:/ 'getDashboardResponse' smart constructor.
data GetDashboardResponse = GetDashboardResponse'
  { _gdrsDashboardName  :: !(Maybe Text)
  , _gdrsDashboardBody  :: !(Maybe Text)
  , _gdrsDashboardARN   :: !(Maybe Text)
  , _gdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDashboardResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsDashboardName' - The name of the dashboard.
--
-- * 'gdrsDashboardBody' - The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see 'CloudWatch-Dashboard-Body-Structure' .
--
-- * 'gdrsDashboardARN' - The Amazon Resource Name (ARN) of the dashboard.
--
-- * 'gdrsResponseStatus' - -- | The response status code.
getDashboardResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> GetDashboardResponse
getDashboardResponse pResponseStatus_ =
  GetDashboardResponse'
    { _gdrsDashboardName = Nothing
    , _gdrsDashboardBody = Nothing
    , _gdrsDashboardARN = Nothing
    , _gdrsResponseStatus = pResponseStatus_
    }


-- | The name of the dashboard.
gdrsDashboardName :: Lens' GetDashboardResponse (Maybe Text)
gdrsDashboardName = lens _gdrsDashboardName (\ s a -> s{_gdrsDashboardName = a})

-- | The detailed information about the dashboard, including what widgets are included and their location on the dashboard. For more information about the @DashboardBody@ syntax, see 'CloudWatch-Dashboard-Body-Structure' .
gdrsDashboardBody :: Lens' GetDashboardResponse (Maybe Text)
gdrsDashboardBody = lens _gdrsDashboardBody (\ s a -> s{_gdrsDashboardBody = a})

-- | The Amazon Resource Name (ARN) of the dashboard.
gdrsDashboardARN :: Lens' GetDashboardResponse (Maybe Text)
gdrsDashboardARN = lens _gdrsDashboardARN (\ s a -> s{_gdrsDashboardARN = a})

-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDashboardResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

instance NFData GetDashboardResponse where
