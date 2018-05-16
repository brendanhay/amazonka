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
-- Module      : Network.AWS.CloudWatch.PutDashboard
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dashboard if it does not already exist, or updates an existing dashboard. If you update a dashboard, the entire contents are replaced with what you specify here.
--
--
-- You can have up to 500 dashboards per account. All dashboards in your account are global, not region-specific.
--
-- A simple way to create a dashboard using @PutDashboard@ is to copy an existing dashboard. To copy an existing dashboard using the console, you can load the dashboard and then use the View/edit source command in the Actions menu to display the JSON block for that dashboard. Another way to copy a dashboard is to use @GetDashboard@ , and then use the data returned within @DashboardBody@ as the template for the new dashboard when you call @PutDashboard@ .
--
-- When you create a dashboard with @PutDashboard@ , a good practice is to add a text widget at the top of the dashboard with a message that the dashboard was created by script and should not be changed in the console. This message could also point console users to the location of the @DashboardBody@ script or the CloudFormation template used to create the dashboard.
--
module Network.AWS.CloudWatch.PutDashboard
    (
    -- * Creating a Request
      putDashboard
    , PutDashboard
    -- * Request Lenses
    , pdDashboardName
    , pdDashboardBody

    -- * Destructuring the Response
    , putDashboardResponse
    , PutDashboardResponse
    -- * Response Lenses
    , pdrsDashboardValidationMessages
    , pdrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putDashboard' smart constructor.
data PutDashboard = PutDashboard'
  { _pdDashboardName :: !Text
  , _pdDashboardBody :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutDashboard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDashboardName' - The name of the dashboard. If a dashboard with this name already exists, this call modifies that dashboard, replacing its current contents. Otherwise, a new dashboard is created. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, "-", and "_". This parameter is required.
--
-- * 'pdDashboardBody' - The detailed information about the dashboard in JSON format, including the widgets to include and their location on the dashboard. This parameter is required. For more information about the syntax, see 'CloudWatch-Dashboard-Body-Structure' .
putDashboard
    :: Text -- ^ 'pdDashboardName'
    -> Text -- ^ 'pdDashboardBody'
    -> PutDashboard
putDashboard pDashboardName_ pDashboardBody_ =
  PutDashboard'
    {_pdDashboardName = pDashboardName_, _pdDashboardBody = pDashboardBody_}


-- | The name of the dashboard. If a dashboard with this name already exists, this call modifies that dashboard, replacing its current contents. Otherwise, a new dashboard is created. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, "-", and "_". This parameter is required.
pdDashboardName :: Lens' PutDashboard Text
pdDashboardName = lens _pdDashboardName (\ s a -> s{_pdDashboardName = a})

-- | The detailed information about the dashboard in JSON format, including the widgets to include and their location on the dashboard. This parameter is required. For more information about the syntax, see 'CloudWatch-Dashboard-Body-Structure' .
pdDashboardBody :: Lens' PutDashboard Text
pdDashboardBody = lens _pdDashboardBody (\ s a -> s{_pdDashboardBody = a})

instance AWSRequest PutDashboard where
        type Rs PutDashboard = PutDashboardResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "PutDashboardResult"
              (\ s h x ->
                 PutDashboardResponse' <$>
                   (x .@? "DashboardValidationMessages" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable PutDashboard where

instance NFData PutDashboard where

instance ToHeaders PutDashboard where
        toHeaders = const mempty

instance ToPath PutDashboard where
        toPath = const "/"

instance ToQuery PutDashboard where
        toQuery PutDashboard'{..}
          = mconcat
              ["Action" =: ("PutDashboard" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "DashboardName" =: _pdDashboardName,
               "DashboardBody" =: _pdDashboardBody]

-- | /See:/ 'putDashboardResponse' smart constructor.
data PutDashboardResponse = PutDashboardResponse'
  { _pdrsDashboardValidationMessages :: !(Maybe [DashboardValidationMessage])
  , _pdrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutDashboardResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdrsDashboardValidationMessages' - If the input for @PutDashboard@ was correct and the dashboard was successfully created or modified, this result is empty. If this result includes only warning messages, then the input was valid enough for the dashboard to be created or modified, but some elements of the dashboard may not render. If this result includes error messages, the input was not valid and the operation failed.
--
-- * 'pdrsResponseStatus' - -- | The response status code.
putDashboardResponse
    :: Int -- ^ 'pdrsResponseStatus'
    -> PutDashboardResponse
putDashboardResponse pResponseStatus_ =
  PutDashboardResponse'
    { _pdrsDashboardValidationMessages = Nothing
    , _pdrsResponseStatus = pResponseStatus_
    }


-- | If the input for @PutDashboard@ was correct and the dashboard was successfully created or modified, this result is empty. If this result includes only warning messages, then the input was valid enough for the dashboard to be created or modified, but some elements of the dashboard may not render. If this result includes error messages, the input was not valid and the operation failed.
pdrsDashboardValidationMessages :: Lens' PutDashboardResponse [DashboardValidationMessage]
pdrsDashboardValidationMessages = lens _pdrsDashboardValidationMessages (\ s a -> s{_pdrsDashboardValidationMessages = a}) . _Default . _Coerce

-- | -- | The response status code.
pdrsResponseStatus :: Lens' PutDashboardResponse Int
pdrsResponseStatus = lens _pdrsResponseStatus (\ s a -> s{_pdrsResponseStatus = a})

instance NFData PutDashboardResponse where
