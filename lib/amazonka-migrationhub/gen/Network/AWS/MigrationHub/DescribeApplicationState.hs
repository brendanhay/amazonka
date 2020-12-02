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
-- Module      : Network.AWS.MigrationHub.DescribeApplicationState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the migration status of an application.
--
--
module Network.AWS.MigrationHub.DescribeApplicationState
    (
    -- * Creating a Request
      describeApplicationState
    , DescribeApplicationState
    -- * Request Lenses
    , dasApplicationId

    -- * Destructuring the Response
    , describeApplicationStateResponse
    , DescribeApplicationStateResponse
    -- * Response Lenses
    , dasrsLastUpdatedTime
    , dasrsApplicationStatus
    , dasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeApplicationState' smart constructor.
newtype DescribeApplicationState = DescribeApplicationState'
  { _dasApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasApplicationId' - The configurationId in ADS that uniquely identifies the grouped application.
describeApplicationState
    :: Text -- ^ 'dasApplicationId'
    -> DescribeApplicationState
describeApplicationState pApplicationId_ =
  DescribeApplicationState' {_dasApplicationId = pApplicationId_}


-- | The configurationId in ADS that uniquely identifies the grouped application.
dasApplicationId :: Lens' DescribeApplicationState Text
dasApplicationId = lens _dasApplicationId (\ s a -> s{_dasApplicationId = a})

instance AWSRequest DescribeApplicationState where
        type Rs DescribeApplicationState =
             DescribeApplicationStateResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 DescribeApplicationStateResponse' <$>
                   (x .?> "LastUpdatedTime") <*>
                     (x .?> "ApplicationStatus")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeApplicationState where

instance NFData DescribeApplicationState where

instance ToHeaders DescribeApplicationState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.DescribeApplicationState" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeApplicationState where
        toJSON DescribeApplicationState'{..}
          = object
              (catMaybes
                 [Just ("ApplicationId" .= _dasApplicationId)])

instance ToPath DescribeApplicationState where
        toPath = const "/"

instance ToQuery DescribeApplicationState where
        toQuery = const mempty

-- | /See:/ 'describeApplicationStateResponse' smart constructor.
data DescribeApplicationStateResponse = DescribeApplicationStateResponse'
  { _dasrsLastUpdatedTime   :: !(Maybe POSIX)
  , _dasrsApplicationStatus :: !(Maybe ApplicationStatus)
  , _dasrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApplicationStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsLastUpdatedTime' - The timestamp when the application status was last updated.
--
-- * 'dasrsApplicationStatus' - Status of the application - Not Started, In-Progress, Complete.
--
-- * 'dasrsResponseStatus' - -- | The response status code.
describeApplicationStateResponse
    :: Int -- ^ 'dasrsResponseStatus'
    -> DescribeApplicationStateResponse
describeApplicationStateResponse pResponseStatus_ =
  DescribeApplicationStateResponse'
    { _dasrsLastUpdatedTime = Nothing
    , _dasrsApplicationStatus = Nothing
    , _dasrsResponseStatus = pResponseStatus_
    }


-- | The timestamp when the application status was last updated.
dasrsLastUpdatedTime :: Lens' DescribeApplicationStateResponse (Maybe UTCTime)
dasrsLastUpdatedTime = lens _dasrsLastUpdatedTime (\ s a -> s{_dasrsLastUpdatedTime = a}) . mapping _Time

-- | Status of the application - Not Started, In-Progress, Complete.
dasrsApplicationStatus :: Lens' DescribeApplicationStateResponse (Maybe ApplicationStatus)
dasrsApplicationStatus = lens _dasrsApplicationStatus (\ s a -> s{_dasrsApplicationStatus = a})

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DescribeApplicationStateResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\ s a -> s{_dasrsResponseStatus = a})

instance NFData DescribeApplicationStateResponse
         where
