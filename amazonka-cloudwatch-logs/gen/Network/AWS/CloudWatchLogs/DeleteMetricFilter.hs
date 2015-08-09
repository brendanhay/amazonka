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
-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metric filter associated with the specified log group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteMetricFilter.html AWS API Reference> for DeleteMetricFilter.
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
    (
    -- * Creating a Request
      deleteMetricFilter
    , DeleteMetricFilter
    -- * Request Lenses
    , delLogGroupName
    , delFilterName

    -- * Destructuring the Response
    , deleteMetricFilterResponse
    , DeleteMetricFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteMetricFilter' smart constructor.
data DeleteMetricFilter = DeleteMetricFilter'
    { _delLogGroupName :: !Text
    , _delFilterName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delLogGroupName'
--
-- * 'delFilterName'
deleteMetricFilter
    :: Text -- ^ 'delLogGroupName'
    -> Text -- ^ 'delFilterName'
    -> DeleteMetricFilter
deleteMetricFilter pLogGroupName_ pFilterName_ =
    DeleteMetricFilter'
    { _delLogGroupName = pLogGroupName_
    , _delFilterName = pFilterName_
    }

-- | The name of the log group that is associated with the metric filter to
-- delete.
delLogGroupName :: Lens' DeleteMetricFilter Text
delLogGroupName = lens _delLogGroupName (\ s a -> s{_delLogGroupName = a});

-- | The name of the metric filter to delete.
delFilterName :: Lens' DeleteMetricFilter Text
delFilterName = lens _delFilterName (\ s a -> s{_delFilterName = a});

instance AWSRequest DeleteMetricFilter where
        type Sv DeleteMetricFilter = CloudWatchLogs
        type Rs DeleteMetricFilter =
             DeleteMetricFilterResponse
        request = postJSON
        response = receiveNull DeleteMetricFilterResponse'

instance ToHeaders DeleteMetricFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteMetricFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMetricFilter where
        toJSON DeleteMetricFilter'{..}
          = object
              ["logGroupName" .= _delLogGroupName,
               "filterName" .= _delFilterName]

instance ToPath DeleteMetricFilter where
        toPath = const "/"

instance ToQuery DeleteMetricFilter where
        toQuery = const mempty

-- | /See:/ 'deleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse =
    DeleteMetricFilterResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMetricFilterResponse' with the minimum fields required to make a request.
--
deleteMetricFilterResponse
    :: DeleteMetricFilterResponse
deleteMetricFilterResponse = DeleteMetricFilterResponse'
