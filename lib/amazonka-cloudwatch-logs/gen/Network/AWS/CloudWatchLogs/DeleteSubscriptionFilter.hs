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
-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription filter.
--
--
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
    (
    -- * Creating a Request
      deleteSubscriptionFilter
    , DeleteSubscriptionFilter
    -- * Request Lenses
    , dLogGroupName
    , dFilterName

    -- * Destructuring the Response
    , deleteSubscriptionFilterResponse
    , DeleteSubscriptionFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSubscriptionFilter' smart constructor.
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
  { _dLogGroupName :: !Text
  , _dFilterName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriptionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLogGroupName' - The name of the log group.
--
-- * 'dFilterName' - The name of the subscription filter.
deleteSubscriptionFilter
    :: Text -- ^ 'dLogGroupName'
    -> Text -- ^ 'dFilterName'
    -> DeleteSubscriptionFilter
deleteSubscriptionFilter pLogGroupName_ pFilterName_ =
  DeleteSubscriptionFilter'
    {_dLogGroupName = pLogGroupName_, _dFilterName = pFilterName_}


-- | The name of the log group.
dLogGroupName :: Lens' DeleteSubscriptionFilter Text
dLogGroupName = lens _dLogGroupName (\ s a -> s{_dLogGroupName = a})

-- | The name of the subscription filter.
dFilterName :: Lens' DeleteSubscriptionFilter Text
dFilterName = lens _dFilterName (\ s a -> s{_dFilterName = a})

instance AWSRequest DeleteSubscriptionFilter where
        type Rs DeleteSubscriptionFilter =
             DeleteSubscriptionFilterResponse
        request = postJSON cloudWatchLogs
        response
          = receiveNull DeleteSubscriptionFilterResponse'

instance Hashable DeleteSubscriptionFilter where

instance NFData DeleteSubscriptionFilter where

instance ToHeaders DeleteSubscriptionFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteSubscriptionFilter" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubscriptionFilter where
        toJSON DeleteSubscriptionFilter'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _dLogGroupName),
                  Just ("filterName" .= _dFilterName)])

instance ToPath DeleteSubscriptionFilter where
        toPath = const "/"

instance ToQuery DeleteSubscriptionFilter where
        toQuery = const mempty

-- | /See:/ 'deleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse =
  DeleteSubscriptionFilterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriptionFilterResponse' with the minimum fields required to make a request.
--
deleteSubscriptionFilterResponse
    :: DeleteSubscriptionFilterResponse
deleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'


instance NFData DeleteSubscriptionFilterResponse
         where
