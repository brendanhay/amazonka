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
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetApplications.html AWS API Reference> for BatchGetApplications.
module Network.AWS.CodeDeploy.BatchGetApplications
    (
    -- * Creating a Request
      batchGetApplications
    , BatchGetApplications
    -- * Request Lenses
    , bgaApplicationNames

    -- * Destructuring the Response
    , batchGetApplicationsResponse
    , BatchGetApplicationsResponse
    -- * Response Lenses
    , bgarsApplicationsInfo
    , bgarsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a batch get applications operation.
--
-- /See:/ 'batchGetApplications' smart constructor.
newtype BatchGetApplications = BatchGetApplications'
    { _bgaApplicationNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchGetApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgaApplicationNames'
batchGetApplications
    :: BatchGetApplications
batchGetApplications =
    BatchGetApplications'
    { _bgaApplicationNames = Nothing
    }

-- | A list of application names, with multiple application names separated
-- by spaces.
bgaApplicationNames :: Lens' BatchGetApplications [Text]
bgaApplicationNames = lens _bgaApplicationNames (\ s a -> s{_bgaApplicationNames = a}) . _Default . _Coerce;

instance AWSRequest BatchGetApplications where
        type Sv BatchGetApplications = CodeDeploy
        type Rs BatchGetApplications =
             BatchGetApplicationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetApplicationsResponse' <$>
                   (x .?> "applicationsInfo" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders BatchGetApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetApplications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetApplications where
        toJSON BatchGetApplications'{..}
          = object ["applicationNames" .= _bgaApplicationNames]

instance ToPath BatchGetApplications where
        toPath = const "/"

instance ToQuery BatchGetApplications where
        toQuery = const mempty

-- | Represents the output of a batch get applications operation.
--
-- /See:/ 'batchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
    { _bgarsApplicationsInfo :: !(Maybe [ApplicationInfo])
    , _bgarsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchGetApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarsApplicationsInfo'
--
-- * 'bgarsStatus'
batchGetApplicationsResponse
    :: Int -- ^ 'bgarsStatus'
    -> BatchGetApplicationsResponse
batchGetApplicationsResponse pStatus_ =
    BatchGetApplicationsResponse'
    { _bgarsApplicationsInfo = Nothing
    , _bgarsStatus = pStatus_
    }

-- | Information about the applications.
bgarsApplicationsInfo :: Lens' BatchGetApplicationsResponse [ApplicationInfo]
bgarsApplicationsInfo = lens _bgarsApplicationsInfo (\ s a -> s{_bgarsApplicationsInfo = a}) . _Default . _Coerce;

-- | The response status code.
bgarsStatus :: Lens' BatchGetApplicationsResponse Int
bgarsStatus = lens _bgarsStatus (\ s a -> s{_bgarsStatus = a});
