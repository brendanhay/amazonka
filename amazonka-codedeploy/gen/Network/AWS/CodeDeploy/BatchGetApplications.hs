{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets information about one or more applications.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetApplications.html>
module Network.AWS.CodeDeploy.BatchGetApplications
    (
    -- * Request
      BatchGetApplications
    -- ** Request constructor
    , batchGetApplications
    -- ** Request lenses
    , bgaApplicationNames

    -- * Response
    , BatchGetApplicationsResponse
    -- ** Response constructor
    , batchGetApplicationsResponse
    -- ** Response lenses
    , bgarApplicationsInfo
    , bgarStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a batch get applications operation.
--
-- /See:/ 'batchGetApplications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgaApplicationNames'
newtype BatchGetApplications = BatchGetApplications'
    { _bgaApplicationNames :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetApplications' smart constructor.
batchGetApplications :: BatchGetApplications
batchGetApplications =
    BatchGetApplications'
    { _bgaApplicationNames = Nothing
    }

-- | A list of application names, with multiple application names separated
-- by spaces.
bgaApplicationNames :: Lens' BatchGetApplications [Text]
bgaApplicationNames = lens _bgaApplicationNames (\ s a -> s{_bgaApplicationNames = a}) . _Default;

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgarApplicationsInfo'
--
-- * 'bgarStatus'
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
    { _bgarApplicationsInfo :: !(Maybe [ApplicationInfo])
    , _bgarStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchGetApplicationsResponse' smart constructor.
batchGetApplicationsResponse :: Int -> BatchGetApplicationsResponse
batchGetApplicationsResponse pStatus =
    BatchGetApplicationsResponse'
    { _bgarApplicationsInfo = Nothing
    , _bgarStatus = pStatus
    }

-- | Information about the applications.
bgarApplicationsInfo :: Lens' BatchGetApplicationsResponse [ApplicationInfo]
bgarApplicationsInfo = lens _bgarApplicationsInfo (\ s a -> s{_bgarApplicationsInfo = a}) . _Default;

-- | FIXME: Undocumented member.
bgarStatus :: Lens' BatchGetApplicationsResponse Int
bgarStatus = lens _bgarStatus (\ s a -> s{_bgarStatus = a});
