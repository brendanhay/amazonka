{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows
-- instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html>
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Request
      CancelBundleTask
    -- ** Request constructor
    , cancelBundleTask
    -- ** Request lenses
    , cbtrqDryRun
    , cbtrqBundleId

    -- * Response
    , CancelBundleTaskResponse
    -- ** Response constructor
    , cancelBundleTaskResponse
    -- ** Response lenses
    , cbtrsBundleTask
    , cbtrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelBundleTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtrqDryRun'
--
-- * 'cbtrqBundleId'
data CancelBundleTask = CancelBundleTask'
    { _cbtrqDryRun   :: !(Maybe Bool)
    , _cbtrqBundleId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelBundleTask' smart constructor.
cancelBundleTask :: Text -> CancelBundleTask
cancelBundleTask pBundleId =
    CancelBundleTask'
    { _cbtrqDryRun = Nothing
    , _cbtrqBundleId = pBundleId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cbtrqDryRun :: Lens' CancelBundleTask (Maybe Bool)
cbtrqDryRun = lens _cbtrqDryRun (\ s a -> s{_cbtrqDryRun = a});

-- | The ID of the bundle task.
cbtrqBundleId :: Lens' CancelBundleTask Text
cbtrqBundleId = lens _cbtrqBundleId (\ s a -> s{_cbtrqBundleId = a});

instance AWSRequest CancelBundleTask where
        type Sv CancelBundleTask = EC2
        type Rs CancelBundleTask = CancelBundleTaskResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CancelBundleTaskResponse' <$>
                   (x .@? "bundleInstanceTask") <*> (pure (fromEnum s)))

instance ToHeaders CancelBundleTask where
        toHeaders = const mempty

instance ToPath CancelBundleTask where
        toPath = const "/"

instance ToQuery CancelBundleTask where
        toQuery CancelBundleTask'{..}
          = mconcat
              ["Action" =: ("CancelBundleTask" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cbtrqDryRun,
               "BundleId" =: _cbtrqBundleId]

-- | /See:/ 'cancelBundleTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtrsBundleTask'
--
-- * 'cbtrsStatus'
data CancelBundleTaskResponse = CancelBundleTaskResponse'
    { _cbtrsBundleTask :: !(Maybe BundleTask)
    , _cbtrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelBundleTaskResponse' smart constructor.
cancelBundleTaskResponse :: Int -> CancelBundleTaskResponse
cancelBundleTaskResponse pStatus =
    CancelBundleTaskResponse'
    { _cbtrsBundleTask = Nothing
    , _cbtrsStatus = pStatus
    }

-- | Information about the bundle task.
cbtrsBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrsBundleTask = lens _cbtrsBundleTask (\ s a -> s{_cbtrsBundleTask = a});

-- | FIXME: Undocumented member.
cbtrsStatus :: Lens' CancelBundleTaskResponse Int
cbtrsStatus = lens _cbtrsStatus (\ s a -> s{_cbtrsStatus = a});
