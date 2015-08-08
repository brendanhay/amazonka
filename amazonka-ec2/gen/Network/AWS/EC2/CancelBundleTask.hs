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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html AWS API Reference> for CancelBundleTask.
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Creating a Request
      CancelBundleTask
    , cancelBundleTask
    -- * Request Lenses
    , cbtDryRun
    , cbtBundleId

    -- * Destructuring the Response
    , CancelBundleTaskResponse
    , cancelBundleTaskResponse
    -- * Response Lenses
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
-- * 'cbtDryRun'
--
-- * 'cbtBundleId'
data CancelBundleTask = CancelBundleTask'
    { _cbtDryRun   :: !(Maybe Bool)
    , _cbtBundleId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelBundleTask' smart constructor.
cancelBundleTask :: Text -> CancelBundleTask
cancelBundleTask pBundleId_ =
    CancelBundleTask'
    { _cbtDryRun = Nothing
    , _cbtBundleId = pBundleId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cbtDryRun :: Lens' CancelBundleTask (Maybe Bool)
cbtDryRun = lens _cbtDryRun (\ s a -> s{_cbtDryRun = a});

-- | The ID of the bundle task.
cbtBundleId :: Lens' CancelBundleTask Text
cbtBundleId = lens _cbtBundleId (\ s a -> s{_cbtBundleId = a});

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
               "DryRun" =: _cbtDryRun, "BundleId" =: _cbtBundleId]

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
cancelBundleTaskResponse pStatus_ =
    CancelBundleTaskResponse'
    { _cbtrsBundleTask = Nothing
    , _cbtrsStatus = pStatus_
    }

-- | Information about the bundle task.
cbtrsBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrsBundleTask = lens _cbtrsBundleTask (\ s a -> s{_cbtrsBundleTask = a});

-- | Undocumented member.
cbtrsStatus :: Lens' CancelBundleTaskResponse Int
cbtrsStatus = lens _cbtrsStatus (\ s a -> s{_cbtrsStatus = a});
