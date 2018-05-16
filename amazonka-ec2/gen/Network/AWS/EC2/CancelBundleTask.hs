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
-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bundling operation for an instance store-backed Windows instance.
--
--
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Creating a Request
      cancelBundleTask
    , CancelBundleTask
    -- * Request Lenses
    , cbtDryRun
    , cbtBundleId

    -- * Destructuring the Response
    , cancelBundleTaskResponse
    , CancelBundleTaskResponse
    -- * Response Lenses
    , cbtrsBundleTask
    , cbtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CancelBundleTask.
--
--
--
-- /See:/ 'cancelBundleTask' smart constructor.
data CancelBundleTask = CancelBundleTask'
  { _cbtDryRun   :: !(Maybe Bool)
  , _cbtBundleId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelBundleTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cbtBundleId' - The ID of the bundle task.
cancelBundleTask
    :: Text -- ^ 'cbtBundleId'
    -> CancelBundleTask
cancelBundleTask pBundleId_ =
  CancelBundleTask' {_cbtDryRun = Nothing, _cbtBundleId = pBundleId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cbtDryRun :: Lens' CancelBundleTask (Maybe Bool)
cbtDryRun = lens _cbtDryRun (\ s a -> s{_cbtDryRun = a})

-- | The ID of the bundle task.
cbtBundleId :: Lens' CancelBundleTask Text
cbtBundleId = lens _cbtBundleId (\ s a -> s{_cbtBundleId = a})

instance AWSRequest CancelBundleTask where
        type Rs CancelBundleTask = CancelBundleTaskResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CancelBundleTaskResponse' <$>
                   (x .@? "bundleInstanceTask") <*> (pure (fromEnum s)))

instance Hashable CancelBundleTask where

instance NFData CancelBundleTask where

instance ToHeaders CancelBundleTask where
        toHeaders = const mempty

instance ToPath CancelBundleTask where
        toPath = const "/"

instance ToQuery CancelBundleTask where
        toQuery CancelBundleTask'{..}
          = mconcat
              ["Action" =: ("CancelBundleTask" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cbtDryRun, "BundleId" =: _cbtBundleId]

-- | Contains the output of CancelBundleTask.
--
--
--
-- /See:/ 'cancelBundleTaskResponse' smart constructor.
data CancelBundleTaskResponse = CancelBundleTaskResponse'
  { _cbtrsBundleTask     :: !(Maybe BundleTask)
  , _cbtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelBundleTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbtrsBundleTask' - Information about the bundle task.
--
-- * 'cbtrsResponseStatus' - -- | The response status code.
cancelBundleTaskResponse
    :: Int -- ^ 'cbtrsResponseStatus'
    -> CancelBundleTaskResponse
cancelBundleTaskResponse pResponseStatus_ =
  CancelBundleTaskResponse'
    {_cbtrsBundleTask = Nothing, _cbtrsResponseStatus = pResponseStatus_}


-- | Information about the bundle task.
cbtrsBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrsBundleTask = lens _cbtrsBundleTask (\ s a -> s{_cbtrsBundleTask = a})

-- | -- | The response status code.
cbtrsResponseStatus :: Lens' CancelBundleTaskResponse Int
cbtrsResponseStatus = lens _cbtrsResponseStatus (\ s a -> s{_cbtrsResponseStatus = a})

instance NFData CancelBundleTaskResponse where
