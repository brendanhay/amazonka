{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.BundleInstance
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

-- | Bundles an Amazon instance store-backed Windows instance.
--
-- During bundling, only the root device volume (C:\\) is bundled. Data on
-- other instance store volumes is not preserved.
--
-- This action is not applicable for Linux\/Unix instances or Windows
-- instances that are backed by Amazon EBS.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/Creating_InstanceStoreBacked_WinAMI.html Creating an Instance Store-Backed Windows AMI>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-BundleInstance.html>
module Network.AWS.EC2.BundleInstance
    (
    -- * Request
      BundleInstance
    -- ** Request constructor
    , bundleInstance
    -- ** Request lenses
    , biDryRun
    , biInstanceId
    , biStorage

    -- * Response
    , BundleInstanceResponse
    -- ** Response constructor
    , bundleInstanceResponse
    -- ** Response lenses
    , birBundleTask
    , birStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'bundleInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'biDryRun'
--
-- * 'biInstanceId'
--
-- * 'biStorage'
data BundleInstance = BundleInstance'
    { _biDryRun     :: !(Maybe Bool)
    , _biInstanceId :: !Text
    , _biStorage    :: !Storage
    } deriving (Eq,Read,Show)

-- | 'BundleInstance' smart constructor.
bundleInstance :: Text -> Storage -> BundleInstance
bundleInstance pInstanceId pStorage =
    BundleInstance'
    { _biDryRun = Nothing
    , _biInstanceId = pInstanceId
    , _biStorage = pStorage
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
biDryRun :: Lens' BundleInstance (Maybe Bool)
biDryRun = lens _biDryRun (\ s a -> s{_biDryRun = a});

-- | The ID of the instance to bundle.
--
-- Type: String
--
-- Default: None
--
-- Required: Yes
biInstanceId :: Lens' BundleInstance Text
biInstanceId = lens _biInstanceId (\ s a -> s{_biInstanceId = a});

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
biStorage :: Lens' BundleInstance Storage
biStorage = lens _biStorage (\ s a -> s{_biStorage = a});

instance AWSRequest BundleInstance where
        type Sv BundleInstance = EC2
        type Rs BundleInstance = BundleInstanceResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 BundleInstanceResponse' <$>
                   (x .@? "bundleInstanceTask") <*> (pure (fromEnum s)))

instance ToHeaders BundleInstance where
        toHeaders = const mempty

instance ToPath BundleInstance where
        toPath = const "/"

instance ToQuery BundleInstance where
        toQuery BundleInstance'{..}
          = mconcat
              ["Action" =: ("BundleInstance" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _biDryRun, "InstanceId" =: _biInstanceId,
               "Storage" =: _biStorage]

-- | /See:/ 'bundleInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'birBundleTask'
--
-- * 'birStatus'
data BundleInstanceResponse = BundleInstanceResponse'
    { _birBundleTask :: !(Maybe BundleTask)
    , _birStatus     :: !Int
    } deriving (Eq,Read,Show)

-- | 'BundleInstanceResponse' smart constructor.
bundleInstanceResponse :: Int -> BundleInstanceResponse
bundleInstanceResponse pStatus =
    BundleInstanceResponse'
    { _birBundleTask = Nothing
    , _birStatus = pStatus
    }

-- | Information about the bundle task.
birBundleTask :: Lens' BundleInstanceResponse (Maybe BundleTask)
birBundleTask = lens _birBundleTask (\ s a -> s{_birBundleTask = a});

-- | FIXME: Undocumented member.
birStatus :: Lens' BundleInstanceResponse Int
birStatus = lens _birStatus (\ s a -> s{_birStatus = a});
