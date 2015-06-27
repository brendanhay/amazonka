{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deregisters the specified AMI. After you deregister an AMI, it can\'t be
-- used to launch new instances.
--
-- This command does not delete the AMI.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html>
module Network.AWS.EC2.DeregisterImage
    (
    -- * Request
      DeregisterImage
    -- ** Request constructor
    , deregisterImage
    -- ** Request lenses
    , diDryRun
    , diImageId

    -- * Response
    , DeregisterImageResponse
    -- ** Response constructor
    , deregisterImageResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterImage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDryRun'
--
-- * 'diImageId'
data DeregisterImage = DeregisterImage'
    { _diDryRun  :: !(Maybe Bool)
    , _diImageId :: !Text
    } deriving (Eq,Read,Show)

-- | 'DeregisterImage' smart constructor.
deregisterImage :: Text -> DeregisterImage
deregisterImage pImageId =
    DeregisterImage'
    { _diDryRun = Nothing
    , _diImageId = pImageId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diDryRun :: Lens' DeregisterImage (Maybe Bool)
diDryRun = lens _diDryRun (\ s a -> s{_diDryRun = a});

-- | The ID of the AMI.
diImageId :: Lens' DeregisterImage Text
diImageId = lens _diImageId (\ s a -> s{_diImageId = a});

instance AWSRequest DeregisterImage where
        type Sv DeregisterImage = EC2
        type Rs DeregisterImage = DeregisterImageResponse
        request = post
        response = receiveNull DeregisterImageResponse'

instance ToHeaders DeregisterImage where
        toHeaders = const mempty

instance ToPath DeregisterImage where
        toPath = const "/"

instance ToQuery DeregisterImage where
        toQuery DeregisterImage'{..}
          = mconcat
              ["Action" =: ("DeregisterImage" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _diDryRun, "ImageId" =: _diImageId]

-- | /See:/ 'deregisterImageResponse' smart constructor.
data DeregisterImageResponse =
    DeregisterImageResponse'
    deriving (Eq,Read,Show)

-- | 'DeregisterImageResponse' smart constructor.
deregisterImageResponse :: DeregisterImageResponse
deregisterImageResponse = DeregisterImageResponse'
