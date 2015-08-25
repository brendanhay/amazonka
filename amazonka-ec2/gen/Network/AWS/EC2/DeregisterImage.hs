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
-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified AMI. After you deregister an AMI, it can\'t be
-- used to launch new instances.
--
-- This command does not delete the AMI.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeregisterImage.html AWS API Reference> for DeregisterImage.
module Network.AWS.EC2.DeregisterImage
    (
    -- * Creating a Request
      deregisterImage
    , DeregisterImage
    -- * Request Lenses
    , diDryRun
    , diImageId

    -- * Destructuring the Response
    , deregisterImageResponse
    , DeregisterImageResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterImage' smart constructor.
data DeregisterImage = DeregisterImage'
    { _diDryRun  :: !(Maybe Bool)
    , _diImageId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDryRun'
--
-- * 'diImageId'
deregisterImage
    :: Text -- ^ 'diImageId'
    -> DeregisterImage
deregisterImage pImageId_ =
    DeregisterImage'
    { _diDryRun = Nothing
    , _diImageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
diDryRun :: Lens' DeregisterImage (Maybe Bool)
diDryRun = lens _diDryRun (\ s a -> s{_diDryRun = a});

-- | The ID of the AMI.
diImageId :: Lens' DeregisterImage Text
diImageId = lens _diImageId (\ s a -> s{_diImageId = a});

instance AWSRequest DeregisterImage where
        type Rs DeregisterImage = DeregisterImageResponse
        request = postQuery eC2
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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterImageResponse' with the minimum fields required to make a request.
--
deregisterImageResponse
    :: DeregisterImageResponse
deregisterImageResponse = DeregisterImageResponse'
