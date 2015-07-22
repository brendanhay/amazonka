{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables I\/O operations for a volume that had I\/O operations disabled
-- because the data on the volume was potentially inconsistent.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-EnableVolumeIO.html>
module Network.AWS.EC2.EnableVolumeIO
    (
    -- * Request
      EnableVolumeIO
    -- ** Request constructor
    , enableVolumeIO
    -- ** Request lenses
    , eviorqDryRun
    , eviorqVolumeId

    -- * Response
    , EnableVolumeIOResponse
    -- ** Response constructor
    , enableVolumeIOResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableVolumeIO' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eviorqDryRun'
--
-- * 'eviorqVolumeId'
data EnableVolumeIO = EnableVolumeIO'
    { _eviorqDryRun   :: !(Maybe Bool)
    , _eviorqVolumeId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVolumeIO' smart constructor.
enableVolumeIO :: Text -> EnableVolumeIO
enableVolumeIO pVolumeId_ =
    EnableVolumeIO'
    { _eviorqDryRun = Nothing
    , _eviorqVolumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
eviorqDryRun :: Lens' EnableVolumeIO (Maybe Bool)
eviorqDryRun = lens _eviorqDryRun (\ s a -> s{_eviorqDryRun = a});

-- | The ID of the volume.
eviorqVolumeId :: Lens' EnableVolumeIO Text
eviorqVolumeId = lens _eviorqVolumeId (\ s a -> s{_eviorqVolumeId = a});

instance AWSRequest EnableVolumeIO where
        type Sv EnableVolumeIO = EC2
        type Rs EnableVolumeIO = EnableVolumeIOResponse
        request = post
        response = receiveNull EnableVolumeIOResponse'

instance ToHeaders EnableVolumeIO where
        toHeaders = const mempty

instance ToPath EnableVolumeIO where
        toPath = const "/"

instance ToQuery EnableVolumeIO where
        toQuery EnableVolumeIO'{..}
          = mconcat
              ["Action" =: ("EnableVolumeIO" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _eviorqDryRun,
               "VolumeId" =: _eviorqVolumeId]

-- | /See:/ 'enableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse =
    EnableVolumeIOResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVolumeIOResponse' smart constructor.
enableVolumeIOResponse :: EnableVolumeIOResponse
enableVolumeIOResponse = EnableVolumeIOResponse'
