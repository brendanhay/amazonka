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
    , evioDryRun
    , evioVolumeId

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
-- * 'evioDryRun'
--
-- * 'evioVolumeId'
data EnableVolumeIO = EnableVolumeIO'
    { _evioDryRun   :: !(Maybe Bool)
    , _evioVolumeId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVolumeIO' smart constructor.
enableVolumeIO :: Text -> EnableVolumeIO
enableVolumeIO pVolumeId_ =
    EnableVolumeIO'
    { _evioDryRun = Nothing
    , _evioVolumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
evioDryRun :: Lens' EnableVolumeIO (Maybe Bool)
evioDryRun = lens _evioDryRun (\ s a -> s{_evioDryRun = a});

-- | The ID of the volume.
evioVolumeId :: Lens' EnableVolumeIO Text
evioVolumeId = lens _evioVolumeId (\ s a -> s{_evioVolumeId = a});

instance AWSRequest EnableVolumeIO where
        type Sv EnableVolumeIO = EC2
        type Rs EnableVolumeIO = EnableVolumeIOResponse
        request = post
        response = receiveNull EnableVolumeIOResponse'

instance ToHeaders EnableVolumeIO where
        toHeaders = const mempty

instance ToPath EnableVolumeIO where
        toPath = const mempty

instance ToQuery EnableVolumeIO where
        toQuery EnableVolumeIO'{..}
          = mconcat
              ["Action" =: ("EnableVolumeIO" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _evioDryRun, "VolumeId" =: _evioVolumeId]

-- | /See:/ 'enableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse =
    EnableVolumeIOResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableVolumeIOResponse' smart constructor.
enableVolumeIOResponse :: EnableVolumeIOResponse
enableVolumeIOResponse = EnableVolumeIOResponse'
