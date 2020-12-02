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
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I/O operations for a volume that had I/O operations disabled because the data on the volume was potentially inconsistent.
--
--
module Network.AWS.EC2.EnableVolumeIO
    (
    -- * Creating a Request
      enableVolumeIO
    , EnableVolumeIO
    -- * Request Lenses
    , evioDryRun
    , evioVolumeId

    -- * Destructuring the Response
    , enableVolumeIOResponse
    , EnableVolumeIOResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for EnableVolumeIO.
--
--
--
-- /See:/ 'enableVolumeIO' smart constructor.
data EnableVolumeIO = EnableVolumeIO'
  { _evioDryRun   :: !(Maybe Bool)
  , _evioVolumeId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableVolumeIO' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evioDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'evioVolumeId' - The ID of the volume.
enableVolumeIO
    :: Text -- ^ 'evioVolumeId'
    -> EnableVolumeIO
enableVolumeIO pVolumeId_ =
  EnableVolumeIO' {_evioDryRun = Nothing, _evioVolumeId = pVolumeId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
evioDryRun :: Lens' EnableVolumeIO (Maybe Bool)
evioDryRun = lens _evioDryRun (\ s a -> s{_evioDryRun = a})

-- | The ID of the volume.
evioVolumeId :: Lens' EnableVolumeIO Text
evioVolumeId = lens _evioVolumeId (\ s a -> s{_evioVolumeId = a})

instance AWSRequest EnableVolumeIO where
        type Rs EnableVolumeIO = EnableVolumeIOResponse
        request = postQuery ec2
        response = receiveNull EnableVolumeIOResponse'

instance Hashable EnableVolumeIO where

instance NFData EnableVolumeIO where

instance ToHeaders EnableVolumeIO where
        toHeaders = const mempty

instance ToPath EnableVolumeIO where
        toPath = const "/"

instance ToQuery EnableVolumeIO where
        toQuery EnableVolumeIO'{..}
          = mconcat
              ["Action" =: ("EnableVolumeIO" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _evioDryRun, "VolumeId" =: _evioVolumeId]

-- | /See:/ 'enableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse =
  EnableVolumeIOResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableVolumeIOResponse' with the minimum fields required to make a request.
--
enableVolumeIOResponse
    :: EnableVolumeIOResponse
enableVolumeIOResponse = EnableVolumeIOResponse'


instance NFData EnableVolumeIOResponse where
