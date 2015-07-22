{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@
-- state (not attached to an instance).
--
-- The volume may remain in the @deleting@ state for several minutes.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS Volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>
module Network.AWS.EC2.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , deleeDryRun
    , deleeVolumeId

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deleeDryRun'
--
-- * 'deleeVolumeId'
data DeleteVolume = DeleteVolume'
    { _deleeDryRun   :: !(Maybe Bool)
    , _deleeVolumeId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVolume' smart constructor.
deleteVolume :: Text -> DeleteVolume
deleteVolume pVolumeId =
    DeleteVolume'
    { _deleeDryRun = Nothing
    , _deleeVolumeId = pVolumeId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleeDryRun :: Lens' DeleteVolume (Maybe Bool)
deleeDryRun = lens _deleeDryRun (\ s a -> s{_deleeDryRun = a});

-- | The ID of the volume.
deleeVolumeId :: Lens' DeleteVolume Text
deleeVolumeId = lens _deleeVolumeId (\ s a -> s{_deleeVolumeId = a});

instance AWSRequest DeleteVolume where
        type Sv DeleteVolume = EC2
        type Rs DeleteVolume = DeleteVolumeResponse
        request = post
        response = receiveNull DeleteVolumeResponse'

instance ToHeaders DeleteVolume where
        toHeaders = const mempty

instance ToPath DeleteVolume where
        toPath = const "/"

instance ToQuery DeleteVolume where
        toQuery DeleteVolume'{..}
          = mconcat
              ["Action" =: ("DeleteVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _deleeDryRun,
               "VolumeId" =: _deleeVolumeId]

-- | /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse =
    DeleteVolumeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVolumeResponse' smart constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse'
