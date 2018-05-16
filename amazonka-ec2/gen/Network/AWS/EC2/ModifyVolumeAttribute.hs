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
-- Module      : Network.AWS.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a volume attribute.
--
--
-- By default, all I/O operations for the volume are suspended when the data on the volume is determined to be potentially inconsistent, to prevent undetectable, latent data corruption. The I/O access to the volume can be resumed by first enabling I/O access and then checking the data consistency on your volume.
--
-- You can change the default behavior to resume I/O operations. We recommend that you change this only for boot volumes or for volumes that are stateless or disposable.
--
module Network.AWS.EC2.ModifyVolumeAttribute
    (
    -- * Creating a Request
      modifyVolumeAttribute
    , ModifyVolumeAttribute
    -- * Request Lenses
    , mvaAutoEnableIO
    , mvaDryRun
    , mvaVolumeId

    -- * Destructuring the Response
    , modifyVolumeAttributeResponse
    , ModifyVolumeAttributeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyVolumeAttribute.
--
--
--
-- /See:/ 'modifyVolumeAttribute' smart constructor.
data ModifyVolumeAttribute = ModifyVolumeAttribute'
  { _mvaAutoEnableIO :: !(Maybe AttributeBooleanValue)
  , _mvaDryRun       :: !(Maybe Bool)
  , _mvaVolumeId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolumeAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvaAutoEnableIO' - Indicates whether the volume should be auto-enabled for I/O operations.
--
-- * 'mvaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvaVolumeId' - The ID of the volume.
modifyVolumeAttribute
    :: Text -- ^ 'mvaVolumeId'
    -> ModifyVolumeAttribute
modifyVolumeAttribute pVolumeId_ =
  ModifyVolumeAttribute'
    { _mvaAutoEnableIO = Nothing
    , _mvaDryRun = Nothing
    , _mvaVolumeId = pVolumeId_
    }


-- | Indicates whether the volume should be auto-enabled for I/O operations.
mvaAutoEnableIO :: Lens' ModifyVolumeAttribute (Maybe AttributeBooleanValue)
mvaAutoEnableIO = lens _mvaAutoEnableIO (\ s a -> s{_mvaAutoEnableIO = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvaDryRun :: Lens' ModifyVolumeAttribute (Maybe Bool)
mvaDryRun = lens _mvaDryRun (\ s a -> s{_mvaDryRun = a})

-- | The ID of the volume.
mvaVolumeId :: Lens' ModifyVolumeAttribute Text
mvaVolumeId = lens _mvaVolumeId (\ s a -> s{_mvaVolumeId = a})

instance AWSRequest ModifyVolumeAttribute where
        type Rs ModifyVolumeAttribute =
             ModifyVolumeAttributeResponse
        request = postQuery ec2
        response = receiveNull ModifyVolumeAttributeResponse'

instance Hashable ModifyVolumeAttribute where

instance NFData ModifyVolumeAttribute where

instance ToHeaders ModifyVolumeAttribute where
        toHeaders = const mempty

instance ToPath ModifyVolumeAttribute where
        toPath = const "/"

instance ToQuery ModifyVolumeAttribute where
        toQuery ModifyVolumeAttribute'{..}
          = mconcat
              ["Action" =: ("ModifyVolumeAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AutoEnableIO" =: _mvaAutoEnableIO,
               "DryRun" =: _mvaDryRun, "VolumeId" =: _mvaVolumeId]

-- | /See:/ 'modifyVolumeAttributeResponse' smart constructor.
data ModifyVolumeAttributeResponse =
  ModifyVolumeAttributeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolumeAttributeResponse' with the minimum fields required to make a request.
--
modifyVolumeAttributeResponse
    :: ModifyVolumeAttributeResponse
modifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'


instance NFData ModifyVolumeAttributeResponse where
