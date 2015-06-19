{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DeleteLayer
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

-- | Deletes a specified layer. You must first stop and then delete all
-- associated instances or unassign registered instances. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html How to Delete a Layer>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteLayer.html>
module Network.AWS.OpsWorks.DeleteLayer
    (
    -- * Request
      DeleteLayer
    -- ** Request constructor
    , deleteLayer
    -- ** Request lenses
    , dlLayerId

    -- * Response
    , DeleteLayerResponse
    -- ** Response constructor
    , deleteLayerResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLayer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlLayerId'
newtype DeleteLayer = DeleteLayer'{_dlLayerId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteLayer' smart constructor.
deleteLayer :: Text -> DeleteLayer
deleteLayer pLayerId = DeleteLayer'{_dlLayerId = pLayerId};

-- | The layer ID.
dlLayerId :: Lens' DeleteLayer Text
dlLayerId = lens _dlLayerId (\ s a -> s{_dlLayerId = a});

instance AWSRequest DeleteLayer where
        type Sv DeleteLayer = OpsWorks
        type Rs DeleteLayer = DeleteLayerResponse
        request = postJSON
        response = receiveNull DeleteLayerResponse'

instance ToHeaders DeleteLayer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DeleteLayer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLayer where
        toJSON DeleteLayer'{..}
          = object ["LayerId" .= _dlLayerId]

instance ToPath DeleteLayer where
        toPath = const "/"

instance ToQuery DeleteLayer where
        toQuery = const mempty

-- | /See:/ 'deleteLayerResponse' smart constructor.
data DeleteLayerResponse = DeleteLayerResponse' deriving (Eq, Read, Show)

-- | 'DeleteLayerResponse' smart constructor.
deleteLayerResponse :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse';
