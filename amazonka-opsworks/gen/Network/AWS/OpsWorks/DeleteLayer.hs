{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified layer. You must first stop and then delete all
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
    , dlrqLayerId

    -- * Response
    , DeleteLayerResponse
    -- ** Response constructor
    , deleteLayerResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLayer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlrqLayerId'
newtype DeleteLayer = DeleteLayer'
    { _dlrqLayerId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLayer' smart constructor.
deleteLayer :: Text -> DeleteLayer
deleteLayer pLayerId =
    DeleteLayer'
    { _dlrqLayerId = pLayerId
    }

-- | The layer ID.
dlrqLayerId :: Lens' DeleteLayer Text
dlrqLayerId = lens _dlrqLayerId (\ s a -> s{_dlrqLayerId = a});

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
          = object ["LayerId" .= _dlrqLayerId]

instance ToPath DeleteLayer where
        toPath = const "/"

instance ToQuery DeleteLayer where
        toQuery = const mempty

-- | /See:/ 'deleteLayerResponse' smart constructor.
data DeleteLayerResponse =
    DeleteLayerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLayerResponse' smart constructor.
deleteLayerResponse :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse'
