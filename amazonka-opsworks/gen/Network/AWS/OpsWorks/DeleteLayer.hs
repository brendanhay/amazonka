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
-- Module      : Network.AWS.OpsWorks.DeleteLayer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteLayer.html AWS API Reference> for DeleteLayer.
module Network.AWS.OpsWorks.DeleteLayer
    (
    -- * Creating a Request
      deleteLayer
    , DeleteLayer
    -- * Request Lenses
    , dlLayerId

    -- * Destructuring the Response
    , deleteLayerResponse
    , DeleteLayerResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLayer' smart constructor.
newtype DeleteLayer = DeleteLayer'
    { _dlLayerId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLayer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLayerId'
deleteLayer
    :: Text -- ^ 'dlLayerId'
    -> DeleteLayer
deleteLayer pLayerId_ =
    DeleteLayer'
    { _dlLayerId = pLayerId_
    }

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
data DeleteLayerResponse =
    DeleteLayerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteLayerResponse' with the minimum fields required to make a request.
--
deleteLayerResponse
    :: DeleteLayerResponse
deleteLayerResponse = DeleteLayerResponse'
