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
-- Module      : Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.Route53AutoNaming.UpdateInstanceCustomHealthStatus
    (
    -- * Creating a Request
      updateInstanceCustomHealthStatus
    , UpdateInstanceCustomHealthStatus
    -- * Request Lenses
    , uichsServiceId
    , uichsInstanceId
    , uichsStatus

    -- * Destructuring the Response
    , updateInstanceCustomHealthStatusResponse
    , UpdateInstanceCustomHealthStatusResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'updateInstanceCustomHealthStatus' smart constructor.
data UpdateInstanceCustomHealthStatus = UpdateInstanceCustomHealthStatus'
  { _uichsServiceId  :: !Text
  , _uichsInstanceId :: !Text
  , _uichsStatus     :: !CustomHealthStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstanceCustomHealthStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uichsServiceId' - Undocumented member.
--
-- * 'uichsInstanceId' - Undocumented member.
--
-- * 'uichsStatus' - Undocumented member.
updateInstanceCustomHealthStatus
    :: Text -- ^ 'uichsServiceId'
    -> Text -- ^ 'uichsInstanceId'
    -> CustomHealthStatus -- ^ 'uichsStatus'
    -> UpdateInstanceCustomHealthStatus
updateInstanceCustomHealthStatus pServiceId_ pInstanceId_ pStatus_ =
  UpdateInstanceCustomHealthStatus'
    { _uichsServiceId = pServiceId_
    , _uichsInstanceId = pInstanceId_
    , _uichsStatus = pStatus_
    }


-- | Undocumented member.
uichsServiceId :: Lens' UpdateInstanceCustomHealthStatus Text
uichsServiceId = lens _uichsServiceId (\ s a -> s{_uichsServiceId = a})

-- | Undocumented member.
uichsInstanceId :: Lens' UpdateInstanceCustomHealthStatus Text
uichsInstanceId = lens _uichsInstanceId (\ s a -> s{_uichsInstanceId = a})

-- | Undocumented member.
uichsStatus :: Lens' UpdateInstanceCustomHealthStatus CustomHealthStatus
uichsStatus = lens _uichsStatus (\ s a -> s{_uichsStatus = a})

instance AWSRequest UpdateInstanceCustomHealthStatus
         where
        type Rs UpdateInstanceCustomHealthStatus =
             UpdateInstanceCustomHealthStatusResponse
        request = postJSON route53AutoNaming
        response
          = receiveNull
              UpdateInstanceCustomHealthStatusResponse'

instance Hashable UpdateInstanceCustomHealthStatus
         where

instance NFData UpdateInstanceCustomHealthStatus
         where

instance ToHeaders UpdateInstanceCustomHealthStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.UpdateInstanceCustomHealthStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateInstanceCustomHealthStatus
         where
        toJSON UpdateInstanceCustomHealthStatus'{..}
          = object
              (catMaybes
                 [Just ("ServiceId" .= _uichsServiceId),
                  Just ("InstanceId" .= _uichsInstanceId),
                  Just ("Status" .= _uichsStatus)])

instance ToPath UpdateInstanceCustomHealthStatus
         where
        toPath = const "/"

instance ToQuery UpdateInstanceCustomHealthStatus
         where
        toQuery = const mempty

-- | /See:/ 'updateInstanceCustomHealthStatusResponse' smart constructor.
data UpdateInstanceCustomHealthStatusResponse =
  UpdateInstanceCustomHealthStatusResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstanceCustomHealthStatusResponse' with the minimum fields required to make a request.
--
updateInstanceCustomHealthStatusResponse
    :: UpdateInstanceCustomHealthStatusResponse
updateInstanceCustomHealthStatusResponse =
  UpdateInstanceCustomHealthStatusResponse'


instance NFData
           UpdateInstanceCustomHealthStatusResponse
         where
