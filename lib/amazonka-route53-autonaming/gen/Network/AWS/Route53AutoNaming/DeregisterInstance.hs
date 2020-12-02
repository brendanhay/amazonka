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
-- Module      : Network.AWS.Route53AutoNaming.DeregisterInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the records and the health check, if any, that Amazon Route 53 created for the specified instance.
--
--
module Network.AWS.Route53AutoNaming.DeregisterInstance
    (
    -- * Creating a Request
      deregisterInstance
    , DeregisterInstance
    -- * Request Lenses
    , diServiceId
    , diInstanceId

    -- * Destructuring the Response
    , deregisterInstanceResponse
    , DeregisterInstanceResponse
    -- * Response Lenses
    , dirsOperationId
    , dirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'deregisterInstance' smart constructor.
data DeregisterInstance = DeregisterInstance'
  { _diServiceId  :: !Text
  , _diInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diServiceId' - The ID of the service that the instance is associated with.
--
-- * 'diInstanceId' - The value that you specified for @Id@ in the 'RegisterInstance' request.
deregisterInstance
    :: Text -- ^ 'diServiceId'
    -> Text -- ^ 'diInstanceId'
    -> DeregisterInstance
deregisterInstance pServiceId_ pInstanceId_ =
  DeregisterInstance' {_diServiceId = pServiceId_, _diInstanceId = pInstanceId_}


-- | The ID of the service that the instance is associated with.
diServiceId :: Lens' DeregisterInstance Text
diServiceId = lens _diServiceId (\ s a -> s{_diServiceId = a})

-- | The value that you specified for @Id@ in the 'RegisterInstance' request.
diInstanceId :: Lens' DeregisterInstance Text
diInstanceId = lens _diInstanceId (\ s a -> s{_diInstanceId = a})

instance AWSRequest DeregisterInstance where
        type Rs DeregisterInstance =
             DeregisterInstanceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterInstanceResponse' <$>
                   (x .?> "OperationId") <*> (pure (fromEnum s)))

instance Hashable DeregisterInstance where

instance NFData DeregisterInstance where

instance ToHeaders DeregisterInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.DeregisterInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterInstance where
        toJSON DeregisterInstance'{..}
          = object
              (catMaybes
                 [Just ("ServiceId" .= _diServiceId),
                  Just ("InstanceId" .= _diInstanceId)])

instance ToPath DeregisterInstance where
        toPath = const "/"

instance ToQuery DeregisterInstance where
        toQuery = const mempty

-- | /See:/ 'deregisterInstanceResponse' smart constructor.
data DeregisterInstanceResponse = DeregisterInstanceResponse'
  { _dirsOperationId    :: !(Maybe Text)
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsOperationId' - A value that you can use to determine whether the request completed successfully. For more information, see 'GetOperation' .
--
-- * 'dirsResponseStatus' - -- | The response status code.
deregisterInstanceResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeregisterInstanceResponse
deregisterInstanceResponse pResponseStatus_ =
  DeregisterInstanceResponse'
    {_dirsOperationId = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | A value that you can use to determine whether the request completed successfully. For more information, see 'GetOperation' .
dirsOperationId :: Lens' DeregisterInstanceResponse (Maybe Text)
dirsOperationId = lens _dirsOperationId (\ s a -> s{_dirsOperationId = a})

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeregisterInstanceResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeregisterInstanceResponse where
