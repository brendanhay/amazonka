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
-- Module      : Network.AWS.ELBv2.DeregisterTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.
--
--
module Network.AWS.ELBv2.DeregisterTargets
    (
    -- * Creating a Request
      deregisterTargets
    , DeregisterTargets
    -- * Request Lenses
    , dtTargetGroupARN
    , dtTargets

    -- * Destructuring the Response
    , deregisterTargetsResponse
    , DeregisterTargetsResponse
    -- * Response Lenses
    , dtsrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { _dtTargetGroupARN :: !Text
  , _dtTargets        :: ![TargetDescription]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
--
-- * 'dtTargets' - The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
deregisterTargets
    :: Text -- ^ 'dtTargetGroupARN'
    -> DeregisterTargets
deregisterTargets pTargetGroupARN_ =
  DeregisterTargets' {_dtTargetGroupARN = pTargetGroupARN_, _dtTargets = mempty}


-- | The Amazon Resource Name (ARN) of the target group.
dtTargetGroupARN :: Lens' DeregisterTargets Text
dtTargetGroupARN = lens _dtTargetGroupARN (\ s a -> s{_dtTargetGroupARN = a})

-- | The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
dtTargets :: Lens' DeregisterTargets [TargetDescription]
dtTargets = lens _dtTargets (\ s a -> s{_dtTargets = a}) . _Coerce

instance AWSRequest DeregisterTargets where
        type Rs DeregisterTargets = DeregisterTargetsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DeregisterTargetsResult"
              (\ s h x ->
                 DeregisterTargetsResponse' <$> (pure (fromEnum s)))

instance Hashable DeregisterTargets where

instance NFData DeregisterTargets where

instance ToHeaders DeregisterTargets where
        toHeaders = const mempty

instance ToPath DeregisterTargets where
        toPath = const "/"

instance ToQuery DeregisterTargets where
        toQuery DeregisterTargets'{..}
          = mconcat
              ["Action" =: ("DeregisterTargets" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "TargetGroupArn" =: _dtTargetGroupARN,
               "Targets" =: toQueryList "member" _dtTargets]

-- | /See:/ 'deregisterTargetsResponse' smart constructor.
newtype DeregisterTargetsResponse = DeregisterTargetsResponse'
  { _dtsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsrsResponseStatus' - -- | The response status code.
deregisterTargetsResponse
    :: Int -- ^ 'dtsrsResponseStatus'
    -> DeregisterTargetsResponse
deregisterTargetsResponse pResponseStatus_ =
  DeregisterTargetsResponse' {_dtsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtsrsResponseStatus :: Lens' DeregisterTargetsResponse Int
dtsrsResponseStatus = lens _dtsrsResponseStatus (\ s a -> s{_dtsrsResponseStatus = a})

instance NFData DeregisterTargetsResponse where
