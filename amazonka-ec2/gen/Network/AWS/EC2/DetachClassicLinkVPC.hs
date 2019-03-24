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
-- Module      : Network.AWS.EC2.DetachClassicLinkVPC
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the instance has been unlinked, the VPC security groups are no longer associated with it. An instance is automatically unlinked from a VPC when it's stopped.
--
--
module Network.AWS.EC2.DetachClassicLinkVPC
    (
    -- * Creating a Request
      detachClassicLinkVPC
    , DetachClassicLinkVPC
    -- * Request Lenses
    , dclvDryRun
    , dclvInstanceId
    , dclvVPCId

    -- * Destructuring the Response
    , detachClassicLinkVPCResponse
    , DetachClassicLinkVPCResponse
    -- * Response Lenses
    , dclvrsReturn
    , dclvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachClassicLinkVPC' smart constructor.
data DetachClassicLinkVPC = DetachClassicLinkVPC'
  { _dclvDryRun     :: !(Maybe Bool)
  , _dclvInstanceId :: !Text
  , _dclvVPCId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachClassicLinkVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dclvInstanceId' - The ID of the instance to unlink from the VPC.
--
-- * 'dclvVPCId' - The ID of the VPC to which the instance is linked.
detachClassicLinkVPC
    :: Text -- ^ 'dclvInstanceId'
    -> Text -- ^ 'dclvVPCId'
    -> DetachClassicLinkVPC
detachClassicLinkVPC pInstanceId_ pVPCId_ =
  DetachClassicLinkVPC'
    { _dclvDryRun = Nothing
    , _dclvInstanceId = pInstanceId_
    , _dclvVPCId = pVPCId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dclvDryRun :: Lens' DetachClassicLinkVPC (Maybe Bool)
dclvDryRun = lens _dclvDryRun (\ s a -> s{_dclvDryRun = a})

-- | The ID of the instance to unlink from the VPC.
dclvInstanceId :: Lens' DetachClassicLinkVPC Text
dclvInstanceId = lens _dclvInstanceId (\ s a -> s{_dclvInstanceId = a})

-- | The ID of the VPC to which the instance is linked.
dclvVPCId :: Lens' DetachClassicLinkVPC Text
dclvVPCId = lens _dclvVPCId (\ s a -> s{_dclvVPCId = a})

instance AWSRequest DetachClassicLinkVPC where
        type Rs DetachClassicLinkVPC =
             DetachClassicLinkVPCResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DetachClassicLinkVPCResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DetachClassicLinkVPC where

instance NFData DetachClassicLinkVPC where

instance ToHeaders DetachClassicLinkVPC where
        toHeaders = const mempty

instance ToPath DetachClassicLinkVPC where
        toPath = const "/"

instance ToQuery DetachClassicLinkVPC where
        toQuery DetachClassicLinkVPC'{..}
          = mconcat
              ["Action" =: ("DetachClassicLinkVpc" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dclvDryRun,
               "InstanceId" =: _dclvInstanceId,
               "VpcId" =: _dclvVPCId]

-- | /See:/ 'detachClassicLinkVPCResponse' smart constructor.
data DetachClassicLinkVPCResponse = DetachClassicLinkVPCResponse'
  { _dclvrsReturn         :: !(Maybe Bool)
  , _dclvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachClassicLinkVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclvrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'dclvrsResponseStatus' - -- | The response status code.
detachClassicLinkVPCResponse
    :: Int -- ^ 'dclvrsResponseStatus'
    -> DetachClassicLinkVPCResponse
detachClassicLinkVPCResponse pResponseStatus_ =
  DetachClassicLinkVPCResponse'
    {_dclvrsReturn = Nothing, _dclvrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
dclvrsReturn :: Lens' DetachClassicLinkVPCResponse (Maybe Bool)
dclvrsReturn = lens _dclvrsReturn (\ s a -> s{_dclvrsReturn = a})

-- | -- | The response status code.
dclvrsResponseStatus :: Lens' DetachClassicLinkVPCResponse Int
dclvrsResponseStatus = lens _dclvrsResponseStatus (\ s a -> s{_dclvrsResponseStatus = a})

instance NFData DetachClassicLinkVPCResponse where
