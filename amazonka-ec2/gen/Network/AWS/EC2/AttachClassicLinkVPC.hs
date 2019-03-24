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
-- Module      : Network.AWS.EC2.AttachClassicLinkVPC
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an EC2-Classic instance to a ClassicLink-enabled VPC through one or more of the VPC's security groups. You cannot link an EC2-Classic instance to more than one VPC at a time. You can only link an instance that's in the @running@ state. An instance is automatically unlinked from a VPC when it's stopped - you can link it to the VPC again when you restart it.
--
--
-- After you've linked an instance, you cannot change the VPC security groups that are associated with it. To change the security groups, you must first unlink the instance, and then link it again.
--
-- Linking your instance to a VPC is sometimes referred to as /attaching/ your instance.
--
module Network.AWS.EC2.AttachClassicLinkVPC
    (
    -- * Creating a Request
      attachClassicLinkVPC
    , AttachClassicLinkVPC
    -- * Request Lenses
    , aclvDryRun
    , aclvGroups
    , aclvInstanceId
    , aclvVPCId

    -- * Destructuring the Response
    , attachClassicLinkVPCResponse
    , AttachClassicLinkVPCResponse
    -- * Response Lenses
    , aclvrsReturn
    , aclvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachClassicLinkVPC' smart constructor.
data AttachClassicLinkVPC = AttachClassicLinkVPC'
  { _aclvDryRun     :: !(Maybe Bool)
  , _aclvGroups     :: ![Text]
  , _aclvInstanceId :: !Text
  , _aclvVPCId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachClassicLinkVPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aclvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'aclvGroups' - The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
--
-- * 'aclvInstanceId' - The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
--
-- * 'aclvVPCId' - The ID of a ClassicLink-enabled VPC.
attachClassicLinkVPC
    :: Text -- ^ 'aclvInstanceId'
    -> Text -- ^ 'aclvVPCId'
    -> AttachClassicLinkVPC
attachClassicLinkVPC pInstanceId_ pVPCId_ =
  AttachClassicLinkVPC'
    { _aclvDryRun = Nothing
    , _aclvGroups = mempty
    , _aclvInstanceId = pInstanceId_
    , _aclvVPCId = pVPCId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
aclvDryRun :: Lens' AttachClassicLinkVPC (Maybe Bool)
aclvDryRun = lens _aclvDryRun (\ s a -> s{_aclvDryRun = a})

-- | The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
aclvGroups :: Lens' AttachClassicLinkVPC [Text]
aclvGroups = lens _aclvGroups (\ s a -> s{_aclvGroups = a}) . _Coerce

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
aclvInstanceId :: Lens' AttachClassicLinkVPC Text
aclvInstanceId = lens _aclvInstanceId (\ s a -> s{_aclvInstanceId = a})

-- | The ID of a ClassicLink-enabled VPC.
aclvVPCId :: Lens' AttachClassicLinkVPC Text
aclvVPCId = lens _aclvVPCId (\ s a -> s{_aclvVPCId = a})

instance AWSRequest AttachClassicLinkVPC where
        type Rs AttachClassicLinkVPC =
             AttachClassicLinkVPCResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AttachClassicLinkVPCResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable AttachClassicLinkVPC where

instance NFData AttachClassicLinkVPC where

instance ToHeaders AttachClassicLinkVPC where
        toHeaders = const mempty

instance ToPath AttachClassicLinkVPC where
        toPath = const "/"

instance ToQuery AttachClassicLinkVPC where
        toQuery AttachClassicLinkVPC'{..}
          = mconcat
              ["Action" =: ("AttachClassicLinkVpc" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _aclvDryRun,
               toQueryList "SecurityGroupId" _aclvGroups,
               "InstanceId" =: _aclvInstanceId,
               "VpcId" =: _aclvVPCId]

-- | /See:/ 'attachClassicLinkVPCResponse' smart constructor.
data AttachClassicLinkVPCResponse = AttachClassicLinkVPCResponse'
  { _aclvrsReturn         :: !(Maybe Bool)
  , _aclvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachClassicLinkVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aclvrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'aclvrsResponseStatus' - -- | The response status code.
attachClassicLinkVPCResponse
    :: Int -- ^ 'aclvrsResponseStatus'
    -> AttachClassicLinkVPCResponse
attachClassicLinkVPCResponse pResponseStatus_ =
  AttachClassicLinkVPCResponse'
    {_aclvrsReturn = Nothing, _aclvrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
aclvrsReturn :: Lens' AttachClassicLinkVPCResponse (Maybe Bool)
aclvrsReturn = lens _aclvrsReturn (\ s a -> s{_aclvrsReturn = a})

-- | -- | The response status code.
aclvrsResponseStatus :: Lens' AttachClassicLinkVPCResponse Int
aclvrsResponseStatus = lens _aclvrsResponseStatus (\ s a -> s{_aclvrsResponseStatus = a})

instance NFData AttachClassicLinkVPCResponse where
