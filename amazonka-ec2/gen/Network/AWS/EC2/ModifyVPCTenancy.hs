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
-- Module      : Network.AWS.EC2.ModifyVPCTenancy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the instance tenancy attribute of the specified VPC. You can change the instance tenancy attribute of a VPC to @default@ only. You cannot change the instance tenancy attribute to @dedicated@ .
--
--
-- After you modify the tenancy of the VPC, any new instances that you launch into the VPC have a tenancy of @default@ , unless you specify otherwise during launch. The tenancy of any existing instances in the VPC is not affected.
--
-- For more information about Dedicated Instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.ModifyVPCTenancy
    (
    -- * Creating a Request
      modifyVPCTenancy
    , ModifyVPCTenancy
    -- * Request Lenses
    , mvtDryRun
    , mvtVPCId
    , mvtInstanceTenancy

    -- * Destructuring the Response
    , modifyVPCTenancyResponse
    , ModifyVPCTenancyResponse
    -- * Response Lenses
    , mvtrsReturnValue
    , mvtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ModifyVpcTenancy.
--
--
--
-- /See:/ 'modifyVPCTenancy' smart constructor.
data ModifyVPCTenancy = ModifyVPCTenancy'
  { _mvtDryRun          :: !(Maybe Bool)
  , _mvtVPCId           :: !Text
  , _mvtInstanceTenancy :: !VPCTenancy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCTenancy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtDryRun' - Checks whether you have the required permissions for the operation, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvtVPCId' - The ID of the VPC.
--
-- * 'mvtInstanceTenancy' - The instance tenancy attribute for the VPC.
modifyVPCTenancy
    :: Text -- ^ 'mvtVPCId'
    -> VPCTenancy -- ^ 'mvtInstanceTenancy'
    -> ModifyVPCTenancy
modifyVPCTenancy pVPCId_ pInstanceTenancy_ =
  ModifyVPCTenancy'
    { _mvtDryRun = Nothing
    , _mvtVPCId = pVPCId_
    , _mvtInstanceTenancy = pInstanceTenancy_
    }


-- | Checks whether you have the required permissions for the operation, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvtDryRun :: Lens' ModifyVPCTenancy (Maybe Bool)
mvtDryRun = lens _mvtDryRun (\ s a -> s{_mvtDryRun = a})

-- | The ID of the VPC.
mvtVPCId :: Lens' ModifyVPCTenancy Text
mvtVPCId = lens _mvtVPCId (\ s a -> s{_mvtVPCId = a})

-- | The instance tenancy attribute for the VPC.
mvtInstanceTenancy :: Lens' ModifyVPCTenancy VPCTenancy
mvtInstanceTenancy = lens _mvtInstanceTenancy (\ s a -> s{_mvtInstanceTenancy = a})

instance AWSRequest ModifyVPCTenancy where
        type Rs ModifyVPCTenancy = ModifyVPCTenancyResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCTenancyResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyVPCTenancy where

instance NFData ModifyVPCTenancy where

instance ToHeaders ModifyVPCTenancy where
        toHeaders = const mempty

instance ToPath ModifyVPCTenancy where
        toPath = const "/"

instance ToQuery ModifyVPCTenancy where
        toQuery ModifyVPCTenancy'{..}
          = mconcat
              ["Action" =: ("ModifyVpcTenancy" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _mvtDryRun, "VpcId" =: _mvtVPCId,
               "InstanceTenancy" =: _mvtInstanceTenancy]

-- | Contains the output of ModifyVpcTenancy.
--
--
--
-- /See:/ 'modifyVPCTenancyResponse' smart constructor.
data ModifyVPCTenancyResponse = ModifyVPCTenancyResponse'
  { _mvtrsReturnValue    :: !(Maybe Bool)
  , _mvtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCTenancyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvtrsReturnValue' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- * 'mvtrsResponseStatus' - -- | The response status code.
modifyVPCTenancyResponse
    :: Int -- ^ 'mvtrsResponseStatus'
    -> ModifyVPCTenancyResponse
modifyVPCTenancyResponse pResponseStatus_ =
  ModifyVPCTenancyResponse'
    {_mvtrsReturnValue = Nothing, _mvtrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, returns an error.
mvtrsReturnValue :: Lens' ModifyVPCTenancyResponse (Maybe Bool)
mvtrsReturnValue = lens _mvtrsReturnValue (\ s a -> s{_mvtrsReturnValue = a})

-- | -- | The response status code.
mvtrsResponseStatus :: Lens' ModifyVPCTenancyResponse Int
mvtrsResponseStatus = lens _mvtrsResponseStatus (\ s a -> s{_mvtrsResponseStatus = a})

instance NFData ModifyVPCTenancyResponse where
