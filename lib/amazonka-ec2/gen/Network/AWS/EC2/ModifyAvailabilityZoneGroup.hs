{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyAvailabilityZoneGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the opt-in status of the Local Zone and Wavelength Zone group for your account.
--
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the value for @GroupName@ .
module Network.AWS.EC2.ModifyAvailabilityZoneGroup
  ( -- * Creating a Request
    modifyAvailabilityZoneGroup,
    ModifyAvailabilityZoneGroup,

    -- * Request Lenses
    mazgDryRun,
    mazgGroupName,
    mazgOptInStatus,

    -- * Destructuring the Response
    modifyAvailabilityZoneGroupResponse,
    ModifyAvailabilityZoneGroupResponse,

    -- * Response Lenses
    mazgrsReturn,
    mazgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyAvailabilityZoneGroup' smart constructor.
data ModifyAvailabilityZoneGroup = ModifyAvailabilityZoneGroup'
  { _mazgDryRun ::
      !(Maybe Bool),
    _mazgGroupName :: !Text,
    _mazgOptInStatus ::
      !ModifyAvailabilityZoneOptInStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyAvailabilityZoneGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mazgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mazgGroupName' - The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
--
-- * 'mazgOptInStatus' - Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
modifyAvailabilityZoneGroup ::
  -- | 'mazgGroupName'
  Text ->
  -- | 'mazgOptInStatus'
  ModifyAvailabilityZoneOptInStatus ->
  ModifyAvailabilityZoneGroup
modifyAvailabilityZoneGroup pGroupName_ pOptInStatus_ =
  ModifyAvailabilityZoneGroup'
    { _mazgDryRun = Nothing,
      _mazgGroupName = pGroupName_,
      _mazgOptInStatus = pOptInStatus_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mazgDryRun :: Lens' ModifyAvailabilityZoneGroup (Maybe Bool)
mazgDryRun = lens _mazgDryRun (\s a -> s {_mazgDryRun = a})

-- | The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
mazgGroupName :: Lens' ModifyAvailabilityZoneGroup Text
mazgGroupName = lens _mazgGroupName (\s a -> s {_mazgGroupName = a})

-- | Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
mazgOptInStatus :: Lens' ModifyAvailabilityZoneGroup ModifyAvailabilityZoneOptInStatus
mazgOptInStatus = lens _mazgOptInStatus (\s a -> s {_mazgOptInStatus = a})

instance AWSRequest ModifyAvailabilityZoneGroup where
  type
    Rs ModifyAvailabilityZoneGroup =
      ModifyAvailabilityZoneGroupResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyAvailabilityZoneGroupResponse'
            <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable ModifyAvailabilityZoneGroup

instance NFData ModifyAvailabilityZoneGroup

instance ToHeaders ModifyAvailabilityZoneGroup where
  toHeaders = const mempty

instance ToPath ModifyAvailabilityZoneGroup where
  toPath = const "/"

instance ToQuery ModifyAvailabilityZoneGroup where
  toQuery ModifyAvailabilityZoneGroup' {..} =
    mconcat
      [ "Action" =: ("ModifyAvailabilityZoneGroup" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _mazgDryRun,
        "GroupName" =: _mazgGroupName,
        "OptInStatus" =: _mazgOptInStatus
      ]

-- | /See:/ 'modifyAvailabilityZoneGroupResponse' smart constructor.
data ModifyAvailabilityZoneGroupResponse = ModifyAvailabilityZoneGroupResponse'
  { _mazgrsReturn ::
      !(Maybe Bool),
    _mazgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyAvailabilityZoneGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mazgrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'mazgrsResponseStatus' - -- | The response status code.
modifyAvailabilityZoneGroupResponse ::
  -- | 'mazgrsResponseStatus'
  Int ->
  ModifyAvailabilityZoneGroupResponse
modifyAvailabilityZoneGroupResponse pResponseStatus_ =
  ModifyAvailabilityZoneGroupResponse'
    { _mazgrsReturn = Nothing,
      _mazgrsResponseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
mazgrsReturn :: Lens' ModifyAvailabilityZoneGroupResponse (Maybe Bool)
mazgrsReturn = lens _mazgrsReturn (\s a -> s {_mazgrsReturn = a})

-- | -- | The response status code.
mazgrsResponseStatus :: Lens' ModifyAvailabilityZoneGroupResponse Int
mazgrsResponseStatus = lens _mazgrsResponseStatus (\s a -> s {_mazgrsResponseStatus = a})

instance NFData ModifyAvailabilityZoneGroupResponse
