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
-- Module      : Network.AWS.EC2.ModifyDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the default credit option for CPU usage of burstable performance instances. The default credit option is set at the account level per AWS Region, and is specified per instance family. All new burstable performance instances in the account launch using the default credit option.
--
--
-- @ModifyDefaultCreditSpecification@ is an asynchronous operation, which works at an AWS Region level and modifies the credit option for each Availability Zone. All zones in a Region are updated within five minutes. But if instances are launched during this operation, they might not get the new credit option until the zone is updated. To verify whether the update has occurred, you can call @GetDefaultCreditSpecification@ and check @DefaultCreditSpecification@ for updates.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyDefaultCreditSpecification
  ( -- * Creating a Request
    modifyDefaultCreditSpecification,
    ModifyDefaultCreditSpecification,

    -- * Request Lenses
    mdcsDryRun,
    mdcsInstanceFamily,
    mdcsCPUCredits,

    -- * Destructuring the Response
    modifyDefaultCreditSpecificationResponse,
    ModifyDefaultCreditSpecificationResponse,

    -- * Response Lenses
    mdcsrsInstanceFamilyCreditSpecification,
    mdcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyDefaultCreditSpecification' smart constructor.
data ModifyDefaultCreditSpecification = ModifyDefaultCreditSpecification'
  { _mdcsDryRun ::
      !(Maybe Bool),
    _mdcsInstanceFamily ::
      !UnlimitedSupportedInstanceFamily,
    _mdcsCPUCredits :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDefaultCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mdcsInstanceFamily' - The instance family.
--
-- * 'mdcsCPUCredits' - The credit option for CPU usage of the instance family. Valid Values: @standard@ | @unlimited@
modifyDefaultCreditSpecification ::
  -- | 'mdcsInstanceFamily'
  UnlimitedSupportedInstanceFamily ->
  -- | 'mdcsCPUCredits'
  Text ->
  ModifyDefaultCreditSpecification
modifyDefaultCreditSpecification pInstanceFamily_ pCPUCredits_ =
  ModifyDefaultCreditSpecification'
    { _mdcsDryRun = Nothing,
      _mdcsInstanceFamily = pInstanceFamily_,
      _mdcsCPUCredits = pCPUCredits_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mdcsDryRun :: Lens' ModifyDefaultCreditSpecification (Maybe Bool)
mdcsDryRun = lens _mdcsDryRun (\s a -> s {_mdcsDryRun = a})

-- | The instance family.
mdcsInstanceFamily :: Lens' ModifyDefaultCreditSpecification UnlimitedSupportedInstanceFamily
mdcsInstanceFamily = lens _mdcsInstanceFamily (\s a -> s {_mdcsInstanceFamily = a})

-- | The credit option for CPU usage of the instance family. Valid Values: @standard@ | @unlimited@
mdcsCPUCredits :: Lens' ModifyDefaultCreditSpecification Text
mdcsCPUCredits = lens _mdcsCPUCredits (\s a -> s {_mdcsCPUCredits = a})

instance AWSRequest ModifyDefaultCreditSpecification where
  type
    Rs ModifyDefaultCreditSpecification =
      ModifyDefaultCreditSpecificationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyDefaultCreditSpecificationResponse'
            <$> (x .@? "instanceFamilyCreditSpecification")
            <*> (pure (fromEnum s))
      )

instance Hashable ModifyDefaultCreditSpecification

instance NFData ModifyDefaultCreditSpecification

instance ToHeaders ModifyDefaultCreditSpecification where
  toHeaders = const mempty

instance ToPath ModifyDefaultCreditSpecification where
  toPath = const "/"

instance ToQuery ModifyDefaultCreditSpecification where
  toQuery ModifyDefaultCreditSpecification' {..} =
    mconcat
      [ "Action" =: ("ModifyDefaultCreditSpecification" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _mdcsDryRun,
        "InstanceFamily" =: _mdcsInstanceFamily,
        "CpuCredits" =: _mdcsCPUCredits
      ]

-- | /See:/ 'modifyDefaultCreditSpecificationResponse' smart constructor.
data ModifyDefaultCreditSpecificationResponse = ModifyDefaultCreditSpecificationResponse'
  { _mdcsrsInstanceFamilyCreditSpecification ::
      !( Maybe
           InstanceFamilyCreditSpecification
       ),
    _mdcsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDefaultCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdcsrsInstanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
--
-- * 'mdcsrsResponseStatus' - -- | The response status code.
modifyDefaultCreditSpecificationResponse ::
  -- | 'mdcsrsResponseStatus'
  Int ->
  ModifyDefaultCreditSpecificationResponse
modifyDefaultCreditSpecificationResponse pResponseStatus_ =
  ModifyDefaultCreditSpecificationResponse'
    { _mdcsrsInstanceFamilyCreditSpecification =
        Nothing,
      _mdcsrsResponseStatus = pResponseStatus_
    }

-- | The default credit option for CPU usage of the instance family.
mdcsrsInstanceFamilyCreditSpecification :: Lens' ModifyDefaultCreditSpecificationResponse (Maybe InstanceFamilyCreditSpecification)
mdcsrsInstanceFamilyCreditSpecification = lens _mdcsrsInstanceFamilyCreditSpecification (\s a -> s {_mdcsrsInstanceFamilyCreditSpecification = a})

-- | -- | The response status code.
mdcsrsResponseStatus :: Lens' ModifyDefaultCreditSpecificationResponse Int
mdcsrsResponseStatus = lens _mdcsrsResponseStatus (\s a -> s {_mdcsrsResponseStatus = a})

instance NFData ModifyDefaultCreditSpecificationResponse
