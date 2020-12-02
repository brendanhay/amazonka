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
-- Module      : Network.AWS.EC2.GetDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default credit option for CPU usage of a burstable performance instance family.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetDefaultCreditSpecification
  ( -- * Creating a Request
    getDefaultCreditSpecification,
    GetDefaultCreditSpecification,

    -- * Request Lenses
    gdcsDryRun,
    gdcsInstanceFamily,

    -- * Destructuring the Response
    getDefaultCreditSpecificationResponse,
    GetDefaultCreditSpecificationResponse,

    -- * Response Lenses
    gdcsrsInstanceFamilyCreditSpecification,
    gdcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDefaultCreditSpecification' smart constructor.
data GetDefaultCreditSpecification = GetDefaultCreditSpecification'
  { _gdcsDryRun ::
      !(Maybe Bool),
    _gdcsInstanceFamily ::
      !UnlimitedSupportedInstanceFamily
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDefaultCreditSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gdcsInstanceFamily' - The instance family.
getDefaultCreditSpecification ::
  -- | 'gdcsInstanceFamily'
  UnlimitedSupportedInstanceFamily ->
  GetDefaultCreditSpecification
getDefaultCreditSpecification pInstanceFamily_ =
  GetDefaultCreditSpecification'
    { _gdcsDryRun = Nothing,
      _gdcsInstanceFamily = pInstanceFamily_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gdcsDryRun :: Lens' GetDefaultCreditSpecification (Maybe Bool)
gdcsDryRun = lens _gdcsDryRun (\s a -> s {_gdcsDryRun = a})

-- | The instance family.
gdcsInstanceFamily :: Lens' GetDefaultCreditSpecification UnlimitedSupportedInstanceFamily
gdcsInstanceFamily = lens _gdcsInstanceFamily (\s a -> s {_gdcsInstanceFamily = a})

instance AWSRequest GetDefaultCreditSpecification where
  type
    Rs GetDefaultCreditSpecification =
      GetDefaultCreditSpecificationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetDefaultCreditSpecificationResponse'
            <$> (x .@? "instanceFamilyCreditSpecification")
            <*> (pure (fromEnum s))
      )

instance Hashable GetDefaultCreditSpecification

instance NFData GetDefaultCreditSpecification

instance ToHeaders GetDefaultCreditSpecification where
  toHeaders = const mempty

instance ToPath GetDefaultCreditSpecification where
  toPath = const "/"

instance ToQuery GetDefaultCreditSpecification where
  toQuery GetDefaultCreditSpecification' {..} =
    mconcat
      [ "Action" =: ("GetDefaultCreditSpecification" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _gdcsDryRun,
        "InstanceFamily" =: _gdcsInstanceFamily
      ]

-- | /See:/ 'getDefaultCreditSpecificationResponse' smart constructor.
data GetDefaultCreditSpecificationResponse = GetDefaultCreditSpecificationResponse'
  { _gdcsrsInstanceFamilyCreditSpecification ::
      !( Maybe
           InstanceFamilyCreditSpecification
       ),
    _gdcsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDefaultCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcsrsInstanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
--
-- * 'gdcsrsResponseStatus' - -- | The response status code.
getDefaultCreditSpecificationResponse ::
  -- | 'gdcsrsResponseStatus'
  Int ->
  GetDefaultCreditSpecificationResponse
getDefaultCreditSpecificationResponse pResponseStatus_ =
  GetDefaultCreditSpecificationResponse'
    { _gdcsrsInstanceFamilyCreditSpecification =
        Nothing,
      _gdcsrsResponseStatus = pResponseStatus_
    }

-- | The default credit option for CPU usage of the instance family.
gdcsrsInstanceFamilyCreditSpecification :: Lens' GetDefaultCreditSpecificationResponse (Maybe InstanceFamilyCreditSpecification)
gdcsrsInstanceFamilyCreditSpecification = lens _gdcsrsInstanceFamilyCreditSpecification (\s a -> s {_gdcsrsInstanceFamilyCreditSpecification = a})

-- | -- | The response status code.
gdcsrsResponseStatus :: Lens' GetDefaultCreditSpecificationResponse Int
gdcsrsResponseStatus = lens _gdcsrsResponseStatus (\s a -> s {_gdcsrsResponseStatus = a})

instance NFData GetDefaultCreditSpecificationResponse
