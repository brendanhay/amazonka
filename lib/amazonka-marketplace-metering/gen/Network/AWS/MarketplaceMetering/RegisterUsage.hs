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
-- Module      : Network.AWS.MarketplaceMetering.RegisterUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Paid container software products sold through AWS Marketplace must integrate with the AWS Marketplace Metering Service and call the RegisterUsage operation for software entitlement and metering. Free and BYOL products for Amazon ECS or Amazon EKS aren't required to call RegisterUsage, but you may choose to do so if you would like to receive usage data in your seller reports. The sections below explain the behavior of RegisterUsage. RegisterUsage performs two primary functions: metering and entitlement.
--
--
--     * /Entitlement/ : RegisterUsage allows you to verify that the customer running your paid software is subscribed to your product on AWS Marketplace, enabling you to guard against unauthorized use. Your container image that integrates with RegisterUsage is only required to guard against unauthorized use at container startup, as such a CustomerNotSubscribedException/PlatformNotSupportedException will only be thrown on the initial call to RegisterUsage. Subsequent calls from the same Amazon ECS task instance (e.g. task-id) or Amazon EKS pod will not throw a CustomerNotSubscribedException, even if the customer unsubscribes while the Amazon ECS task or Amazon EKS pod is still running.
--
--     * /Metering/ : RegisterUsage meters software use per ECS task, per hour, or per pod for Amazon EKS with usage prorated to the second. A minimum of 1 minute of usage applies to tasks that are short lived. For example, if a customer has a 10 node Amazon ECS or Amazon EKS cluster and a service configured as a Daemon Set, then Amazon ECS or Amazon EKS will launch a task on all 10 cluster nodes and the customer will be charged: (10 * hourly_rate). Metering for software use is automatically handled by the AWS Marketplace Metering Control Plane -- your software is not required to perform any metering specific actions, other than call RegisterUsage once for metering of software use to commence. The AWS Marketplace Metering Control Plane will also continue to bill customers for running ECS tasks and Amazon EKS pods, regardless of the customers subscription state, removing the need for your software to perform entitlement checks at runtime.
module Network.AWS.MarketplaceMetering.RegisterUsage
  ( -- * Creating a Request
    registerUsage,
    RegisterUsage,

    -- * Request Lenses
    ruNonce,
    ruProductCode,
    ruPublicKeyVersion,

    -- * Destructuring the Response
    registerUsageResponse,
    RegisterUsageResponse,

    -- * Response Lenses
    rursSignature,
    rursPublicKeyRotationTimestamp,
    rursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerUsage' smart constructor.
data RegisterUsage = RegisterUsage'
  { _ruNonce :: !(Maybe Text),
    _ruProductCode :: !Text,
    _ruPublicKeyVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruNonce' - (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
--
-- * 'ruProductCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- * 'ruPublicKeyVersion' - Public Key Version provided by AWS Marketplace
registerUsage ::
  -- | 'ruProductCode'
  Text ->
  -- | 'ruPublicKeyVersion'
  Natural ->
  RegisterUsage
registerUsage pProductCode_ pPublicKeyVersion_ =
  RegisterUsage'
    { _ruNonce = Nothing,
      _ruProductCode = pProductCode_,
      _ruPublicKeyVersion = _Nat # pPublicKeyVersion_
    }

-- | (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
ruNonce :: Lens' RegisterUsage (Maybe Text)
ruNonce = lens _ruNonce (\s a -> s {_ruNonce = a})

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
ruProductCode :: Lens' RegisterUsage Text
ruProductCode = lens _ruProductCode (\s a -> s {_ruProductCode = a})

-- | Public Key Version provided by AWS Marketplace
ruPublicKeyVersion :: Lens' RegisterUsage Natural
ruPublicKeyVersion = lens _ruPublicKeyVersion (\s a -> s {_ruPublicKeyVersion = a}) . _Nat

instance AWSRequest RegisterUsage where
  type Rs RegisterUsage = RegisterUsageResponse
  request = postJSON marketplaceMetering
  response =
    receiveJSON
      ( \s h x ->
          RegisterUsageResponse'
            <$> (x .?> "Signature")
            <*> (x .?> "PublicKeyRotationTimestamp")
            <*> (pure (fromEnum s))
      )

instance Hashable RegisterUsage

instance NFData RegisterUsage

instance ToHeaders RegisterUsage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSMPMeteringService.RegisterUsage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterUsage where
  toJSON RegisterUsage' {..} =
    object
      ( catMaybes
          [ ("Nonce" .=) <$> _ruNonce,
            Just ("ProductCode" .= _ruProductCode),
            Just ("PublicKeyVersion" .= _ruPublicKeyVersion)
          ]
      )

instance ToPath RegisterUsage where
  toPath = const "/"

instance ToQuery RegisterUsage where
  toQuery = const mempty

-- | /See:/ 'registerUsageResponse' smart constructor.
data RegisterUsageResponse = RegisterUsageResponse'
  { _rursSignature ::
      !(Maybe Text),
    _rursPublicKeyRotationTimestamp ::
      !(Maybe POSIX),
    _rursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rursSignature' - JWT Token
--
-- * 'rursPublicKeyRotationTimestamp' - (Optional) Only included when public key version has expired
--
-- * 'rursResponseStatus' - -- | The response status code.
registerUsageResponse ::
  -- | 'rursResponseStatus'
  Int ->
  RegisterUsageResponse
registerUsageResponse pResponseStatus_ =
  RegisterUsageResponse'
    { _rursSignature = Nothing,
      _rursPublicKeyRotationTimestamp = Nothing,
      _rursResponseStatus = pResponseStatus_
    }

-- | JWT Token
rursSignature :: Lens' RegisterUsageResponse (Maybe Text)
rursSignature = lens _rursSignature (\s a -> s {_rursSignature = a})

-- | (Optional) Only included when public key version has expired
rursPublicKeyRotationTimestamp :: Lens' RegisterUsageResponse (Maybe UTCTime)
rursPublicKeyRotationTimestamp = lens _rursPublicKeyRotationTimestamp (\s a -> s {_rursPublicKeyRotationTimestamp = a}) . mapping _Time

-- | -- | The response status code.
rursResponseStatus :: Lens' RegisterUsageResponse Int
rursResponseStatus = lens _rursResponseStatus (\s a -> s {_rursResponseStatus = a})

instance NFData RegisterUsageResponse
