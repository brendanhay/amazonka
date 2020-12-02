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
-- Module      : Network.AWS.Shield.AssociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds health-based detection to the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
--
-- You define the health check in Route 53 and then associate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.AssociateHealthCheck
  ( -- * Creating a Request
    associateHealthCheck,
    AssociateHealthCheck,

    -- * Request Lenses
    ahcProtectionId,
    ahcHealthCheckARN,

    -- * Destructuring the Response
    associateHealthCheckResponse,
    AssociateHealthCheckResponse,

    -- * Response Lenses
    ahcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'associateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { _ahcProtectionId ::
      !Text,
    _ahcHealthCheckARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahcProtectionId' - The unique identifier (ID) for the 'Protection' object to add the health check association to.
--
-- * 'ahcHealthCheckARN' - The Amazon Resource Name (ARN) of the health check to associate with the protection.
associateHealthCheck ::
  -- | 'ahcProtectionId'
  Text ->
  -- | 'ahcHealthCheckARN'
  Text ->
  AssociateHealthCheck
associateHealthCheck pProtectionId_ pHealthCheckARN_ =
  AssociateHealthCheck'
    { _ahcProtectionId = pProtectionId_,
      _ahcHealthCheckARN = pHealthCheckARN_
    }

-- | The unique identifier (ID) for the 'Protection' object to add the health check association to.
ahcProtectionId :: Lens' AssociateHealthCheck Text
ahcProtectionId = lens _ahcProtectionId (\s a -> s {_ahcProtectionId = a})

-- | The Amazon Resource Name (ARN) of the health check to associate with the protection.
ahcHealthCheckARN :: Lens' AssociateHealthCheck Text
ahcHealthCheckARN = lens _ahcHealthCheckARN (\s a -> s {_ahcHealthCheckARN = a})

instance AWSRequest AssociateHealthCheck where
  type Rs AssociateHealthCheck = AssociateHealthCheckResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> AssociateHealthCheckResponse' <$> (pure (fromEnum s)))

instance Hashable AssociateHealthCheck

instance NFData AssociateHealthCheck

instance ToHeaders AssociateHealthCheck where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.AssociateHealthCheck" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateHealthCheck where
  toJSON AssociateHealthCheck' {..} =
    object
      ( catMaybes
          [ Just ("ProtectionId" .= _ahcProtectionId),
            Just ("HealthCheckArn" .= _ahcHealthCheckARN)
          ]
      )

instance ToPath AssociateHealthCheck where
  toPath = const "/"

instance ToQuery AssociateHealthCheck where
  toQuery = const mempty

-- | /See:/ 'associateHealthCheckResponse' smart constructor.
newtype AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { _ahcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahcrsResponseStatus' - -- | The response status code.
associateHealthCheckResponse ::
  -- | 'ahcrsResponseStatus'
  Int ->
  AssociateHealthCheckResponse
associateHealthCheckResponse pResponseStatus_ =
  AssociateHealthCheckResponse'
    { _ahcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ahcrsResponseStatus :: Lens' AssociateHealthCheckResponse Int
ahcrsResponseStatus = lens _ahcrsResponseStatus (\s a -> s {_ahcrsResponseStatus = a})

instance NFData AssociateHealthCheckResponse
