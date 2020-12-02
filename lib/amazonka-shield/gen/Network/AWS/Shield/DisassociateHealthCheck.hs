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
-- Module      : Network.AWS.Shield.DisassociateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes health-based detection from the Shield Advanced protection for a resource. Shield Advanced health-based detection uses the health of your AWS resource to improve responsiveness and accuracy in attack detection and mitigation.
--
--
-- You define the health check in Route 53 and then associate or disassociate it with your Shield Advanced protection. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection> in the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide> .
module Network.AWS.Shield.DisassociateHealthCheck
  ( -- * Creating a Request
    disassociateHealthCheck,
    DisassociateHealthCheck,

    -- * Request Lenses
    dhcProtectionId,
    dhcHealthCheckARN,

    -- * Destructuring the Response
    disassociateHealthCheckResponse,
    DisassociateHealthCheckResponse,

    -- * Response Lenses
    dhcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'disassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { _dhcProtectionId ::
      !Text,
    _dhcHealthCheckARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateHealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcProtectionId' - The unique identifier (ID) for the 'Protection' object to remove the health check association from.
--
-- * 'dhcHealthCheckARN' - The Amazon Resource Name (ARN) of the health check that is associated with the protection.
disassociateHealthCheck ::
  -- | 'dhcProtectionId'
  Text ->
  -- | 'dhcHealthCheckARN'
  Text ->
  DisassociateHealthCheck
disassociateHealthCheck pProtectionId_ pHealthCheckARN_ =
  DisassociateHealthCheck'
    { _dhcProtectionId = pProtectionId_,
      _dhcHealthCheckARN = pHealthCheckARN_
    }

-- | The unique identifier (ID) for the 'Protection' object to remove the health check association from.
dhcProtectionId :: Lens' DisassociateHealthCheck Text
dhcProtectionId = lens _dhcProtectionId (\s a -> s {_dhcProtectionId = a})

-- | The Amazon Resource Name (ARN) of the health check that is associated with the protection.
dhcHealthCheckARN :: Lens' DisassociateHealthCheck Text
dhcHealthCheckARN = lens _dhcHealthCheckARN (\s a -> s {_dhcHealthCheckARN = a})

instance AWSRequest DisassociateHealthCheck where
  type Rs DisassociateHealthCheck = DisassociateHealthCheckResponse
  request = postJSON shield
  response =
    receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisassociateHealthCheck

instance NFData DisassociateHealthCheck

instance ToHeaders DisassociateHealthCheck where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.DisassociateHealthCheck" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck' {..} =
    object
      ( catMaybes
          [ Just ("ProtectionId" .= _dhcProtectionId),
            Just ("HealthCheckArn" .= _dhcHealthCheckARN)
          ]
      )

instance ToPath DisassociateHealthCheck where
  toPath = const "/"

instance ToQuery DisassociateHealthCheck where
  toQuery = const mempty

-- | /See:/ 'disassociateHealthCheckResponse' smart constructor.
newtype DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { _dhcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateHealthCheckResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcrsResponseStatus' - -- | The response status code.
disassociateHealthCheckResponse ::
  -- | 'dhcrsResponseStatus'
  Int ->
  DisassociateHealthCheckResponse
disassociateHealthCheckResponse pResponseStatus_ =
  DisassociateHealthCheckResponse'
    { _dhcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dhcrsResponseStatus :: Lens' DisassociateHealthCheckResponse Int
dhcrsResponseStatus = lens _dhcrsResponseStatus (\s a -> s {_dhcrsResponseStatus = a})

instance NFData DisassociateHealthCheckResponse
