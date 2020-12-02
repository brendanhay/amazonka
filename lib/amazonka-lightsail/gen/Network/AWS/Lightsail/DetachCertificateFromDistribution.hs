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
-- Module      : Network.AWS.Lightsail.DetachCertificateFromDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an SSL/TLS certificate from your Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- After the certificate is detached, your distribution stops accepting traffic for all of the domains that are associated with the certificate.
module Network.AWS.Lightsail.DetachCertificateFromDistribution
  ( -- * Creating a Request
    detachCertificateFromDistribution,
    DetachCertificateFromDistribution,

    -- * Request Lenses
    dcfdDistributionName,

    -- * Destructuring the Response
    detachCertificateFromDistributionResponse,
    DetachCertificateFromDistributionResponse,

    -- * Response Lenses
    dcfdrsOperation,
    dcfdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachCertificateFromDistribution' smart constructor.
newtype DetachCertificateFromDistribution = DetachCertificateFromDistribution'
  { _dcfdDistributionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DetachCertificateFromDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfdDistributionName' - The name of the distribution from which to detach the certificate. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
detachCertificateFromDistribution ::
  -- | 'dcfdDistributionName'
  Text ->
  DetachCertificateFromDistribution
detachCertificateFromDistribution pDistributionName_ =
  DetachCertificateFromDistribution'
    { _dcfdDistributionName =
        pDistributionName_
    }

-- | The name of the distribution from which to detach the certificate. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
dcfdDistributionName :: Lens' DetachCertificateFromDistribution Text
dcfdDistributionName = lens _dcfdDistributionName (\s a -> s {_dcfdDistributionName = a})

instance AWSRequest DetachCertificateFromDistribution where
  type
    Rs DetachCertificateFromDistribution =
      DetachCertificateFromDistributionResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DetachCertificateFromDistributionResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable DetachCertificateFromDistribution

instance NFData DetachCertificateFromDistribution

instance ToHeaders DetachCertificateFromDistribution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.DetachCertificateFromDistribution" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DetachCertificateFromDistribution where
  toJSON DetachCertificateFromDistribution' {..} =
    object
      (catMaybes [Just ("distributionName" .= _dcfdDistributionName)])

instance ToPath DetachCertificateFromDistribution where
  toPath = const "/"

instance ToQuery DetachCertificateFromDistribution where
  toQuery = const mempty

-- | /See:/ 'detachCertificateFromDistributionResponse' smart constructor.
data DetachCertificateFromDistributionResponse = DetachCertificateFromDistributionResponse'
  { _dcfdrsOperation ::
      !( Maybe
           Operation
       ),
    _dcfdrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DetachCertificateFromDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfdrsOperation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dcfdrsResponseStatus' - -- | The response status code.
detachCertificateFromDistributionResponse ::
  -- | 'dcfdrsResponseStatus'
  Int ->
  DetachCertificateFromDistributionResponse
detachCertificateFromDistributionResponse pResponseStatus_ =
  DetachCertificateFromDistributionResponse'
    { _dcfdrsOperation =
        Nothing,
      _dcfdrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dcfdrsOperation :: Lens' DetachCertificateFromDistributionResponse (Maybe Operation)
dcfdrsOperation = lens _dcfdrsOperation (\s a -> s {_dcfdrsOperation = a})

-- | -- | The response status code.
dcfdrsResponseStatus :: Lens' DetachCertificateFromDistributionResponse Int
dcfdrsResponseStatus = lens _dcfdrsResponseStatus (\s a -> s {_dcfdrsResponseStatus = a})

instance NFData DetachCertificateFromDistributionResponse
