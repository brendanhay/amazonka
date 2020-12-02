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
-- Module      : Network.AWS.Lightsail.AttachCertificateToDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an SSL/TLS certificate to your Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- After the certificate is attached, your distribution accepts HTTPS traffic for all of the domains that are associated with the certificate.
--
-- Use the @CreateCertificate@ action to create a certificate that you can attach to your distribution.
--
-- /Important:/ Only certificates created in the @us-east-1@ AWS Region can be attached to Lightsail distributions. Lightsail distributions are global resources that can reference an origin in any AWS Region, and distribute its content globally. However, all distributions are located in the @us-east-1@ Region.
module Network.AWS.Lightsail.AttachCertificateToDistribution
  ( -- * Creating a Request
    attachCertificateToDistribution,
    AttachCertificateToDistribution,

    -- * Request Lenses
    actdDistributionName,
    actdCertificateName,

    -- * Destructuring the Response
    attachCertificateToDistributionResponse,
    AttachCertificateToDistributionResponse,

    -- * Response Lenses
    actdrsOperation,
    actdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachCertificateToDistribution' smart constructor.
data AttachCertificateToDistribution = AttachCertificateToDistribution'
  { _actdDistributionName ::
      !Text,
    _actdCertificateName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachCertificateToDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actdDistributionName' - The name of the distribution that the certificate will be attached to. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- * 'actdCertificateName' - The name of the certificate to attach to a distribution. Only certificates with a status of @ISSUED@ can be attached to a distribution. Use the @GetCertificates@ action to get a list of certificate names that you can specify.
attachCertificateToDistribution ::
  -- | 'actdDistributionName'
  Text ->
  -- | 'actdCertificateName'
  Text ->
  AttachCertificateToDistribution
attachCertificateToDistribution
  pDistributionName_
  pCertificateName_ =
    AttachCertificateToDistribution'
      { _actdDistributionName =
          pDistributionName_,
        _actdCertificateName = pCertificateName_
      }

-- | The name of the distribution that the certificate will be attached to. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
actdDistributionName :: Lens' AttachCertificateToDistribution Text
actdDistributionName = lens _actdDistributionName (\s a -> s {_actdDistributionName = a})

-- | The name of the certificate to attach to a distribution. Only certificates with a status of @ISSUED@ can be attached to a distribution. Use the @GetCertificates@ action to get a list of certificate names that you can specify.
actdCertificateName :: Lens' AttachCertificateToDistribution Text
actdCertificateName = lens _actdCertificateName (\s a -> s {_actdCertificateName = a})

instance AWSRequest AttachCertificateToDistribution where
  type
    Rs AttachCertificateToDistribution =
      AttachCertificateToDistributionResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          AttachCertificateToDistributionResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable AttachCertificateToDistribution

instance NFData AttachCertificateToDistribution

instance ToHeaders AttachCertificateToDistribution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.AttachCertificateToDistribution" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AttachCertificateToDistribution where
  toJSON AttachCertificateToDistribution' {..} =
    object
      ( catMaybes
          [ Just ("distributionName" .= _actdDistributionName),
            Just ("certificateName" .= _actdCertificateName)
          ]
      )

instance ToPath AttachCertificateToDistribution where
  toPath = const "/"

instance ToQuery AttachCertificateToDistribution where
  toQuery = const mempty

-- | /See:/ 'attachCertificateToDistributionResponse' smart constructor.
data AttachCertificateToDistributionResponse = AttachCertificateToDistributionResponse'
  { _actdrsOperation ::
      !( Maybe
           Operation
       ),
    _actdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachCertificateToDistributionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'actdrsOperation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'actdrsResponseStatus' - -- | The response status code.
attachCertificateToDistributionResponse ::
  -- | 'actdrsResponseStatus'
  Int ->
  AttachCertificateToDistributionResponse
attachCertificateToDistributionResponse pResponseStatus_ =
  AttachCertificateToDistributionResponse'
    { _actdrsOperation =
        Nothing,
      _actdrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
actdrsOperation :: Lens' AttachCertificateToDistributionResponse (Maybe Operation)
actdrsOperation = lens _actdrsOperation (\s a -> s {_actdrsOperation = a})

-- | -- | The response status code.
actdrsResponseStatus :: Lens' AttachCertificateToDistributionResponse Int
actdrsResponseStatus = lens _actdrsResponseStatus (\s a -> s {_actdrsResponseStatus = a})

instance NFData AttachCertificateToDistributionResponse
