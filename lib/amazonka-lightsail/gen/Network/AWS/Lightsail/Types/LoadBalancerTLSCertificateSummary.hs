{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of SSL/TLS certificate metadata.
--
--
--
-- /See:/ 'loadBalancerTLSCertificateSummary' smart constructor.
data LoadBalancerTLSCertificateSummary = LoadBalancerTLSCertificateSummary'
  { _lbtcsIsAttached ::
      !(Maybe Bool),
    _lbtcsName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerTLSCertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtcsIsAttached' - When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- * 'lbtcsName' - The name of the SSL/TLS certificate.
loadBalancerTLSCertificateSummary ::
  LoadBalancerTLSCertificateSummary
loadBalancerTLSCertificateSummary =
  LoadBalancerTLSCertificateSummary'
    { _lbtcsIsAttached = Nothing,
      _lbtcsName = Nothing
    }

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
lbtcsIsAttached :: Lens' LoadBalancerTLSCertificateSummary (Maybe Bool)
lbtcsIsAttached = lens _lbtcsIsAttached (\s a -> s {_lbtcsIsAttached = a})

-- | The name of the SSL/TLS certificate.
lbtcsName :: Lens' LoadBalancerTLSCertificateSummary (Maybe Text)
lbtcsName = lens _lbtcsName (\s a -> s {_lbtcsName = a})

instance FromJSON LoadBalancerTLSCertificateSummary where
  parseJSON =
    withObject
      "LoadBalancerTLSCertificateSummary"
      ( \x ->
          LoadBalancerTLSCertificateSummary'
            <$> (x .:? "isAttached") <*> (x .:? "name")
      )

instance Hashable LoadBalancerTLSCertificateSummary

instance NFData LoadBalancerTLSCertificateSummary
