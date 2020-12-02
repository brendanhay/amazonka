{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceProperties where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSProperties
import Network.AWS.Route53AutoNaming.Types.HTTPProperties

-- | A complex type that contains information that is specific to the namespace type.
--
--
--
-- /See:/ 'namespaceProperties' smart constructor.
data NamespaceProperties = NamespaceProperties'
  { _npDNSProperties ::
      !(Maybe DNSProperties),
    _npHTTPProperties :: !(Maybe HTTPProperties)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NamespaceProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npDNSProperties' - A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- * 'npHTTPProperties' - A complex type that contains the name of an HTTP namespace.
namespaceProperties ::
  NamespaceProperties
namespaceProperties =
  NamespaceProperties'
    { _npDNSProperties = Nothing,
      _npHTTPProperties = Nothing
    }

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
npDNSProperties :: Lens' NamespaceProperties (Maybe DNSProperties)
npDNSProperties = lens _npDNSProperties (\s a -> s {_npDNSProperties = a})

-- | A complex type that contains the name of an HTTP namespace.
npHTTPProperties :: Lens' NamespaceProperties (Maybe HTTPProperties)
npHTTPProperties = lens _npHTTPProperties (\s a -> s {_npHTTPProperties = a})

instance FromJSON NamespaceProperties where
  parseJSON =
    withObject
      "NamespaceProperties"
      ( \x ->
          NamespaceProperties'
            <$> (x .:? "DnsProperties") <*> (x .:? "HttpProperties")
      )

instance Hashable NamespaceProperties

instance NFData NamespaceProperties
