{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.LinkedService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.LinkedService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

-- | If a health check or hosted zone was created by another service, @LinkedService@ is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
--
--
-- /See:/ 'linkedService' smart constructor.
data LinkedService = LinkedService'
  { _lsServicePrincipal ::
      !(Maybe Text),
    _lsDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LinkedService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsServicePrincipal' - If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- * 'lsDescription' - If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
linkedService ::
  LinkedService
linkedService =
  LinkedService'
    { _lsServicePrincipal = Nothing,
      _lsDescription = Nothing
    }

-- | If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
lsServicePrincipal :: Lens' LinkedService (Maybe Text)
lsServicePrincipal = lens _lsServicePrincipal (\s a -> s {_lsServicePrincipal = a})

-- | If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
lsDescription :: Lens' LinkedService (Maybe Text)
lsDescription = lens _lsDescription (\s a -> s {_lsDescription = a})

instance FromXML LinkedService where
  parseXML x =
    LinkedService'
      <$> (x .@? "ServicePrincipal") <*> (x .@? "Description")

instance Hashable LinkedService

instance NFData LinkedService
