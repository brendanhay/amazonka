{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceTypeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceTypeDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ServiceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the type of service for a VPC endpoint.
--
--
--
-- /See:/ 'serviceTypeDetail' smart constructor.
newtype ServiceTypeDetail = ServiceTypeDetail'
  { _stdServiceType ::
      Maybe ServiceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceTypeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdServiceType' - The type of service.
serviceTypeDetail ::
  ServiceTypeDetail
serviceTypeDetail = ServiceTypeDetail' {_stdServiceType = Nothing}

-- | The type of service.
stdServiceType :: Lens' ServiceTypeDetail (Maybe ServiceType)
stdServiceType = lens _stdServiceType (\s a -> s {_stdServiceType = a})

instance FromXML ServiceTypeDetail where
  parseXML x = ServiceTypeDetail' <$> (x .@? "serviceType")

instance Hashable ServiceTypeDetail

instance NFData ServiceTypeDetail
