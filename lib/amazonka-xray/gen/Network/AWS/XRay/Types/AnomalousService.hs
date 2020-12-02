{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AnomalousService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnomalousService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.ServiceId

-- | The service within the service graph that has anomalously high fault rates.
--
--
--
-- /See:/ 'anomalousService' smart constructor.
newtype AnomalousService = AnomalousService'
  { _asServiceId ::
      Maybe ServiceId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalousService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asServiceId' - Undocumented member.
anomalousService ::
  AnomalousService
anomalousService = AnomalousService' {_asServiceId = Nothing}

-- | Undocumented member.
asServiceId :: Lens' AnomalousService (Maybe ServiceId)
asServiceId = lens _asServiceId (\s a -> s {_asServiceId = a})

instance FromJSON AnomalousService where
  parseJSON =
    withObject
      "AnomalousService"
      (\x -> AnomalousService' <$> (x .:? "ServiceId"))

instance Hashable AnomalousService

instance NFData AnomalousService
