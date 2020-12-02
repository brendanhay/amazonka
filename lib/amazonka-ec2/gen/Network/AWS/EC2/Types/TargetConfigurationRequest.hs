{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfigurationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the target configuration.
--
--
--
-- /See:/ 'targetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { _tcrInstanceCount ::
      !(Maybe Int),
    _tcrOfferingId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetConfigurationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcrInstanceCount' - The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- * 'tcrOfferingId' - The Convertible Reserved Instance offering ID.
targetConfigurationRequest ::
  -- | 'tcrOfferingId'
  Text ->
  TargetConfigurationRequest
targetConfigurationRequest pOfferingId_ =
  TargetConfigurationRequest'
    { _tcrInstanceCount = Nothing,
      _tcrOfferingId = pOfferingId_
    }

-- | The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
tcrInstanceCount :: Lens' TargetConfigurationRequest (Maybe Int)
tcrInstanceCount = lens _tcrInstanceCount (\s a -> s {_tcrInstanceCount = a})

-- | The Convertible Reserved Instance offering ID.
tcrOfferingId :: Lens' TargetConfigurationRequest Text
tcrOfferingId = lens _tcrOfferingId (\s a -> s {_tcrOfferingId = a})

instance Hashable TargetConfigurationRequest

instance NFData TargetConfigurationRequest

instance ToQuery TargetConfigurationRequest where
  toQuery TargetConfigurationRequest' {..} =
    mconcat
      [ "InstanceCount" =: _tcrInstanceCount,
        "OfferingId" =: _tcrOfferingId
      ]
