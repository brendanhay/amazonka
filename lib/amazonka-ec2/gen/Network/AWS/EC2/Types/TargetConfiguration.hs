{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Convertible Reserved Instance offering.
--
--
--
-- /See:/ 'targetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { _tcInstanceCount ::
      !(Maybe Int),
    _tcOfferingId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInstanceCount' - The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- * 'tcOfferingId' - The ID of the Convertible Reserved Instance offering.
targetConfiguration ::
  TargetConfiguration
targetConfiguration =
  TargetConfiguration'
    { _tcInstanceCount = Nothing,
      _tcOfferingId = Nothing
    }

-- | The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
tcInstanceCount :: Lens' TargetConfiguration (Maybe Int)
tcInstanceCount = lens _tcInstanceCount (\s a -> s {_tcInstanceCount = a})

-- | The ID of the Convertible Reserved Instance offering.
tcOfferingId :: Lens' TargetConfiguration (Maybe Text)
tcOfferingId = lens _tcOfferingId (\s a -> s {_tcOfferingId = a})

instance FromXML TargetConfiguration where
  parseXML x =
    TargetConfiguration'
      <$> (x .@? "instanceCount") <*> (x .@? "offeringId")

instance Hashable TargetConfiguration

instance NFData TargetConfiguration
