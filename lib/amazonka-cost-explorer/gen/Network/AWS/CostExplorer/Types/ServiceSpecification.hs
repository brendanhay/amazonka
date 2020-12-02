{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ServiceSpecification where

import Network.AWS.CostExplorer.Types.EC2Specification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Hardware specifications for the service that you want recommendations for.
--
--
--
-- /See:/ 'serviceSpecification' smart constructor.
newtype ServiceSpecification = ServiceSpecification'
  { _ssEC2Specification ::
      Maybe EC2Specification
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssEC2Specification' - The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
serviceSpecification ::
  ServiceSpecification
serviceSpecification =
  ServiceSpecification' {_ssEC2Specification = Nothing}

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
ssEC2Specification :: Lens' ServiceSpecification (Maybe EC2Specification)
ssEC2Specification = lens _ssEC2Specification (\s a -> s {_ssEC2Specification = a})

instance FromJSON ServiceSpecification where
  parseJSON =
    withObject
      "ServiceSpecification"
      (\x -> ServiceSpecification' <$> (x .:? "EC2Specification"))

instance Hashable ServiceSpecification

instance NFData ServiceSpecification

instance ToJSON ServiceSpecification where
  toJSON ServiceSpecification' {..} =
    object
      (catMaybes [("EC2Specification" .=) <$> _ssEC2Specification])
