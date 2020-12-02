{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2Specification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2Specification where

import Network.AWS.CostExplorer.Types.OfferingClass
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
--
--
-- /See:/ 'ec2Specification' smart constructor.
newtype EC2Specification = EC2Specification'
  { _esOfferingClass ::
      Maybe OfferingClass
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2Specification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esOfferingClass' - Whether you want a recommendation for standard or convertible reservations.
ec2Specification ::
  EC2Specification
ec2Specification = EC2Specification' {_esOfferingClass = Nothing}

-- | Whether you want a recommendation for standard or convertible reservations.
esOfferingClass :: Lens' EC2Specification (Maybe OfferingClass)
esOfferingClass = lens _esOfferingClass (\s a -> s {_esOfferingClass = a})

instance FromJSON EC2Specification where
  parseJSON =
    withObject
      "EC2Specification"
      (\x -> EC2Specification' <$> (x .:? "OfferingClass"))

instance Hashable EC2Specification

instance NFData EC2Specification

instance ToJSON EC2Specification where
  toJSON EC2Specification' {..} =
    object (catMaybes [("OfferingClass" .=) <$> _esOfferingClass])
