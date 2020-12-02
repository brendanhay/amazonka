{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SubnetOutpost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SubnetOutpost where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ID of the outpost subnet.
--
--
--
-- /See:/ 'subnetOutpost' smart constructor.
newtype SubnetOutpost = SubnetOutpost'
  { _soSubnetOutpostARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubnetOutpost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soSubnetOutpostARN' - The outpost ARN of the subnet.
subnetOutpost ::
  SubnetOutpost
subnetOutpost = SubnetOutpost' {_soSubnetOutpostARN = Nothing}

-- | The outpost ARN of the subnet.
soSubnetOutpostARN :: Lens' SubnetOutpost (Maybe Text)
soSubnetOutpostARN = lens _soSubnetOutpostARN (\s a -> s {_soSubnetOutpostARN = a})

instance FromXML SubnetOutpost where
  parseXML x = SubnetOutpost' <$> (x .@? "SubnetOutpostArn")

instance Hashable SubnetOutpost

instance NFData SubnetOutpost
