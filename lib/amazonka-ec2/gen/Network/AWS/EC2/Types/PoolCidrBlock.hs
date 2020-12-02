{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PoolCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PoolCidrBlock where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a CIDR block for an address pool.
--
--
--
-- /See:/ 'poolCidrBlock' smart constructor.
newtype PoolCidrBlock = PoolCidrBlock' {_pcbCidr :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PoolCidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcbCidr' - The CIDR block.
poolCidrBlock ::
  PoolCidrBlock
poolCidrBlock = PoolCidrBlock' {_pcbCidr = Nothing}

-- | The CIDR block.
pcbCidr :: Lens' PoolCidrBlock (Maybe Text)
pcbCidr = lens _pcbCidr (\s a -> s {_pcbCidr = a})

instance FromXML PoolCidrBlock where
  parseXML x = PoolCidrBlock' <$> (x .@? "poolCidrBlock")

instance Hashable PoolCidrBlock

instance NFData PoolCidrBlock
