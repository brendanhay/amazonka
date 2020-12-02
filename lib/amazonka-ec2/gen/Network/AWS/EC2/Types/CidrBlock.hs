{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CidrBlock where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv4 CIDR block.
--
--
--
-- /See:/ 'cidrBlock' smart constructor.
newtype CidrBlock = CidrBlock' {_cbCidrBlock :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CidrBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbCidrBlock' - The IPv4 CIDR block.
cidrBlock ::
  CidrBlock
cidrBlock = CidrBlock' {_cbCidrBlock = Nothing}

-- | The IPv4 CIDR block.
cbCidrBlock :: Lens' CidrBlock (Maybe Text)
cbCidrBlock = lens _cbCidrBlock (\s a -> s {_cbCidrBlock = a})

instance FromXML CidrBlock where
  parseXML x = CidrBlock' <$> (x .@? "cidrBlock")

instance Hashable CidrBlock

instance NFData CidrBlock
