{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCCidrBlockAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VPCCidrBlockState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv4 CIDR block associated with a VPC.
--
--
--
-- /See:/ 'vpcCidrBlockAssociation' smart constructor.
data VPCCidrBlockAssociation = VPCCidrBlockAssociation'
  { _vcbaAssociationId ::
      !(Maybe Text),
    _vcbaCidrBlockState ::
      !(Maybe VPCCidrBlockState),
    _vcbaCidrBlock :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VPCCidrBlockAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcbaAssociationId' - The association ID for the IPv4 CIDR block.
--
-- * 'vcbaCidrBlockState' - Information about the state of the CIDR block.
--
-- * 'vcbaCidrBlock' - The IPv4 CIDR block.
vpcCidrBlockAssociation ::
  VPCCidrBlockAssociation
vpcCidrBlockAssociation =
  VPCCidrBlockAssociation'
    { _vcbaAssociationId = Nothing,
      _vcbaCidrBlockState = Nothing,
      _vcbaCidrBlock = Nothing
    }

-- | The association ID for the IPv4 CIDR block.
vcbaAssociationId :: Lens' VPCCidrBlockAssociation (Maybe Text)
vcbaAssociationId = lens _vcbaAssociationId (\s a -> s {_vcbaAssociationId = a})

-- | Information about the state of the CIDR block.
vcbaCidrBlockState :: Lens' VPCCidrBlockAssociation (Maybe VPCCidrBlockState)
vcbaCidrBlockState = lens _vcbaCidrBlockState (\s a -> s {_vcbaCidrBlockState = a})

-- | The IPv4 CIDR block.
vcbaCidrBlock :: Lens' VPCCidrBlockAssociation (Maybe Text)
vcbaCidrBlock = lens _vcbaCidrBlock (\s a -> s {_vcbaCidrBlock = a})

instance FromXML VPCCidrBlockAssociation where
  parseXML x =
    VPCCidrBlockAssociation'
      <$> (x .@? "associationId")
      <*> (x .@? "cidrBlockState")
      <*> (x .@? "cidrBlock")

instance Hashable VPCCidrBlockAssociation

instance NFData VPCCidrBlockAssociation
