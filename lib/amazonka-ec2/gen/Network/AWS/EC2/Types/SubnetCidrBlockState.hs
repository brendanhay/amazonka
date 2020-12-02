{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockState where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the state of a CIDR block.
--
--
--
-- /See:/ 'subnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { _scbsState ::
      !(Maybe SubnetCidrBlockStateCode),
    _scbsStatusMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubnetCidrBlockState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scbsState' - The state of a CIDR block.
--
-- * 'scbsStatusMessage' - A message about the status of the CIDR block, if applicable.
subnetCidrBlockState ::
  SubnetCidrBlockState
subnetCidrBlockState =
  SubnetCidrBlockState'
    { _scbsState = Nothing,
      _scbsStatusMessage = Nothing
    }

-- | The state of a CIDR block.
scbsState :: Lens' SubnetCidrBlockState (Maybe SubnetCidrBlockStateCode)
scbsState = lens _scbsState (\s a -> s {_scbsState = a})

-- | A message about the status of the CIDR block, if applicable.
scbsStatusMessage :: Lens' SubnetCidrBlockState (Maybe Text)
scbsStatusMessage = lens _scbsStatusMessage (\s a -> s {_scbsStatusMessage = a})

instance FromXML SubnetCidrBlockState where
  parseXML x =
    SubnetCidrBlockState'
      <$> (x .@? "state") <*> (x .@? "statusMessage")

instance Hashable SubnetCidrBlockState

instance NFData SubnetCidrBlockState
