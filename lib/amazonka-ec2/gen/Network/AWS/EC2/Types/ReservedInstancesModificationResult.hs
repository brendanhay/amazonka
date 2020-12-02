{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModificationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModificationResult where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservedInstancesConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the modification request/s.
--
--
--
-- /See:/ 'reservedInstancesModificationResult' smart constructor.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'
  { _rimrReservedInstancesId ::
      !(Maybe Text),
    _rimrTargetConfiguration ::
      !( Maybe
           ReservedInstancesConfiguration
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstancesModificationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimrReservedInstancesId' - The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
--
-- * 'rimrTargetConfiguration' - The target Reserved Instances configurations supplied as part of the modification request.
reservedInstancesModificationResult ::
  ReservedInstancesModificationResult
reservedInstancesModificationResult =
  ReservedInstancesModificationResult'
    { _rimrReservedInstancesId =
        Nothing,
      _rimrTargetConfiguration = Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
rimrReservedInstancesId :: Lens' ReservedInstancesModificationResult (Maybe Text)
rimrReservedInstancesId = lens _rimrReservedInstancesId (\s a -> s {_rimrReservedInstancesId = a})

-- | The target Reserved Instances configurations supplied as part of the modification request.
rimrTargetConfiguration :: Lens' ReservedInstancesModificationResult (Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration = lens _rimrTargetConfiguration (\s a -> s {_rimrTargetConfiguration = a})

instance FromXML ReservedInstancesModificationResult where
  parseXML x =
    ReservedInstancesModificationResult'
      <$> (x .@? "reservedInstancesId") <*> (x .@? "targetConfiguration")

instance Hashable ReservedInstancesModificationResult

instance NFData ReservedInstancesModificationResult
