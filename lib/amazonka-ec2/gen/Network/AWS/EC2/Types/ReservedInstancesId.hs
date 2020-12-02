{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesId where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the ID of a Reserved Instance.
--
--
--
-- /See:/ 'reservedInstancesId' smart constructor.
newtype ReservedInstancesId = ReservedInstancesId'
  { _riiReservedInstancesId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstancesId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riiReservedInstancesId' - The ID of the Reserved Instance.
reservedInstancesId ::
  ReservedInstancesId
reservedInstancesId =
  ReservedInstancesId' {_riiReservedInstancesId = Nothing}

-- | The ID of the Reserved Instance.
riiReservedInstancesId :: Lens' ReservedInstancesId (Maybe Text)
riiReservedInstancesId = lens _riiReservedInstancesId (\s a -> s {_riiReservedInstancesId = a})

instance FromXML ReservedInstancesId where
  parseXML x = ReservedInstancesId' <$> (x .@? "reservedInstancesId")

instance Hashable ReservedInstancesId

instance NFData ReservedInstancesId
