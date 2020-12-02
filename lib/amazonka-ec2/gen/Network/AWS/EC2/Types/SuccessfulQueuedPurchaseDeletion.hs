{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SuccessfulQueuedPurchaseDeletion where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance whose queued purchase was successfully deleted.
--
--
--
-- /See:/ 'successfulQueuedPurchaseDeletion' smart constructor.
newtype SuccessfulQueuedPurchaseDeletion = SuccessfulQueuedPurchaseDeletion'
  { _sqpdReservedInstancesId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SuccessfulQueuedPurchaseDeletion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqpdReservedInstancesId' - The ID of the Reserved Instance.
successfulQueuedPurchaseDeletion ::
  SuccessfulQueuedPurchaseDeletion
successfulQueuedPurchaseDeletion =
  SuccessfulQueuedPurchaseDeletion'
    { _sqpdReservedInstancesId =
        Nothing
    }

-- | The ID of the Reserved Instance.
sqpdReservedInstancesId :: Lens' SuccessfulQueuedPurchaseDeletion (Maybe Text)
sqpdReservedInstancesId = lens _sqpdReservedInstancesId (\s a -> s {_sqpdReservedInstancesId = a})

instance FromXML SuccessfulQueuedPurchaseDeletion where
  parseXML x =
    SuccessfulQueuedPurchaseDeletion'
      <$> (x .@? "reservedInstancesId")

instance Hashable SuccessfulQueuedPurchaseDeletion

instance NFData SuccessfulQueuedPurchaseDeletion
