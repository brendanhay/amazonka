{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
--
--
-- /See:/ 'failedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { _fqpdError ::
      !( Maybe
           DeleteQueuedReservedInstancesError
       ),
    _fqpdReservedInstancesId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedQueuedPurchaseDeletion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fqpdError' - The error.
--
-- * 'fqpdReservedInstancesId' - The ID of the Reserved Instance.
failedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
failedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { _fqpdError = Nothing,
      _fqpdReservedInstancesId = Nothing
    }

-- | The error.
fqpdError :: Lens' FailedQueuedPurchaseDeletion (Maybe DeleteQueuedReservedInstancesError)
fqpdError = lens _fqpdError (\s a -> s {_fqpdError = a})

-- | The ID of the Reserved Instance.
fqpdReservedInstancesId :: Lens' FailedQueuedPurchaseDeletion (Maybe Text)
fqpdReservedInstancesId = lens _fqpdReservedInstancesId (\s a -> s {_fqpdReservedInstancesId = a})

instance FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      <$> (x .@? "error") <*> (x .@? "reservedInstancesId")

instance Hashable FailedQueuedPurchaseDeletion

instance NFData FailedQueuedPurchaseDeletion
