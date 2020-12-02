{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the burstable performance instance whose credit option for CPU usage was successfully modified.
--
--
--
-- /See:/ 'successfulInstanceCreditSpecificationItem' smart constructor.
newtype SuccessfulInstanceCreditSpecificationItem = SuccessfulInstanceCreditSpecificationItem'
  { _sicsiInstanceId ::
      Maybe
        Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'SuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sicsiInstanceId' - The ID of the instance.
successfulInstanceCreditSpecificationItem ::
  SuccessfulInstanceCreditSpecificationItem
successfulInstanceCreditSpecificationItem =
  SuccessfulInstanceCreditSpecificationItem'
    { _sicsiInstanceId =
        Nothing
    }

-- | The ID of the instance.
sicsiInstanceId :: Lens' SuccessfulInstanceCreditSpecificationItem (Maybe Text)
sicsiInstanceId = lens _sicsiInstanceId (\s a -> s {_sicsiInstanceId = a})

instance FromXML SuccessfulInstanceCreditSpecificationItem where
  parseXML x =
    SuccessfulInstanceCreditSpecificationItem'
      <$> (x .@? "instanceId")

instance Hashable SuccessfulInstanceCreditSpecificationItem

instance NFData SuccessfulInstanceCreditSpecificationItem
