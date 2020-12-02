{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the burstable performance instance whose credit option for CPU usage was not modified.
--
--
--
-- /See:/ 'unsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { _uicsiInstanceId ::
      !( Maybe
           Text
       ),
    _uicsiError ::
      !( Maybe
           UnsuccessfulInstanceCreditSpecificationItemError
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uicsiInstanceId' - The ID of the instance.
--
-- * 'uicsiError' - The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
unsuccessfulInstanceCreditSpecificationItem ::
  UnsuccessfulInstanceCreditSpecificationItem
unsuccessfulInstanceCreditSpecificationItem =
  UnsuccessfulInstanceCreditSpecificationItem'
    { _uicsiInstanceId =
        Nothing,
      _uicsiError = Nothing
    }

-- | The ID of the instance.
uicsiInstanceId :: Lens' UnsuccessfulInstanceCreditSpecificationItem (Maybe Text)
uicsiInstanceId = lens _uicsiInstanceId (\s a -> s {_uicsiInstanceId = a})

-- | The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
uicsiError :: Lens' UnsuccessfulInstanceCreditSpecificationItem (Maybe UnsuccessfulInstanceCreditSpecificationItemError)
uicsiError = lens _uicsiError (\s a -> s {_uicsiError = a})

instance FromXML UnsuccessfulInstanceCreditSpecificationItem where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItem'
      <$> (x .@? "instanceId") <*> (x .@? "error")

instance Hashable UnsuccessfulInstanceCreditSpecificationItem

instance NFData UnsuccessfulInstanceCreditSpecificationItem
