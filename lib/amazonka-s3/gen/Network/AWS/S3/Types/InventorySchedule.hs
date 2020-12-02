{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventorySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventorySchedule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryFrequency

-- | Specifies the schedule for generating inventory results.
--
--
--
-- /See:/ 'inventorySchedule' smart constructor.
newtype InventorySchedule = InventorySchedule'
  { _isFrequency ::
      InventoryFrequency
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventorySchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isFrequency' - Specifies how frequently inventory results are produced.
inventorySchedule ::
  -- | 'isFrequency'
  InventoryFrequency ->
  InventorySchedule
inventorySchedule pFrequency_ =
  InventorySchedule' {_isFrequency = pFrequency_}

-- | Specifies how frequently inventory results are produced.
isFrequency :: Lens' InventorySchedule InventoryFrequency
isFrequency = lens _isFrequency (\s a -> s {_isFrequency = a})

instance FromXML InventorySchedule where
  parseXML x = InventorySchedule' <$> (x .@ "Frequency")

instance Hashable InventorySchedule

instance NFData InventorySchedule

instance ToXML InventorySchedule where
  toXML InventorySchedule' {..} =
    mconcat ["Frequency" @= _isFrequency]
