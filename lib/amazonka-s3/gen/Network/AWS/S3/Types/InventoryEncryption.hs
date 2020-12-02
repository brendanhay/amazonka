{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryEncryption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SSEKMS
import Network.AWS.S3.Types.SSES3

-- | Contains the type of server-side encryption used to encrypt the inventory results.
--
--
--
-- /See:/ 'inventoryEncryption' smart constructor.
data InventoryEncryption = InventoryEncryption'
  { _ieSSES3 ::
      !(Maybe SSES3),
    _ieSSEKMS :: !(Maybe SSEKMS)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieSSES3' - Specifies the use of SSE-S3 to encrypt delivered inventory reports.
--
-- * 'ieSSEKMS' - Specifies the use of SSE-KMS to encrypt delivered inventory reports.
inventoryEncryption ::
  InventoryEncryption
inventoryEncryption =
  InventoryEncryption' {_ieSSES3 = Nothing, _ieSSEKMS = Nothing}

-- | Specifies the use of SSE-S3 to encrypt delivered inventory reports.
ieSSES3 :: Lens' InventoryEncryption (Maybe SSES3)
ieSSES3 = lens _ieSSES3 (\s a -> s {_ieSSES3 = a})

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
ieSSEKMS :: Lens' InventoryEncryption (Maybe SSEKMS)
ieSSEKMS = lens _ieSSEKMS (\s a -> s {_ieSSEKMS = a})

instance FromXML InventoryEncryption where
  parseXML x =
    InventoryEncryption' <$> (x .@? "SSE-S3") <*> (x .@? "SSE-KMS")

instance Hashable InventoryEncryption

instance NFData InventoryEncryption

instance ToXML InventoryEncryption where
  toXML InventoryEncryption' {..} =
    mconcat ["SSE-S3" @= _ieSSES3, "SSE-KMS" @= _ieSSEKMS]
