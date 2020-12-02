{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MemoryInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the memory for the instance type.
--
--
--
-- /See:/ 'memoryInfo' smart constructor.
newtype MemoryInfo = MemoryInfo' {_miSizeInMiB :: Maybe Integer}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MemoryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miSizeInMiB' - The size of the memory, in MiB.
memoryInfo ::
  MemoryInfo
memoryInfo = MemoryInfo' {_miSizeInMiB = Nothing}

-- | The size of the memory, in MiB.
miSizeInMiB :: Lens' MemoryInfo (Maybe Integer)
miSizeInMiB = lens _miSizeInMiB (\s a -> s {_miSizeInMiB = a})

instance FromXML MemoryInfo where
  parseXML x = MemoryInfo' <$> (x .@? "sizeInMiB")

instance Hashable MemoryInfo

instance NFData MemoryInfo
