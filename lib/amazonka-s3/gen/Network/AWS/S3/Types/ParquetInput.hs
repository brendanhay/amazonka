{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ParquetInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ParquetInput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container for Parquet.
--
--
--
-- /See:/ 'parquetInput' smart constructor.
data ParquetInput = ParquetInput'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParquetInput' with the minimum fields required to make a request.
parquetInput ::
  ParquetInput
parquetInput = ParquetInput'

instance Hashable ParquetInput

instance NFData ParquetInput

instance ToXML ParquetInput where
  toXML = const mempty
