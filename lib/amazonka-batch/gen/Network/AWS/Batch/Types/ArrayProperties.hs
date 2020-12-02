{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch array job.
--
--
--
-- /See:/ 'arrayProperties' smart constructor.
newtype ArrayProperties = ArrayProperties' {_apSize :: Maybe Int}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArrayProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apSize' - The size of the array job.
arrayProperties ::
  ArrayProperties
arrayProperties = ArrayProperties' {_apSize = Nothing}

-- | The size of the array job.
apSize :: Lens' ArrayProperties (Maybe Int)
apSize = lens _apSize (\s a -> s {_apSize = a})

instance Hashable ArrayProperties

instance NFData ArrayProperties

instance ToJSON ArrayProperties where
  toJSON ArrayProperties' {..} =
    object (catMaybes [("size" .=) <$> _apSize])
