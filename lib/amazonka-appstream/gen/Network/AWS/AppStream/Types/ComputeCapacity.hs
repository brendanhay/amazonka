{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the capacity for a fleet.
--
--
--
-- /See:/ 'computeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
  { _ccDesiredInstances ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccDesiredInstances' - The desired number of streaming instances.
computeCapacity ::
  -- | 'ccDesiredInstances'
  Int ->
  ComputeCapacity
computeCapacity pDesiredInstances_ =
  ComputeCapacity' {_ccDesiredInstances = pDesiredInstances_}

-- | The desired number of streaming instances.
ccDesiredInstances :: Lens' ComputeCapacity Int
ccDesiredInstances = lens _ccDesiredInstances (\s a -> s {_ccDesiredInstances = a})

instance Hashable ComputeCapacity

instance NFData ComputeCapacity

instance ToJSON ComputeCapacity where
  toJSON ComputeCapacity' {..} =
    object
      (catMaybes [Just ("DesiredInstances" .= _ccDesiredInstances)])
