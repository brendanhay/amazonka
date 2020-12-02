{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.WorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.WorkerBlock where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The WorkerBlock data structure represents a Worker who has been blocked. It has two elements: the WorkerId and the Reason for the block.
--
--
--
-- /See:/ 'workerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { _wbReason :: !(Maybe Text),
    _wbWorkerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkerBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbReason' - A message explaining the reason the Worker was blocked.
--
-- * 'wbWorkerId' - The ID of the Worker who accepted the HIT.
workerBlock ::
  WorkerBlock
workerBlock =
  WorkerBlock' {_wbReason = Nothing, _wbWorkerId = Nothing}

-- | A message explaining the reason the Worker was blocked.
wbReason :: Lens' WorkerBlock (Maybe Text)
wbReason = lens _wbReason (\s a -> s {_wbReason = a})

-- | The ID of the Worker who accepted the HIT.
wbWorkerId :: Lens' WorkerBlock (Maybe Text)
wbWorkerId = lens _wbWorkerId (\s a -> s {_wbWorkerId = a})

instance FromJSON WorkerBlock where
  parseJSON =
    withObject
      "WorkerBlock"
      (\x -> WorkerBlock' <$> (x .:? "Reason") <*> (x .:? "WorkerId"))

instance Hashable WorkerBlock

instance NFData WorkerBlock
