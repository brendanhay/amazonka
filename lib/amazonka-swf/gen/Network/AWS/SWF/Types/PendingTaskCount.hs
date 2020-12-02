{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.PendingTaskCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.PendingTaskCount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the count of tasks in a task list.
--
--
--
-- /See:/ 'pendingTaskCount' smart constructor.
data PendingTaskCount = PendingTaskCount'
  { _ptcTruncated ::
      !(Maybe Bool),
    _ptcCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingTaskCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptcTruncated' - If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- * 'ptcCount' - The number of tasks in the task list.
pendingTaskCount ::
  -- | 'ptcCount'
  Natural ->
  PendingTaskCount
pendingTaskCount pCount_ =
  PendingTaskCount'
    { _ptcTruncated = Nothing,
      _ptcCount = _Nat # pCount_
    }

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
ptcTruncated :: Lens' PendingTaskCount (Maybe Bool)
ptcTruncated = lens _ptcTruncated (\s a -> s {_ptcTruncated = a})

-- | The number of tasks in the task list.
ptcCount :: Lens' PendingTaskCount Natural
ptcCount = lens _ptcCount (\s a -> s {_ptcCount = a}) . _Nat

instance FromJSON PendingTaskCount where
  parseJSON =
    withObject
      "PendingTaskCount"
      ( \x ->
          PendingTaskCount' <$> (x .:? "truncated") <*> (x .: "count")
      )

instance Hashable PendingTaskCount

instance NFData PendingTaskCount
