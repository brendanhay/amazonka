{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Limits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'limits' smart constructor.
data Limits = Limits'
  { _lMaximumReplicationCount :: !Nat,
    _lMaximumPartitionCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMaximumReplicationCount' - Undocumented member.
--
-- * 'lMaximumPartitionCount' - Undocumented member.
limits ::
  -- | 'lMaximumReplicationCount'
  Natural ->
  -- | 'lMaximumPartitionCount'
  Natural ->
  Limits
limits pMaximumReplicationCount_ pMaximumPartitionCount_ =
  Limits'
    { _lMaximumReplicationCount =
        _Nat # pMaximumReplicationCount_,
      _lMaximumPartitionCount = _Nat # pMaximumPartitionCount_
    }

-- | Undocumented member.
lMaximumReplicationCount :: Lens' Limits Natural
lMaximumReplicationCount = lens _lMaximumReplicationCount (\s a -> s {_lMaximumReplicationCount = a}) . _Nat

-- | Undocumented member.
lMaximumPartitionCount :: Lens' Limits Natural
lMaximumPartitionCount = lens _lMaximumPartitionCount (\s a -> s {_lMaximumPartitionCount = a}) . _Nat

instance FromXML Limits where
  parseXML x =
    Limits'
      <$> (x .@ "MaximumReplicationCount") <*> (x .@ "MaximumPartitionCount")

instance Hashable Limits

instance NFData Limits
