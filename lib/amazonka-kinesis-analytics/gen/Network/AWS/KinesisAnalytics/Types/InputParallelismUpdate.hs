{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides updates to the parallelism count.
--
--
--
-- /See:/ 'inputParallelismUpdate' smart constructor.
newtype InputParallelismUpdate = InputParallelismUpdate'
  { _ipuCountUpdate ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputParallelismUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipuCountUpdate' - Number of in-application streams to create for the specified streaming source.
inputParallelismUpdate ::
  InputParallelismUpdate
inputParallelismUpdate =
  InputParallelismUpdate' {_ipuCountUpdate = Nothing}

-- | Number of in-application streams to create for the specified streaming source.
ipuCountUpdate :: Lens' InputParallelismUpdate (Maybe Natural)
ipuCountUpdate = lens _ipuCountUpdate (\s a -> s {_ipuCountUpdate = a}) . mapping _Nat

instance Hashable InputParallelismUpdate

instance NFData InputParallelismUpdate

instance ToJSON InputParallelismUpdate where
  toJSON InputParallelismUpdate' {..} =
    object (catMaybes [("CountUpdate" .=) <$> _ipuCountUpdate])
