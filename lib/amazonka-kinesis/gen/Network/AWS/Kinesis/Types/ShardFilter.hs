{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardFilter where

import Network.AWS.Kinesis.Types.ShardFilterType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'shardFilter' smart constructor.
data ShardFilter = ShardFilter'
  { _sfTimestamp :: !(Maybe POSIX),
    _sfShardId :: !(Maybe Text),
    _sfType :: !ShardFilterType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShardFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfTimestamp' - Undocumented member.
--
-- * 'sfShardId' - Undocumented member.
--
-- * 'sfType' - Undocumented member.
shardFilter ::
  -- | 'sfType'
  ShardFilterType ->
  ShardFilter
shardFilter pType_ =
  ShardFilter'
    { _sfTimestamp = Nothing,
      _sfShardId = Nothing,
      _sfType = pType_
    }

-- | Undocumented member.
sfTimestamp :: Lens' ShardFilter (Maybe UTCTime)
sfTimestamp = lens _sfTimestamp (\s a -> s {_sfTimestamp = a}) . mapping _Time

-- | Undocumented member.
sfShardId :: Lens' ShardFilter (Maybe Text)
sfShardId = lens _sfShardId (\s a -> s {_sfShardId = a})

-- | Undocumented member.
sfType :: Lens' ShardFilter ShardFilterType
sfType = lens _sfType (\s a -> s {_sfType = a})

instance Hashable ShardFilter

instance NFData ShardFilter

instance ToJSON ShardFilter where
  toJSON ShardFilter' {..} =
    object
      ( catMaybes
          [ ("Timestamp" .=) <$> _sfTimestamp,
            ("ShardId" .=) <$> _sfShardId,
            Just ("Type" .= _sfType)
          ]
      )
