{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each @RunCommandTarget@ block can include only one key, but this key may specify multiple values.
--
--
--
-- /See:/ 'runCommandTarget' smart constructor.
data RunCommandTarget = RunCommandTarget'
  { _rctKey :: !Text,
    _rctValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RunCommandTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rctKey' - Can be either @tag:@ /tag-key/ or @InstanceIds@ .
--
-- * 'rctValues' - If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
runCommandTarget ::
  -- | 'rctKey'
  Text ->
  -- | 'rctValues'
  NonEmpty Text ->
  RunCommandTarget
runCommandTarget pKey_ pValues_ =
  RunCommandTarget'
    { _rctKey = pKey_,
      _rctValues = _List1 # pValues_
    }

-- | Can be either @tag:@ /tag-key/ or @InstanceIds@ .
rctKey :: Lens' RunCommandTarget Text
rctKey = lens _rctKey (\s a -> s {_rctKey = a})

-- | If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
rctValues :: Lens' RunCommandTarget (NonEmpty Text)
rctValues = lens _rctValues (\s a -> s {_rctValues = a}) . _List1

instance FromJSON RunCommandTarget where
  parseJSON =
    withObject
      "RunCommandTarget"
      (\x -> RunCommandTarget' <$> (x .: "Key") <*> (x .: "Values"))

instance Hashable RunCommandTarget

instance NFData RunCommandTarget

instance ToJSON RunCommandTarget where
  toJSON RunCommandTarget' {..} =
    object
      ( catMaybes
          [Just ("Key" .= _rctKey), Just ("Values" .= _rctValues)]
      )
