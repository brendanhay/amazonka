{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOperations where

import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the file operation conflicts in a merge operation.
--
--
--
-- /See:/ 'mergeOperations' smart constructor.
data MergeOperations = MergeOperations'
  { _moDestination ::
      !(Maybe ChangeTypeEnum),
    _moSource :: !(Maybe ChangeTypeEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MergeOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'moDestination' - The operation on a file in the destination of a merge or pull request.
--
-- * 'moSource' - The operation (add, modify, or delete) on a file in the source of a merge or pull request.
mergeOperations ::
  MergeOperations
mergeOperations =
  MergeOperations' {_moDestination = Nothing, _moSource = Nothing}

-- | The operation on a file in the destination of a merge or pull request.
moDestination :: Lens' MergeOperations (Maybe ChangeTypeEnum)
moDestination = lens _moDestination (\s a -> s {_moDestination = a})

-- | The operation (add, modify, or delete) on a file in the source of a merge or pull request.
moSource :: Lens' MergeOperations (Maybe ChangeTypeEnum)
moSource = lens _moSource (\s a -> s {_moSource = a})

instance FromJSON MergeOperations where
  parseJSON =
    withObject
      "MergeOperations"
      ( \x ->
          MergeOperations' <$> (x .:? "destination") <*> (x .:? "source")
      )

instance Hashable MergeOperations

instance NFData MergeOperations
