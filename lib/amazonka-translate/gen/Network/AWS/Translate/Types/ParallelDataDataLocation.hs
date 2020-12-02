{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataDataLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate.
--
--
--
-- /See:/ 'parallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { _pddlRepositoryType ::
      !Text,
    _pddlLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParallelDataDataLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pddlRepositoryType' - Describes the repository that contains the parallel data input file.
--
-- * 'pddlLocation' - The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
parallelDataDataLocation ::
  -- | 'pddlRepositoryType'
  Text ->
  -- | 'pddlLocation'
  Text ->
  ParallelDataDataLocation
parallelDataDataLocation pRepositoryType_ pLocation_ =
  ParallelDataDataLocation'
    { _pddlRepositoryType = pRepositoryType_,
      _pddlLocation = pLocation_
    }

-- | Describes the repository that contains the parallel data input file.
pddlRepositoryType :: Lens' ParallelDataDataLocation Text
pddlRepositoryType = lens _pddlRepositoryType (\s a -> s {_pddlRepositoryType = a})

-- | The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
pddlLocation :: Lens' ParallelDataDataLocation Text
pddlLocation = lens _pddlLocation (\s a -> s {_pddlLocation = a})

instance FromJSON ParallelDataDataLocation where
  parseJSON =
    withObject
      "ParallelDataDataLocation"
      ( \x ->
          ParallelDataDataLocation'
            <$> (x .: "RepositoryType") <*> (x .: "Location")
      )

instance Hashable ParallelDataDataLocation

instance NFData ParallelDataDataLocation
