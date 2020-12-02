{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UniqueProblem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UniqueProblem where

import Network.AWS.DeviceFarm.Types.Problem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of one or more problems, grouped by their result.
--
--
--
-- /See:/ 'uniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
  { _upProblems ::
      !(Maybe [Problem]),
    _upMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UniqueProblem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upProblems' - Information about the problems.
--
-- * 'upMessage' - A message about the unique problems' result.
uniqueProblem ::
  UniqueProblem
uniqueProblem =
  UniqueProblem' {_upProblems = Nothing, _upMessage = Nothing}

-- | Information about the problems.
upProblems :: Lens' UniqueProblem [Problem]
upProblems = lens _upProblems (\s a -> s {_upProblems = a}) . _Default . _Coerce

-- | A message about the unique problems' result.
upMessage :: Lens' UniqueProblem (Maybe Text)
upMessage = lens _upMessage (\s a -> s {_upMessage = a})

instance FromJSON UniqueProblem where
  parseJSON =
    withObject
      "UniqueProblem"
      ( \x ->
          UniqueProblem'
            <$> (x .:? "problems" .!= mempty) <*> (x .:? "message")
      )

instance Hashable UniqueProblem

instance NFData UniqueProblem
