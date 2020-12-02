{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Code
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Code where

import Network.AWS.CodeStar.Types.CodeDestination
import Network.AWS.CodeStar.Types.CodeSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Location and destination information about the source code files provided with the project request. The source code is uploaded to the new project source repository after project creation.
--
--
--
-- /See:/ 'code' smart constructor.
data Code = Code'
  { _cSource :: !CodeSource,
    _cDestination :: !CodeDestination
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Code' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSource' - The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- * 'cDestination' - The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
code ::
  -- | 'cSource'
  CodeSource ->
  -- | 'cDestination'
  CodeDestination ->
  Code
code pSource_ pDestination_ =
  Code' {_cSource = pSource_, _cDestination = pDestination_}

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
cSource :: Lens' Code CodeSource
cSource = lens _cSource (\s a -> s {_cSource = a})

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
cDestination :: Lens' Code CodeDestination
cDestination = lens _cDestination (\s a -> s {_cDestination = a})

instance Hashable Code

instance NFData Code

instance ToJSON Code where
  toJSON Code' {..} =
    object
      ( catMaybes
          [ Just ("source" .= _cSource),
            Just ("destination" .= _cDestination)
          ]
      )
