{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeCommitCodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeCommitCodeDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
--
--
-- /See:/ 'codeCommitCodeDestination' smart constructor.
newtype CodeCommitCodeDestination = CodeCommitCodeDestination'
  { _cccdName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeCommitCodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccdName' - The name of the AWS CodeCommit repository to be created in AWS CodeStar.
codeCommitCodeDestination ::
  -- | 'cccdName'
  Text ->
  CodeCommitCodeDestination
codeCommitCodeDestination pName_ =
  CodeCommitCodeDestination' {_cccdName = pName_}

-- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
cccdName :: Lens' CodeCommitCodeDestination Text
cccdName = lens _cccdName (\s a -> s {_cccdName = a})

instance Hashable CodeCommitCodeDestination

instance NFData CodeCommitCodeDestination

instance ToJSON CodeCommitCodeDestination where
  toJSON CodeCommitCodeDestination' {..} =
    object (catMaybes [Just ("name" .= _cccdName)])
