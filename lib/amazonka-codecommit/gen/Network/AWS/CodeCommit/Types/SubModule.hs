{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SubModule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SubModule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a submodule reference in a repository folder.
--
--
--
-- /See:/ 'subModule' smart constructor.
data SubModule = SubModule'
  { _smCommitId :: !(Maybe Text),
    _smAbsolutePath :: !(Maybe Text),
    _smRelativePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubModule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smCommitId' - The commit ID that contains the reference to the submodule.
--
-- * 'smAbsolutePath' - The fully qualified path to the folder that contains the reference to the submodule.
--
-- * 'smRelativePath' - The relative path of the submodule from the folder where the query originated.
subModule ::
  SubModule
subModule =
  SubModule'
    { _smCommitId = Nothing,
      _smAbsolutePath = Nothing,
      _smRelativePath = Nothing
    }

-- | The commit ID that contains the reference to the submodule.
smCommitId :: Lens' SubModule (Maybe Text)
smCommitId = lens _smCommitId (\s a -> s {_smCommitId = a})

-- | The fully qualified path to the folder that contains the reference to the submodule.
smAbsolutePath :: Lens' SubModule (Maybe Text)
smAbsolutePath = lens _smAbsolutePath (\s a -> s {_smAbsolutePath = a})

-- | The relative path of the submodule from the folder where the query originated.
smRelativePath :: Lens' SubModule (Maybe Text)
smRelativePath = lens _smRelativePath (\s a -> s {_smRelativePath = a})

instance FromJSON SubModule where
  parseJSON =
    withObject
      "SubModule"
      ( \x ->
          SubModule'
            <$> (x .:? "commitId")
            <*> (x .:? "absolutePath")
            <*> (x .:? "relativePath")
      )

instance Hashable SubModule

instance NFData SubModule
