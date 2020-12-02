{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.GitSubmodulesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.GitSubmodulesConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Git submodules configuration for an AWS CodeBuild build project.
--
--
--
-- /See:/ 'gitSubmodulesConfig' smart constructor.
newtype GitSubmodulesConfig = GitSubmodulesConfig'
  { _gscFetchSubmodules ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GitSubmodulesConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gscFetchSubmodules' - Set to true to fetch Git submodules for your AWS CodeBuild build project.
gitSubmodulesConfig ::
  -- | 'gscFetchSubmodules'
  Bool ->
  GitSubmodulesConfig
gitSubmodulesConfig pFetchSubmodules_ =
  GitSubmodulesConfig' {_gscFetchSubmodules = pFetchSubmodules_}

-- | Set to true to fetch Git submodules for your AWS CodeBuild build project.
gscFetchSubmodules :: Lens' GitSubmodulesConfig Bool
gscFetchSubmodules = lens _gscFetchSubmodules (\s a -> s {_gscFetchSubmodules = a})

instance FromJSON GitSubmodulesConfig where
  parseJSON =
    withObject
      "GitSubmodulesConfig"
      (\x -> GitSubmodulesConfig' <$> (x .: "fetchSubmodules"))

instance Hashable GitSubmodulesConfig

instance NFData GitSubmodulesConfig

instance ToJSON GitSubmodulesConfig where
  toJSON GitSubmodulesConfig' {..} =
    object
      (catMaybes [Just ("fetchSubmodules" .= _gscFetchSubmodules)])
