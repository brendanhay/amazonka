{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryNameIdPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryNameIdPair where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a repository name and ID.
--
--
--
-- /See:/ 'repositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { _rnipRepositoryId ::
      !(Maybe Text),
    _rnipRepositoryName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RepositoryNameIdPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnipRepositoryId' - The ID associated with the repository.
--
-- * 'rnipRepositoryName' - The name associated with the repository.
repositoryNameIdPair ::
  RepositoryNameIdPair
repositoryNameIdPair =
  RepositoryNameIdPair'
    { _rnipRepositoryId = Nothing,
      _rnipRepositoryName = Nothing
    }

-- | The ID associated with the repository.
rnipRepositoryId :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryId = lens _rnipRepositoryId (\s a -> s {_rnipRepositoryId = a})

-- | The name associated with the repository.
rnipRepositoryName :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryName = lens _rnipRepositoryName (\s a -> s {_rnipRepositoryName = a})

instance FromJSON RepositoryNameIdPair where
  parseJSON =
    withObject
      "RepositoryNameIdPair"
      ( \x ->
          RepositoryNameIdPair'
            <$> (x .:? "repositoryId") <*> (x .:? "repositoryName")
      )

instance Hashable RepositoryNameIdPair

instance NFData RepositoryNameIdPair
