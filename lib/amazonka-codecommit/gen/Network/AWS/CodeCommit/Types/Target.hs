{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Target where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a target for a pull request.
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tDestinationReference :: !(Maybe Text),
    _tRepositoryName :: !Text,
    _tSourceReference :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDestinationReference' - The branch of the repository where the pull request changes are merged. Also known as the destination branch.
--
-- * 'tRepositoryName' - The name of the repository that contains the pull request.
--
-- * 'tSourceReference' - The branch of the repository that contains the changes for the pull request. Also known as the source branch.
target ::
  -- | 'tRepositoryName'
  Text ->
  -- | 'tSourceReference'
  Text ->
  Target
target pRepositoryName_ pSourceReference_ =
  Target'
    { _tDestinationReference = Nothing,
      _tRepositoryName = pRepositoryName_,
      _tSourceReference = pSourceReference_
    }

-- | The branch of the repository where the pull request changes are merged. Also known as the destination branch.
tDestinationReference :: Lens' Target (Maybe Text)
tDestinationReference = lens _tDestinationReference (\s a -> s {_tDestinationReference = a})

-- | The name of the repository that contains the pull request.
tRepositoryName :: Lens' Target Text
tRepositoryName = lens _tRepositoryName (\s a -> s {_tRepositoryName = a})

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
tSourceReference :: Lens' Target Text
tSourceReference = lens _tSourceReference (\s a -> s {_tSourceReference = a})

instance Hashable Target

instance NFData Target

instance ToJSON Target where
  toJSON Target' {..} =
    object
      ( catMaybes
          [ ("destinationReference" .=) <$> _tDestinationReference,
            Just ("repositoryName" .= _tRepositoryName),
            Just ("sourceReference" .= _tSourceReference)
          ]
      )
