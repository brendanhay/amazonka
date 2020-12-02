{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildNotDeleted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildNotDeleted where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a build that could not be successfully deleted.
--
--
--
-- /See:/ 'buildNotDeleted' smart constructor.
data BuildNotDeleted = BuildNotDeleted'
  { _bndId :: !(Maybe Text),
    _bndStatusCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildNotDeleted' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bndId' - The ID of the build that could not be successfully deleted.
--
-- * 'bndStatusCode' - Additional information about the build that could not be successfully deleted.
buildNotDeleted ::
  BuildNotDeleted
buildNotDeleted =
  BuildNotDeleted' {_bndId = Nothing, _bndStatusCode = Nothing}

-- | The ID of the build that could not be successfully deleted.
bndId :: Lens' BuildNotDeleted (Maybe Text)
bndId = lens _bndId (\s a -> s {_bndId = a})

-- | Additional information about the build that could not be successfully deleted.
bndStatusCode :: Lens' BuildNotDeleted (Maybe Text)
bndStatusCode = lens _bndStatusCode (\s a -> s {_bndStatusCode = a})

instance FromJSON BuildNotDeleted where
  parseJSON =
    withObject
      "BuildNotDeleted"
      (\x -> BuildNotDeleted' <$> (x .:? "id") <*> (x .:? "statusCode"))

instance Hashable BuildNotDeleted

instance NFData BuildNotDeleted
