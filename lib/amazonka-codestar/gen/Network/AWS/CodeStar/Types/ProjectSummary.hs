{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ProjectSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the metadata for a project.
--
--
--
-- /See:/ 'projectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { _psProjectARN ::
      !(Maybe Text),
    _psProjectId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psProjectARN' - The Amazon Resource Name (ARN) of the project.
--
-- * 'psProjectId' - The ID of the project.
projectSummary ::
  ProjectSummary
projectSummary =
  ProjectSummary' {_psProjectARN = Nothing, _psProjectId = Nothing}

-- | The Amazon Resource Name (ARN) of the project.
psProjectARN :: Lens' ProjectSummary (Maybe Text)
psProjectARN = lens _psProjectARN (\s a -> s {_psProjectARN = a})

-- | The ID of the project.
psProjectId :: Lens' ProjectSummary (Maybe Text)
psProjectId = lens _psProjectId (\s a -> s {_psProjectId = a})

instance FromJSON ProjectSummary where
  parseJSON =
    withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary' <$> (x .:? "projectArn") <*> (x .:? "projectId")
      )

instance Hashable ProjectSummary

instance NFData ProjectSummary
