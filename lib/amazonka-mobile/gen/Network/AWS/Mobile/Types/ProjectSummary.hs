{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about an AWS Mobile Hub project.
--
--
--
-- /See:/ 'projectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { _psName :: !(Maybe Text),
    _psProjectId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psName' - Name of the project.
--
-- * 'psProjectId' - Unique project identifier.
projectSummary ::
  ProjectSummary
projectSummary =
  ProjectSummary' {_psName = Nothing, _psProjectId = Nothing}

-- | Name of the project.
psName :: Lens' ProjectSummary (Maybe Text)
psName = lens _psName (\s a -> s {_psName = a})

-- | Unique project identifier.
psProjectId :: Lens' ProjectSummary (Maybe Text)
psProjectId = lens _psProjectId (\s a -> s {_psProjectId = a})

instance FromJSON ProjectSummary where
  parseJSON =
    withObject
      "ProjectSummary"
      (\x -> ProjectSummary' <$> (x .:? "name") <*> (x .:? "projectId"))

instance Hashable ProjectSummary

instance NFData ProjectSummary
