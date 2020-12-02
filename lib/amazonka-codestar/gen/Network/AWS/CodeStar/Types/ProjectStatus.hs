{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ProjectStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An indication of whether a project creation or deletion is failed or successful.
--
--
--
-- /See:/ 'projectStatus' smart constructor.
data ProjectStatus = ProjectStatus'
  { _psReason :: !(Maybe Text),
    _psState :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psReason' - In the case of a project creation or deletion failure, a reason for the failure.
--
-- * 'psState' - The phase of completion for a project creation or deletion.
projectStatus ::
  -- | 'psState'
  Text ->
  ProjectStatus
projectStatus pState_ =
  ProjectStatus' {_psReason = Nothing, _psState = pState_}

-- | In the case of a project creation or deletion failure, a reason for the failure.
psReason :: Lens' ProjectStatus (Maybe Text)
psReason = lens _psReason (\s a -> s {_psReason = a})

-- | The phase of completion for a project creation or deletion.
psState :: Lens' ProjectStatus Text
psState = lens _psState (\s a -> s {_psState = a})

instance FromJSON ProjectStatus where
  parseJSON =
    withObject
      "ProjectStatus"
      (\x -> ProjectStatus' <$> (x .:? "reason") <*> (x .: "state"))

instance Hashable ProjectStatus

instance NFData ProjectStatus
