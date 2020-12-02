{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.
--
--
--
-- /See:/ 'workflowTypeFilter' smart constructor.
data WorkflowTypeFilter = WorkflowTypeFilter'
  { _wtfVersion ::
      !(Maybe Text),
    _wtfName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowTypeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtfVersion' - Version of the workflow type.
--
-- * 'wtfName' - Name of the workflow type.
workflowTypeFilter ::
  -- | 'wtfName'
  Text ->
  WorkflowTypeFilter
workflowTypeFilter pName_ =
  WorkflowTypeFilter' {_wtfVersion = Nothing, _wtfName = pName_}

-- | Version of the workflow type.
wtfVersion :: Lens' WorkflowTypeFilter (Maybe Text)
wtfVersion = lens _wtfVersion (\s a -> s {_wtfVersion = a})

-- | Name of the workflow type.
wtfName :: Lens' WorkflowTypeFilter Text
wtfName = lens _wtfName (\s a -> s {_wtfName = a})

instance Hashable WorkflowTypeFilter

instance NFData WorkflowTypeFilter

instance ToJSON WorkflowTypeFilter where
  toJSON WorkflowTypeFilter' {..} =
    object
      ( catMaybes
          [("version" .=) <$> _wtfVersion, Just ("name" .= _wtfName)]
      )
