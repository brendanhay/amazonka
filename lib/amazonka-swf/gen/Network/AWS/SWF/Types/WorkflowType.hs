{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a workflow type.
--
--
--
-- /See:/ 'workflowType' smart constructor.
data WorkflowType = WorkflowType'
  { _wtName :: !Text,
    _wtVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtName' - The name of the workflow type.
--
-- * 'wtVersion' - The version of the workflow type.
workflowType ::
  -- | 'wtName'
  Text ->
  -- | 'wtVersion'
  Text ->
  WorkflowType
workflowType pName_ pVersion_ =
  WorkflowType' {_wtName = pName_, _wtVersion = pVersion_}

-- | The name of the workflow type.
wtName :: Lens' WorkflowType Text
wtName = lens _wtName (\s a -> s {_wtName = a})

-- | The version of the workflow type.
wtVersion :: Lens' WorkflowType Text
wtVersion = lens _wtVersion (\s a -> s {_wtVersion = a})

instance FromJSON WorkflowType where
  parseJSON =
    withObject
      "WorkflowType"
      (\x -> WorkflowType' <$> (x .: "name") <*> (x .: "version"))

instance Hashable WorkflowType

instance NFData WorkflowType

instance ToJSON WorkflowType where
  toJSON WorkflowType' {..} =
    object
      ( catMaybes
          [Just ("name" .= _wtName), Just ("version" .= _wtVersion)]
      )
