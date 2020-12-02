{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowTypeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.RegistrationStatus
import Network.AWS.SWF.Types.WorkflowType

-- | Contains information about a workflow type.
--
--
--
-- /See:/ 'workflowTypeInfo' smart constructor.
data WorkflowTypeInfo = WorkflowTypeInfo'
  { _wtiDeprecationDate ::
      !(Maybe POSIX),
    _wtiDescription :: !(Maybe Text),
    _wtiWorkflowType :: !WorkflowType,
    _wtiStatus :: !RegistrationStatus,
    _wtiCreationDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowTypeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtiDeprecationDate' - If the type is in deprecated state, then it is set to the date when the type was deprecated.
--
-- * 'wtiDescription' - The description of the type registered through 'RegisterWorkflowType' .
--
-- * 'wtiWorkflowType' - The workflow type this information is about.
--
-- * 'wtiStatus' - The current status of the workflow type.
--
-- * 'wtiCreationDate' - The date when this type was registered.
workflowTypeInfo ::
  -- | 'wtiWorkflowType'
  WorkflowType ->
  -- | 'wtiStatus'
  RegistrationStatus ->
  -- | 'wtiCreationDate'
  UTCTime ->
  WorkflowTypeInfo
workflowTypeInfo pWorkflowType_ pStatus_ pCreationDate_ =
  WorkflowTypeInfo'
    { _wtiDeprecationDate = Nothing,
      _wtiDescription = Nothing,
      _wtiWorkflowType = pWorkflowType_,
      _wtiStatus = pStatus_,
      _wtiCreationDate = _Time # pCreationDate_
    }

-- | If the type is in deprecated state, then it is set to the date when the type was deprecated.
wtiDeprecationDate :: Lens' WorkflowTypeInfo (Maybe UTCTime)
wtiDeprecationDate = lens _wtiDeprecationDate (\s a -> s {_wtiDeprecationDate = a}) . mapping _Time

-- | The description of the type registered through 'RegisterWorkflowType' .
wtiDescription :: Lens' WorkflowTypeInfo (Maybe Text)
wtiDescription = lens _wtiDescription (\s a -> s {_wtiDescription = a})

-- | The workflow type this information is about.
wtiWorkflowType :: Lens' WorkflowTypeInfo WorkflowType
wtiWorkflowType = lens _wtiWorkflowType (\s a -> s {_wtiWorkflowType = a})

-- | The current status of the workflow type.
wtiStatus :: Lens' WorkflowTypeInfo RegistrationStatus
wtiStatus = lens _wtiStatus (\s a -> s {_wtiStatus = a})

-- | The date when this type was registered.
wtiCreationDate :: Lens' WorkflowTypeInfo UTCTime
wtiCreationDate = lens _wtiCreationDate (\s a -> s {_wtiCreationDate = a}) . _Time

instance FromJSON WorkflowTypeInfo where
  parseJSON =
    withObject
      "WorkflowTypeInfo"
      ( \x ->
          WorkflowTypeInfo'
            <$> (x .:? "deprecationDate")
            <*> (x .:? "description")
            <*> (x .: "workflowType")
            <*> (x .: "status")
            <*> (x .: "creationDate")
      )

instance Hashable WorkflowTypeInfo

instance NFData WorkflowTypeInfo
