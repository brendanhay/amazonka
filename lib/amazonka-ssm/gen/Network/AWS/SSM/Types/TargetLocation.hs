{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.TargetLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.TargetLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
--
--
-- /See:/ 'targetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { _tlAccounts ::
      !(Maybe (List1 Text)),
    _tlTargetLocationMaxConcurrency :: !(Maybe Text),
    _tlTargetLocationMaxErrors :: !(Maybe Text),
    _tlRegions :: !(Maybe (List1 Text)),
    _tlExecutionRoleName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlAccounts' - The AWS accounts targeted by the current Automation execution.
--
-- * 'tlTargetLocationMaxConcurrency' - The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently
--
-- * 'tlTargetLocationMaxErrors' - The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation.
--
-- * 'tlRegions' - The AWS Regions targeted by the current Automation execution.
--
-- * 'tlExecutionRoleName' - The Automation execution role used by the currently running Automation.
targetLocation ::
  TargetLocation
targetLocation =
  TargetLocation'
    { _tlAccounts = Nothing,
      _tlTargetLocationMaxConcurrency = Nothing,
      _tlTargetLocationMaxErrors = Nothing,
      _tlRegions = Nothing,
      _tlExecutionRoleName = Nothing
    }

-- | The AWS accounts targeted by the current Automation execution.
tlAccounts :: Lens' TargetLocation (Maybe (NonEmpty Text))
tlAccounts = lens _tlAccounts (\s a -> s {_tlAccounts = a}) . mapping _List1

-- | The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently
tlTargetLocationMaxConcurrency :: Lens' TargetLocation (Maybe Text)
tlTargetLocationMaxConcurrency = lens _tlTargetLocationMaxConcurrency (\s a -> s {_tlTargetLocationMaxConcurrency = a})

-- | The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation.
tlTargetLocationMaxErrors :: Lens' TargetLocation (Maybe Text)
tlTargetLocationMaxErrors = lens _tlTargetLocationMaxErrors (\s a -> s {_tlTargetLocationMaxErrors = a})

-- | The AWS Regions targeted by the current Automation execution.
tlRegions :: Lens' TargetLocation (Maybe (NonEmpty Text))
tlRegions = lens _tlRegions (\s a -> s {_tlRegions = a}) . mapping _List1

-- | The Automation execution role used by the currently running Automation.
tlExecutionRoleName :: Lens' TargetLocation (Maybe Text)
tlExecutionRoleName = lens _tlExecutionRoleName (\s a -> s {_tlExecutionRoleName = a})

instance FromJSON TargetLocation where
  parseJSON =
    withObject
      "TargetLocation"
      ( \x ->
          TargetLocation'
            <$> (x .:? "Accounts")
            <*> (x .:? "TargetLocationMaxConcurrency")
            <*> (x .:? "TargetLocationMaxErrors")
            <*> (x .:? "Regions")
            <*> (x .:? "ExecutionRoleName")
      )

instance Hashable TargetLocation

instance NFData TargetLocation

instance ToJSON TargetLocation where
  toJSON TargetLocation' {..} =
    object
      ( catMaybes
          [ ("Accounts" .=) <$> _tlAccounts,
            ("TargetLocationMaxConcurrency" .=)
              <$> _tlTargetLocationMaxConcurrency,
            ("TargetLocationMaxErrors" .=) <$> _tlTargetLocationMaxErrors,
            ("Regions" .=) <$> _tlRegions,
            ("ExecutionRoleName" .=) <$> _tlExecutionRoleName
          ]
      )
