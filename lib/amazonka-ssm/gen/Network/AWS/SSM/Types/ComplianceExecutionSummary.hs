{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceExecutionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
--
--
-- /See:/ 'complianceExecutionSummary' smart constructor.
data ComplianceExecutionSummary = ComplianceExecutionSummary'
  { _cesExecutionId ::
      !(Maybe Text),
    _cesExecutionType :: !(Maybe Text),
    _cesExecutionTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceExecutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesExecutionId' - An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
--
-- * 'cesExecutionType' - The type of execution. For example, @Command@ is a valid execution type.
--
-- * 'cesExecutionTime' - The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
complianceExecutionSummary ::
  -- | 'cesExecutionTime'
  UTCTime ->
  ComplianceExecutionSummary
complianceExecutionSummary pExecutionTime_ =
  ComplianceExecutionSummary'
    { _cesExecutionId = Nothing,
      _cesExecutionType = Nothing,
      _cesExecutionTime = _Time # pExecutionTime_
    }

-- | An ID created by the system when @PutComplianceItems@ was called. For example, @CommandID@ is a valid execution ID. You can use this ID in subsequent calls.
cesExecutionId :: Lens' ComplianceExecutionSummary (Maybe Text)
cesExecutionId = lens _cesExecutionId (\s a -> s {_cesExecutionId = a})

-- | The type of execution. For example, @Command@ is a valid execution type.
cesExecutionType :: Lens' ComplianceExecutionSummary (Maybe Text)
cesExecutionType = lens _cesExecutionType (\s a -> s {_cesExecutionType = a})

-- | The time the execution ran as a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
cesExecutionTime :: Lens' ComplianceExecutionSummary UTCTime
cesExecutionTime = lens _cesExecutionTime (\s a -> s {_cesExecutionTime = a}) . _Time

instance FromJSON ComplianceExecutionSummary where
  parseJSON =
    withObject
      "ComplianceExecutionSummary"
      ( \x ->
          ComplianceExecutionSummary'
            <$> (x .:? "ExecutionId")
            <*> (x .:? "ExecutionType")
            <*> (x .: "ExecutionTime")
      )

instance Hashable ComplianceExecutionSummary

instance NFData ComplianceExecutionSummary

instance ToJSON ComplianceExecutionSummary where
  toJSON ComplianceExecutionSummary' {..} =
    object
      ( catMaybes
          [ ("ExecutionId" .=) <$> _cesExecutionId,
            ("ExecutionType" .=) <$> _cesExecutionType,
            Just ("ExecutionTime" .= _cesExecutionTime)
          ]
      )
