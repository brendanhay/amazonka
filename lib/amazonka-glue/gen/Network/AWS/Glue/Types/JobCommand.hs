{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobCommand where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies code executed when a job is run.
--
--
--
-- /See:/ 'jobCommand' smart constructor.
data JobCommand = JobCommand'
  { _jobScriptLocation :: !(Maybe Text),
    _jobPythonVersion :: !(Maybe Text),
    _jobName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobScriptLocation' - Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
--
-- * 'jobPythonVersion' - The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
--
-- * 'jobName' - The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
jobCommand ::
  JobCommand
jobCommand =
  JobCommand'
    { _jobScriptLocation = Nothing,
      _jobPythonVersion = Nothing,
      _jobName = Nothing
    }

-- | Specifies the Amazon Simple Storage Service (Amazon S3) path to a script that executes a job.
jobScriptLocation :: Lens' JobCommand (Maybe Text)
jobScriptLocation = lens _jobScriptLocation (\s a -> s {_jobScriptLocation = a})

-- | The Python version being used to execute a Python shell job. Allowed values are 2 or 3.
jobPythonVersion :: Lens' JobCommand (Maybe Text)
jobPythonVersion = lens _jobPythonVersion (\s a -> s {_jobPythonVersion = a})

-- | The name of the job command. For an Apache Spark ETL job, this must be @glueetl@ . For a Python shell job, it must be @pythonshell@ . For an Apache Spark streaming ETL job, this must be @gluestreaming@ .
jobName :: Lens' JobCommand (Maybe Text)
jobName = lens _jobName (\s a -> s {_jobName = a})

instance FromJSON JobCommand where
  parseJSON =
    withObject
      "JobCommand"
      ( \x ->
          JobCommand'
            <$> (x .:? "ScriptLocation")
            <*> (x .:? "PythonVersion")
            <*> (x .:? "Name")
      )

instance Hashable JobCommand

instance NFData JobCommand

instance ToJSON JobCommand where
  toJSON JobCommand' {..} =
    object
      ( catMaybes
          [ ("ScriptLocation" .=) <$> _jobScriptLocation,
            ("PythonVersion" .=) <$> _jobPythonVersion,
            ("Name" .=) <$> _jobName
          ]
      )
