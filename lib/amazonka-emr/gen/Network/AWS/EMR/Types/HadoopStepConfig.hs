{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.HadoopStepConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
--
--
-- /See:/ 'hadoopStepConfig' smart constructor.
data HadoopStepConfig = HadoopStepConfig'
  { _hscArgs ::
      !(Maybe [Text]),
    _hscJAR :: !(Maybe Text),
    _hscMainClass :: !(Maybe Text),
    _hscProperties :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HadoopStepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hscArgs' - The list of command line arguments to pass to the JAR file's main function for execution.
--
-- * 'hscJAR' - The path to the JAR file that runs during the step.
--
-- * 'hscMainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
--
-- * 'hscProperties' - The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
hadoopStepConfig ::
  HadoopStepConfig
hadoopStepConfig =
  HadoopStepConfig'
    { _hscArgs = Nothing,
      _hscJAR = Nothing,
      _hscMainClass = Nothing,
      _hscProperties = Nothing
    }

-- | The list of command line arguments to pass to the JAR file's main function for execution.
hscArgs :: Lens' HadoopStepConfig [Text]
hscArgs = lens _hscArgs (\s a -> s {_hscArgs = a}) . _Default . _Coerce

-- | The path to the JAR file that runs during the step.
hscJAR :: Lens' HadoopStepConfig (Maybe Text)
hscJAR = lens _hscJAR (\s a -> s {_hscJAR = a})

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a main class in its manifest file.
hscMainClass :: Lens' HadoopStepConfig (Maybe Text)
hscMainClass = lens _hscMainClass (\s a -> s {_hscMainClass = a})

-- | The list of Java properties that are set when the step runs. You can use these properties to pass key-value pairs to your main function.
hscProperties :: Lens' HadoopStepConfig (HashMap Text (Text))
hscProperties = lens _hscProperties (\s a -> s {_hscProperties = a}) . _Default . _Map

instance FromJSON HadoopStepConfig where
  parseJSON =
    withObject
      "HadoopStepConfig"
      ( \x ->
          HadoopStepConfig'
            <$> (x .:? "Args" .!= mempty)
            <*> (x .:? "Jar")
            <*> (x .:? "MainClass")
            <*> (x .:? "Properties" .!= mempty)
      )

instance Hashable HadoopStepConfig

instance NFData HadoopStepConfig
