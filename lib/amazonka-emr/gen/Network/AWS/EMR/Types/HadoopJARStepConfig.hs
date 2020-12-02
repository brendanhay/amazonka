{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.HadoopJARStepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.HadoopJARStepConfig where

import Network.AWS.EMR.Types.KeyValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.
--
--
--
-- /See:/ 'hadoopJARStepConfig' smart constructor.
data HadoopJARStepConfig = HadoopJARStepConfig'
  { _hjscArgs ::
      !(Maybe [Text]),
    _hjscMainClass :: !(Maybe Text),
    _hjscProperties :: !(Maybe [KeyValue]),
    _hjscJAR :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HadoopJARStepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hjscArgs' - A list of command line arguments passed to the JAR file's main function when executed.
--
-- * 'hjscMainClass' - The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
--
-- * 'hjscProperties' - A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
--
-- * 'hjscJAR' - A path to a JAR file run during the step.
hadoopJARStepConfig ::
  -- | 'hjscJAR'
  Text ->
  HadoopJARStepConfig
hadoopJARStepConfig pJAR_ =
  HadoopJARStepConfig'
    { _hjscArgs = Nothing,
      _hjscMainClass = Nothing,
      _hjscProperties = Nothing,
      _hjscJAR = pJAR_
    }

-- | A list of command line arguments passed to the JAR file's main function when executed.
hjscArgs :: Lens' HadoopJARStepConfig [Text]
hjscArgs = lens _hjscArgs (\s a -> s {_hjscArgs = a}) . _Default . _Coerce

-- | The name of the main class in the specified Java file. If not specified, the JAR file should specify a Main-Class in its manifest file.
hjscMainClass :: Lens' HadoopJARStepConfig (Maybe Text)
hjscMainClass = lens _hjscMainClass (\s a -> s {_hjscMainClass = a})

-- | A list of Java properties that are set when the step runs. You can use these properties to pass key value pairs to your main function.
hjscProperties :: Lens' HadoopJARStepConfig [KeyValue]
hjscProperties = lens _hjscProperties (\s a -> s {_hjscProperties = a}) . _Default . _Coerce

-- | A path to a JAR file run during the step.
hjscJAR :: Lens' HadoopJARStepConfig Text
hjscJAR = lens _hjscJAR (\s a -> s {_hjscJAR = a})

instance Hashable HadoopJARStepConfig

instance NFData HadoopJARStepConfig

instance ToJSON HadoopJARStepConfig where
  toJSON HadoopJARStepConfig' {..} =
    object
      ( catMaybes
          [ ("Args" .=) <$> _hjscArgs,
            ("MainClass" .=) <$> _hjscMainClass,
            ("Properties" .=) <$> _hjscProperties,
            Just ("Jar" .= _hjscJAR)
          ]
      )
