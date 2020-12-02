{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the notebook instance lifecycle configuration script.
--
--
-- Each lifecycle configuration script has a limit of 16384 characters.
--
-- The value of the @> PATH@ environment variable that is available to both scripts is @/sbin:bin:/usr/sbin:/usr/bin@ .
--
-- View CloudWatch Logs for notebook instance lifecycle configurations in log group @/aws/sagemaker/NotebookInstances@ in log stream @[notebook-instance-name]/[LifecycleConfigHook]@ .
--
-- Lifecycle configuration scripts cannot run for longer than 5 minutes. If a script runs for longer than 5 minutes, it fails and the notebook instance is not created or started.
--
-- For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
--
-- /See:/ 'notebookInstanceLifecycleHook' smart constructor.
newtype NotebookInstanceLifecycleHook = NotebookInstanceLifecycleHook'
  { _nilhContent ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotebookInstanceLifecycleHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nilhContent' - A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
notebookInstanceLifecycleHook ::
  NotebookInstanceLifecycleHook
notebookInstanceLifecycleHook =
  NotebookInstanceLifecycleHook' {_nilhContent = Nothing}

-- | A base64-encoded string that contains a shell script for a notebook instance lifecycle configuration.
nilhContent :: Lens' NotebookInstanceLifecycleHook (Maybe Text)
nilhContent = lens _nilhContent (\s a -> s {_nilhContent = a})

instance FromJSON NotebookInstanceLifecycleHook where
  parseJSON =
    withObject
      "NotebookInstanceLifecycleHook"
      (\x -> NotebookInstanceLifecycleHook' <$> (x .:? "Content"))

instance Hashable NotebookInstanceLifecycleHook

instance NFData NotebookInstanceLifecycleHook

instance ToJSON NotebookInstanceLifecycleHook where
  toJSON NotebookInstanceLifecycleHook' {..} =
    object (catMaybes [("Content" .=) <$> _nilhContent])
