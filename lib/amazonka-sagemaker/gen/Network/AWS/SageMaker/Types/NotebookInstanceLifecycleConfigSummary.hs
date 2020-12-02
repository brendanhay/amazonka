{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of a notebook instance lifecycle configuration.
--
--
--
-- /See:/ 'notebookInstanceLifecycleConfigSummary' smart constructor.
data NotebookInstanceLifecycleConfigSummary = NotebookInstanceLifecycleConfigSummary'
  { _nilcsCreationTime ::
      !( Maybe
           POSIX
       ),
    _nilcsLastModifiedTime ::
      !( Maybe
           POSIX
       ),
    _nilcsNotebookInstanceLifecycleConfigName ::
      !Text,
    _nilcsNotebookInstanceLifecycleConfigARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotebookInstanceLifecycleConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nilcsCreationTime' - A timestamp that tells when the lifecycle configuration was created.
--
-- * 'nilcsLastModifiedTime' - A timestamp that tells when the lifecycle configuration was last modified.
--
-- * 'nilcsNotebookInstanceLifecycleConfigName' - The name of the lifecycle configuration.
--
-- * 'nilcsNotebookInstanceLifecycleConfigARN' - The Amazon Resource Name (ARN) of the lifecycle configuration.
notebookInstanceLifecycleConfigSummary ::
  -- | 'nilcsNotebookInstanceLifecycleConfigName'
  Text ->
  -- | 'nilcsNotebookInstanceLifecycleConfigARN'
  Text ->
  NotebookInstanceLifecycleConfigSummary
notebookInstanceLifecycleConfigSummary
  pNotebookInstanceLifecycleConfigName_
  pNotebookInstanceLifecycleConfigARN_ =
    NotebookInstanceLifecycleConfigSummary'
      { _nilcsCreationTime =
          Nothing,
        _nilcsLastModifiedTime = Nothing,
        _nilcsNotebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_,
        _nilcsNotebookInstanceLifecycleConfigARN =
          pNotebookInstanceLifecycleConfigARN_
      }

-- | A timestamp that tells when the lifecycle configuration was created.
nilcsCreationTime :: Lens' NotebookInstanceLifecycleConfigSummary (Maybe UTCTime)
nilcsCreationTime = lens _nilcsCreationTime (\s a -> s {_nilcsCreationTime = a}) . mapping _Time

-- | A timestamp that tells when the lifecycle configuration was last modified.
nilcsLastModifiedTime :: Lens' NotebookInstanceLifecycleConfigSummary (Maybe UTCTime)
nilcsLastModifiedTime = lens _nilcsLastModifiedTime (\s a -> s {_nilcsLastModifiedTime = a}) . mapping _Time

-- | The name of the lifecycle configuration.
nilcsNotebookInstanceLifecycleConfigName :: Lens' NotebookInstanceLifecycleConfigSummary Text
nilcsNotebookInstanceLifecycleConfigName = lens _nilcsNotebookInstanceLifecycleConfigName (\s a -> s {_nilcsNotebookInstanceLifecycleConfigName = a})

-- | The Amazon Resource Name (ARN) of the lifecycle configuration.
nilcsNotebookInstanceLifecycleConfigARN :: Lens' NotebookInstanceLifecycleConfigSummary Text
nilcsNotebookInstanceLifecycleConfigARN = lens _nilcsNotebookInstanceLifecycleConfigARN (\s a -> s {_nilcsNotebookInstanceLifecycleConfigARN = a})

instance FromJSON NotebookInstanceLifecycleConfigSummary where
  parseJSON =
    withObject
      "NotebookInstanceLifecycleConfigSummary"
      ( \x ->
          NotebookInstanceLifecycleConfigSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastModifiedTime")
            <*> (x .: "NotebookInstanceLifecycleConfigName")
            <*> (x .: "NotebookInstanceLifecycleConfigArn")
      )

instance Hashable NotebookInstanceLifecycleConfigSummary

instance NFData NotebookInstanceLifecycleConfigSummary
