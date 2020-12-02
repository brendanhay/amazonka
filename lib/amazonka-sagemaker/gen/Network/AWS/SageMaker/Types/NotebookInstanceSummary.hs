{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.InstanceType
import Network.AWS.SageMaker.Types.NotebookInstanceStatus

-- | Provides summary information for an Amazon SageMaker notebook instance.
--
--
--
-- /See:/ 'notebookInstanceSummary' smart constructor.
data NotebookInstanceSummary = NotebookInstanceSummary'
  { _nisCreationTime ::
      !(Maybe POSIX),
    _nisAdditionalCodeRepositories ::
      !(Maybe [Text]),
    _nisURL :: !(Maybe Text),
    _nisLastModifiedTime :: !(Maybe POSIX),
    _nisInstanceType :: !(Maybe InstanceType),
    _nisNotebookInstanceStatus ::
      !(Maybe NotebookInstanceStatus),
    _nisDefaultCodeRepository :: !(Maybe Text),
    _nisNotebookInstanceLifecycleConfigName ::
      !(Maybe Text),
    _nisNotebookInstanceName :: !Text,
    _nisNotebookInstanceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotebookInstanceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nisCreationTime' - A timestamp that shows when the notebook instance was created.
--
-- * 'nisAdditionalCodeRepositories' - An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'nisURL' - The URL that you use to connect to the Jupyter instance running in your notebook instance.
--
-- * 'nisLastModifiedTime' - A timestamp that shows when the notebook instance was last modified.
--
-- * 'nisInstanceType' - The type of ML compute instance that the notebook instance is running on.
--
-- * 'nisNotebookInstanceStatus' - The status of the notebook instance.
--
-- * 'nisDefaultCodeRepository' - The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- * 'nisNotebookInstanceLifecycleConfigName' - The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- * 'nisNotebookInstanceName' - The name of the notebook instance that you want a summary for.
--
-- * 'nisNotebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
notebookInstanceSummary ::
  -- | 'nisNotebookInstanceName'
  Text ->
  -- | 'nisNotebookInstanceARN'
  Text ->
  NotebookInstanceSummary
notebookInstanceSummary
  pNotebookInstanceName_
  pNotebookInstanceARN_ =
    NotebookInstanceSummary'
      { _nisCreationTime = Nothing,
        _nisAdditionalCodeRepositories = Nothing,
        _nisURL = Nothing,
        _nisLastModifiedTime = Nothing,
        _nisInstanceType = Nothing,
        _nisNotebookInstanceStatus = Nothing,
        _nisDefaultCodeRepository = Nothing,
        _nisNotebookInstanceLifecycleConfigName = Nothing,
        _nisNotebookInstanceName = pNotebookInstanceName_,
        _nisNotebookInstanceARN = pNotebookInstanceARN_
      }

-- | A timestamp that shows when the notebook instance was created.
nisCreationTime :: Lens' NotebookInstanceSummary (Maybe UTCTime)
nisCreationTime = lens _nisCreationTime (\s a -> s {_nisCreationTime = a}) . mapping _Time

-- | An array of up to three Git repositories associated with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
nisAdditionalCodeRepositories :: Lens' NotebookInstanceSummary [Text]
nisAdditionalCodeRepositories = lens _nisAdditionalCodeRepositories (\s a -> s {_nisAdditionalCodeRepositories = a}) . _Default . _Coerce

-- | The URL that you use to connect to the Jupyter instance running in your notebook instance.
nisURL :: Lens' NotebookInstanceSummary (Maybe Text)
nisURL = lens _nisURL (\s a -> s {_nisURL = a})

-- | A timestamp that shows when the notebook instance was last modified.
nisLastModifiedTime :: Lens' NotebookInstanceSummary (Maybe UTCTime)
nisLastModifiedTime = lens _nisLastModifiedTime (\s a -> s {_nisLastModifiedTime = a}) . mapping _Time

-- | The type of ML compute instance that the notebook instance is running on.
nisInstanceType :: Lens' NotebookInstanceSummary (Maybe InstanceType)
nisInstanceType = lens _nisInstanceType (\s a -> s {_nisInstanceType = a})

-- | The status of the notebook instance.
nisNotebookInstanceStatus :: Lens' NotebookInstanceSummary (Maybe NotebookInstanceStatus)
nisNotebookInstanceStatus = lens _nisNotebookInstanceStatus (\s a -> s {_nisNotebookInstanceStatus = a})

-- | The Git repository associated with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
nisDefaultCodeRepository :: Lens' NotebookInstanceSummary (Maybe Text)
nisDefaultCodeRepository = lens _nisDefaultCodeRepository (\s a -> s {_nisDefaultCodeRepository = a})

-- | The name of a notebook instance lifecycle configuration associated with this notebook instance. For information about notebook instance lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
nisNotebookInstanceLifecycleConfigName :: Lens' NotebookInstanceSummary (Maybe Text)
nisNotebookInstanceLifecycleConfigName = lens _nisNotebookInstanceLifecycleConfigName (\s a -> s {_nisNotebookInstanceLifecycleConfigName = a})

-- | The name of the notebook instance that you want a summary for.
nisNotebookInstanceName :: Lens' NotebookInstanceSummary Text
nisNotebookInstanceName = lens _nisNotebookInstanceName (\s a -> s {_nisNotebookInstanceName = a})

-- | The Amazon Resource Name (ARN) of the notebook instance.
nisNotebookInstanceARN :: Lens' NotebookInstanceSummary Text
nisNotebookInstanceARN = lens _nisNotebookInstanceARN (\s a -> s {_nisNotebookInstanceARN = a})

instance FromJSON NotebookInstanceSummary where
  parseJSON =
    withObject
      "NotebookInstanceSummary"
      ( \x ->
          NotebookInstanceSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "AdditionalCodeRepositories" .!= mempty)
            <*> (x .:? "Url")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "InstanceType")
            <*> (x .:? "NotebookInstanceStatus")
            <*> (x .:? "DefaultCodeRepository")
            <*> (x .:? "NotebookInstanceLifecycleConfigName")
            <*> (x .: "NotebookInstanceName")
            <*> (x .: "NotebookInstanceArn")
      )

instance Hashable NotebookInstanceSummary

instance NFData NotebookInstanceSummary
