{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.SourceRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.SourceRevision where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the version (or revision) of a source artifact that initiated a pipeline execution.
--
--
--
-- /See:/ 'sourceRevision' smart constructor.
data SourceRevision = SourceRevision'
  { _srRevisionSummary ::
      !(Maybe Text),
    _srRevisionURL :: !(Maybe Text),
    _srRevisionId :: !(Maybe Text),
    _srActionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srRevisionSummary' - Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- * 'srRevisionURL' - The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- * 'srRevisionId' - The system-generated unique ID that identifies the revision number of the artifact.
--
-- * 'srActionName' - The name of the action that processed the revision to the source artifact.
sourceRevision ::
  -- | 'srActionName'
  Text ->
  SourceRevision
sourceRevision pActionName_ =
  SourceRevision'
    { _srRevisionSummary = Nothing,
      _srRevisionURL = Nothing,
      _srRevisionId = Nothing,
      _srActionName = pActionName_
    }

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
srRevisionSummary :: Lens' SourceRevision (Maybe Text)
srRevisionSummary = lens _srRevisionSummary (\s a -> s {_srRevisionSummary = a})

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
srRevisionURL :: Lens' SourceRevision (Maybe Text)
srRevisionURL = lens _srRevisionURL (\s a -> s {_srRevisionURL = a})

-- | The system-generated unique ID that identifies the revision number of the artifact.
srRevisionId :: Lens' SourceRevision (Maybe Text)
srRevisionId = lens _srRevisionId (\s a -> s {_srRevisionId = a})

-- | The name of the action that processed the revision to the source artifact.
srActionName :: Lens' SourceRevision Text
srActionName = lens _srActionName (\s a -> s {_srActionName = a})

instance FromJSON SourceRevision where
  parseJSON =
    withObject
      "SourceRevision"
      ( \x ->
          SourceRevision'
            <$> (x .:? "revisionSummary")
            <*> (x .:? "revisionUrl")
            <*> (x .:? "revisionId")
            <*> (x .: "actionName")
      )

instance Hashable SourceRevision

instance NFData SourceRevision
