{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactRevision where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents revision details of an artifact.
--
--
--
-- /See:/ 'artifactRevision' smart constructor.
data ArtifactRevision = ArtifactRevision'
  { _arRevisionSummary ::
      !(Maybe Text),
    _arRevisionURL :: !(Maybe Text),
    _arCreated :: !(Maybe POSIX),
    _arName :: !(Maybe Text),
    _arRevisionId :: !(Maybe Text),
    _arRevisionChangeIdentifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArtifactRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arRevisionSummary' - Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- * 'arRevisionURL' - The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- * 'arCreated' - The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- * 'arName' - The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
--
-- * 'arRevisionId' - The revision ID of the artifact.
--
-- * 'arRevisionChangeIdentifier' - An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
artifactRevision ::
  ArtifactRevision
artifactRevision =
  ArtifactRevision'
    { _arRevisionSummary = Nothing,
      _arRevisionURL = Nothing,
      _arCreated = Nothing,
      _arName = Nothing,
      _arRevisionId = Nothing,
      _arRevisionChangeIdentifier = Nothing
    }

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
arRevisionSummary :: Lens' ArtifactRevision (Maybe Text)
arRevisionSummary = lens _arRevisionSummary (\s a -> s {_arRevisionSummary = a})

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
arRevisionURL :: Lens' ArtifactRevision (Maybe Text)
arRevisionURL = lens _arRevisionURL (\s a -> s {_arRevisionURL = a})

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
arCreated :: Lens' ArtifactRevision (Maybe UTCTime)
arCreated = lens _arCreated (\s a -> s {_arCreated = a}) . mapping _Time

-- | The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
arName :: Lens' ArtifactRevision (Maybe Text)
arName = lens _arName (\s a -> s {_arName = a})

-- | The revision ID of the artifact.
arRevisionId :: Lens' ArtifactRevision (Maybe Text)
arRevisionId = lens _arRevisionId (\s a -> s {_arRevisionId = a})

-- | An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
arRevisionChangeIdentifier :: Lens' ArtifactRevision (Maybe Text)
arRevisionChangeIdentifier = lens _arRevisionChangeIdentifier (\s a -> s {_arRevisionChangeIdentifier = a})

instance FromJSON ArtifactRevision where
  parseJSON =
    withObject
      "ArtifactRevision"
      ( \x ->
          ArtifactRevision'
            <$> (x .:? "revisionSummary")
            <*> (x .:? "revisionUrl")
            <*> (x .:? "created")
            <*> (x .:? "name")
            <*> (x .:? "revisionId")
            <*> (x .:? "revisionChangeIdentifier")
      )

instance Hashable ArtifactRevision

instance NFData ArtifactRevision
