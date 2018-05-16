{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Product where

import Network.AWS.CodeCommit.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about a specific Git blob object.
--
--
--
-- /See:/ 'blobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { _bmPath   :: !(Maybe Text)
  , _bmMode   :: !(Maybe Text)
  , _bmBlobId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlobMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmPath' - The path to the blob and any associated file name, if any.
--
-- * 'bmMode' - The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
--
-- * 'bmBlobId' - The full ID of the blob.
blobMetadata
    :: BlobMetadata
blobMetadata =
  BlobMetadata' {_bmPath = Nothing, _bmMode = Nothing, _bmBlobId = Nothing}


-- | The path to the blob and any associated file name, if any.
bmPath :: Lens' BlobMetadata (Maybe Text)
bmPath = lens _bmPath (\ s a -> s{_bmPath = a})

-- | The file mode permissions of the blob. File mode permission codes include:     * @100644@ indicates read/write     * @100755@ indicates read/write/execute     * @160000@ indicates a submodule     * @120000@ indicates a symlink
bmMode :: Lens' BlobMetadata (Maybe Text)
bmMode = lens _bmMode (\ s a -> s{_bmMode = a})

-- | The full ID of the blob.
bmBlobId :: Lens' BlobMetadata (Maybe Text)
bmBlobId = lens _bmBlobId (\ s a -> s{_bmBlobId = a})

instance FromJSON BlobMetadata where
        parseJSON
          = withObject "BlobMetadata"
              (\ x ->
                 BlobMetadata' <$>
                   (x .:? "path") <*> (x .:? "mode") <*>
                     (x .:? "blobId"))

instance Hashable BlobMetadata where

instance NFData BlobMetadata where

-- | Returns information about a branch.
--
--
--
-- /See:/ 'branchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { _biCommitId   :: !(Maybe Text)
  , _biBranchName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BranchInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biCommitId' - The ID of the last commit made to the branch.
--
-- * 'biBranchName' - The name of the branch.
branchInfo
    :: BranchInfo
branchInfo = BranchInfo' {_biCommitId = Nothing, _biBranchName = Nothing}


-- | The ID of the last commit made to the branch.
biCommitId :: Lens' BranchInfo (Maybe Text)
biCommitId = lens _biCommitId (\ s a -> s{_biCommitId = a})

-- | The name of the branch.
biBranchName :: Lens' BranchInfo (Maybe Text)
biBranchName = lens _biBranchName (\ s a -> s{_biBranchName = a})

instance FromJSON BranchInfo where
        parseJSON
          = withObject "BranchInfo"
              (\ x ->
                 BranchInfo' <$>
                   (x .:? "commitId") <*> (x .:? "branchName"))

instance Hashable BranchInfo where

instance NFData BranchInfo where

-- | Returns information about a specific comment.
--
--
--
-- /See:/ 'comment' smart constructor.
data Comment = Comment'
  { _cLastModifiedDate   :: !(Maybe POSIX)
  , _cAuthorARN          :: !(Maybe Text)
  , _cContent            :: !(Maybe Text)
  , _cCreationDate       :: !(Maybe POSIX)
  , _cDeleted            :: !(Maybe Bool)
  , _cClientRequestToken :: !(Maybe Text)
  , _cCommentId          :: !(Maybe Text)
  , _cInReplyTo          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastModifiedDate' - The date and time the comment was most recently modified, in timestamp format.
--
-- * 'cAuthorARN' - The Amazon Resource Name (ARN) of the person who posted the comment.
--
-- * 'cContent' - The content of the comment.
--
-- * 'cCreationDate' - The date and time the comment was created, in timestamp format.
--
-- * 'cDeleted' - A Boolean value indicating whether the comment has been deleted.
--
-- * 'cClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'cCommentId' - The system-generated comment ID.
--
-- * 'cInReplyTo' - The ID of the comment for which this comment is a reply, if any.
comment
    :: Comment
comment =
  Comment'
    { _cLastModifiedDate = Nothing
    , _cAuthorARN = Nothing
    , _cContent = Nothing
    , _cCreationDate = Nothing
    , _cDeleted = Nothing
    , _cClientRequestToken = Nothing
    , _cCommentId = Nothing
    , _cInReplyTo = Nothing
    }


-- | The date and time the comment was most recently modified, in timestamp format.
cLastModifiedDate :: Lens' Comment (Maybe UTCTime)
cLastModifiedDate = lens _cLastModifiedDate (\ s a -> s{_cLastModifiedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the person who posted the comment.
cAuthorARN :: Lens' Comment (Maybe Text)
cAuthorARN = lens _cAuthorARN (\ s a -> s{_cAuthorARN = a})

-- | The content of the comment.
cContent :: Lens' Comment (Maybe Text)
cContent = lens _cContent (\ s a -> s{_cContent = a})

-- | The date and time the comment was created, in timestamp format.
cCreationDate :: Lens' Comment (Maybe UTCTime)
cCreationDate = lens _cCreationDate (\ s a -> s{_cCreationDate = a}) . mapping _Time

-- | A Boolean value indicating whether the comment has been deleted.
cDeleted :: Lens' Comment (Maybe Bool)
cDeleted = lens _cDeleted (\ s a -> s{_cDeleted = a})

-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
cClientRequestToken :: Lens' Comment (Maybe Text)
cClientRequestToken = lens _cClientRequestToken (\ s a -> s{_cClientRequestToken = a})

-- | The system-generated comment ID.
cCommentId :: Lens' Comment (Maybe Text)
cCommentId = lens _cCommentId (\ s a -> s{_cCommentId = a})

-- | The ID of the comment for which this comment is a reply, if any.
cInReplyTo :: Lens' Comment (Maybe Text)
cInReplyTo = lens _cInReplyTo (\ s a -> s{_cInReplyTo = a})

instance FromJSON Comment where
        parseJSON
          = withObject "Comment"
              (\ x ->
                 Comment' <$>
                   (x .:? "lastModifiedDate") <*> (x .:? "authorArn")
                     <*> (x .:? "content")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "deleted")
                     <*> (x .:? "clientRequestToken")
                     <*> (x .:? "commentId")
                     <*> (x .:? "inReplyTo"))

instance Hashable Comment where

instance NFData Comment where

-- | Returns information about comments on the comparison between two commits.
--
--
--
-- /See:/ 'commentsForComparedCommit' smart constructor.
data CommentsForComparedCommit = CommentsForComparedCommit'
  { _cfccBeforeBlobId   :: !(Maybe Text)
  , _cfccLocation       :: !(Maybe Location)
  , _cfccAfterCommitId  :: !(Maybe Text)
  , _cfccAfterBlobId    :: !(Maybe Text)
  , _cfccBeforeCommitId :: !(Maybe Text)
  , _cfccRepositoryName :: !(Maybe Text)
  , _cfccComments       :: !(Maybe [Comment])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommentsForComparedCommit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfccBeforeBlobId' - The full blob ID of the commit used to establish the 'before' of the comparison.
--
-- * 'cfccLocation' - Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is 'BEFORE' or 'AFTER'.
--
-- * 'cfccAfterCommitId' - The full commit ID of the commit used to establish the 'after' of the comparison.
--
-- * 'cfccAfterBlobId' - The full blob ID of the commit used to establish the 'after' of the comparison.
--
-- * 'cfccBeforeCommitId' - The full commit ID of the commit used to establish the 'before' of the comparison.
--
-- * 'cfccRepositoryName' - The name of the repository that contains the compared commits.
--
-- * 'cfccComments' - An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
commentsForComparedCommit
    :: CommentsForComparedCommit
commentsForComparedCommit =
  CommentsForComparedCommit'
    { _cfccBeforeBlobId = Nothing
    , _cfccLocation = Nothing
    , _cfccAfterCommitId = Nothing
    , _cfccAfterBlobId = Nothing
    , _cfccBeforeCommitId = Nothing
    , _cfccRepositoryName = Nothing
    , _cfccComments = Nothing
    }


-- | The full blob ID of the commit used to establish the 'before' of the comparison.
cfccBeforeBlobId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccBeforeBlobId = lens _cfccBeforeBlobId (\ s a -> s{_cfccBeforeBlobId = a})

-- | Location information about the comment on the comparison, including the file name, line number, and whether the version of the file where the comment was made is 'BEFORE' or 'AFTER'.
cfccLocation :: Lens' CommentsForComparedCommit (Maybe Location)
cfccLocation = lens _cfccLocation (\ s a -> s{_cfccLocation = a})

-- | The full commit ID of the commit used to establish the 'after' of the comparison.
cfccAfterCommitId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccAfterCommitId = lens _cfccAfterCommitId (\ s a -> s{_cfccAfterCommitId = a})

-- | The full blob ID of the commit used to establish the 'after' of the comparison.
cfccAfterBlobId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccAfterBlobId = lens _cfccAfterBlobId (\ s a -> s{_cfccAfterBlobId = a})

-- | The full commit ID of the commit used to establish the 'before' of the comparison.
cfccBeforeCommitId :: Lens' CommentsForComparedCommit (Maybe Text)
cfccBeforeCommitId = lens _cfccBeforeCommitId (\ s a -> s{_cfccBeforeCommitId = a})

-- | The name of the repository that contains the compared commits.
cfccRepositoryName :: Lens' CommentsForComparedCommit (Maybe Text)
cfccRepositoryName = lens _cfccRepositoryName (\ s a -> s{_cfccRepositoryName = a})

-- | An array of comment objects. Each comment object contains information about a comment on the comparison between commits.
cfccComments :: Lens' CommentsForComparedCommit [Comment]
cfccComments = lens _cfccComments (\ s a -> s{_cfccComments = a}) . _Default . _Coerce

instance FromJSON CommentsForComparedCommit where
        parseJSON
          = withObject "CommentsForComparedCommit"
              (\ x ->
                 CommentsForComparedCommit' <$>
                   (x .:? "beforeBlobId") <*> (x .:? "location") <*>
                     (x .:? "afterCommitId")
                     <*> (x .:? "afterBlobId")
                     <*> (x .:? "beforeCommitId")
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "comments" .!= mempty))

instance Hashable CommentsForComparedCommit where

instance NFData CommentsForComparedCommit where

-- | Returns information about comments on a pull request.
--
--
--
-- /See:/ 'commentsForPullRequest' smart constructor.
data CommentsForPullRequest = CommentsForPullRequest'
  { _cfprBeforeBlobId   :: !(Maybe Text)
  , _cfprLocation       :: !(Maybe Location)
  , _cfprAfterCommitId  :: !(Maybe Text)
  , _cfprPullRequestId  :: !(Maybe Text)
  , _cfprAfterBlobId    :: !(Maybe Text)
  , _cfprBeforeCommitId :: !(Maybe Text)
  , _cfprRepositoryName :: !(Maybe Text)
  , _cfprComments       :: !(Maybe [Comment])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommentsForPullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfprBeforeBlobId' - The full blob ID of the file on which you want to comment on the destination commit.
--
-- * 'cfprLocation' - Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is 'BEFORE' (destination branch) or 'AFTER' (source branch).
--
-- * 'cfprAfterCommitId' - he full commit ID of the commit that was the tip of the source branch at the time the comment was made.
--
-- * 'cfprPullRequestId' - The system-generated ID of the pull request.
--
-- * 'cfprAfterBlobId' - The full blob ID of the file on which you want to comment on the source commit.
--
-- * 'cfprBeforeCommitId' - The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit will be superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
--
-- * 'cfprRepositoryName' - The name of the repository that contains the pull request.
--
-- * 'cfprComments' - An array of comment objects. Each comment object contains information about a comment on the pull request.
commentsForPullRequest
    :: CommentsForPullRequest
commentsForPullRequest =
  CommentsForPullRequest'
    { _cfprBeforeBlobId = Nothing
    , _cfprLocation = Nothing
    , _cfprAfterCommitId = Nothing
    , _cfprPullRequestId = Nothing
    , _cfprAfterBlobId = Nothing
    , _cfprBeforeCommitId = Nothing
    , _cfprRepositoryName = Nothing
    , _cfprComments = Nothing
    }


-- | The full blob ID of the file on which you want to comment on the destination commit.
cfprBeforeBlobId :: Lens' CommentsForPullRequest (Maybe Text)
cfprBeforeBlobId = lens _cfprBeforeBlobId (\ s a -> s{_cfprBeforeBlobId = a})

-- | Location information about the comment on the pull request, including the file name, line number, and whether the version of the file where the comment was made is 'BEFORE' (destination branch) or 'AFTER' (source branch).
cfprLocation :: Lens' CommentsForPullRequest (Maybe Location)
cfprLocation = lens _cfprLocation (\ s a -> s{_cfprLocation = a})

-- | he full commit ID of the commit that was the tip of the source branch at the time the comment was made.
cfprAfterCommitId :: Lens' CommentsForPullRequest (Maybe Text)
cfprAfterCommitId = lens _cfprAfterCommitId (\ s a -> s{_cfprAfterCommitId = a})

-- | The system-generated ID of the pull request.
cfprPullRequestId :: Lens' CommentsForPullRequest (Maybe Text)
cfprPullRequestId = lens _cfprPullRequestId (\ s a -> s{_cfprPullRequestId = a})

-- | The full blob ID of the file on which you want to comment on the source commit.
cfprAfterBlobId :: Lens' CommentsForPullRequest (Maybe Text)
cfprAfterBlobId = lens _cfprAfterBlobId (\ s a -> s{_cfprAfterBlobId = a})

-- | The full commit ID of the commit that was the tip of the destination branch when the pull request was created. This commit will be superceded by the after commit in the source branch when and if you merge the source branch into the destination branch.
cfprBeforeCommitId :: Lens' CommentsForPullRequest (Maybe Text)
cfprBeforeCommitId = lens _cfprBeforeCommitId (\ s a -> s{_cfprBeforeCommitId = a})

-- | The name of the repository that contains the pull request.
cfprRepositoryName :: Lens' CommentsForPullRequest (Maybe Text)
cfprRepositoryName = lens _cfprRepositoryName (\ s a -> s{_cfprRepositoryName = a})

-- | An array of comment objects. Each comment object contains information about a comment on the pull request.
cfprComments :: Lens' CommentsForPullRequest [Comment]
cfprComments = lens _cfprComments (\ s a -> s{_cfprComments = a}) . _Default . _Coerce

instance FromJSON CommentsForPullRequest where
        parseJSON
          = withObject "CommentsForPullRequest"
              (\ x ->
                 CommentsForPullRequest' <$>
                   (x .:? "beforeBlobId") <*> (x .:? "location") <*>
                     (x .:? "afterCommitId")
                     <*> (x .:? "pullRequestId")
                     <*> (x .:? "afterBlobId")
                     <*> (x .:? "beforeCommitId")
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "comments" .!= mempty))

instance Hashable CommentsForPullRequest where

instance NFData CommentsForPullRequest where

-- | Returns information about a specific commit.
--
--
--
-- /See:/ 'commit' smart constructor.
data Commit = Commit'
  { _cCommitId       :: !(Maybe Text)
  , _cCommitter      :: !(Maybe UserInfo)
  , _cTreeId         :: !(Maybe Text)
  , _cAdditionalData :: !(Maybe Text)
  , _cParents        :: !(Maybe [Text])
  , _cAuthor         :: !(Maybe UserInfo)
  , _cMessage        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Commit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCommitId' - The full SHA of the specified commit.
--
-- * 'cCommitter' - Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
--
-- * 'cTreeId' - Tree information for the specified commit.
--
-- * 'cAdditionalData' - Any additional data associated with the specified commit.
--
-- * 'cParents' - A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
--
-- * 'cAuthor' - Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
--
-- * 'cMessage' - The commit message associated with the specified commit.
commit
    :: Commit
commit =
  Commit'
    { _cCommitId = Nothing
    , _cCommitter = Nothing
    , _cTreeId = Nothing
    , _cAdditionalData = Nothing
    , _cParents = Nothing
    , _cAuthor = Nothing
    , _cMessage = Nothing
    }


-- | The full SHA of the specified commit.
cCommitId :: Lens' Commit (Maybe Text)
cCommitId = lens _cCommitId (\ s a -> s{_cCommitId = a})

-- | Information about the person who committed the specified commit, also known as the committer. Information includes the date in timestamp format with GMT offset, the name of the committer, and the email address for the committer, as configured in Git. For more information about the difference between an author and a committer in Git, see <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro Git by Scott Chacon and Ben Straub.
cCommitter :: Lens' Commit (Maybe UserInfo)
cCommitter = lens _cCommitter (\ s a -> s{_cCommitter = a})

-- | Tree information for the specified commit.
cTreeId :: Lens' Commit (Maybe Text)
cTreeId = lens _cTreeId (\ s a -> s{_cTreeId = a})

-- | Any additional data associated with the specified commit.
cAdditionalData :: Lens' Commit (Maybe Text)
cAdditionalData = lens _cAdditionalData (\ s a -> s{_cAdditionalData = a})

-- | A list of parent commits for the specified commit. Each parent commit ID is the full commit ID.
cParents :: Lens' Commit [Text]
cParents = lens _cParents (\ s a -> s{_cParents = a}) . _Default . _Coerce

-- | Information about the author of the specified commit. Information includes the date in timestamp format with GMT offset, the name of the author, and the email address for the author, as configured in Git.
cAuthor :: Lens' Commit (Maybe UserInfo)
cAuthor = lens _cAuthor (\ s a -> s{_cAuthor = a})

-- | The commit message associated with the specified commit.
cMessage :: Lens' Commit (Maybe Text)
cMessage = lens _cMessage (\ s a -> s{_cMessage = a})

instance FromJSON Commit where
        parseJSON
          = withObject "Commit"
              (\ x ->
                 Commit' <$>
                   (x .:? "commitId") <*> (x .:? "committer") <*>
                     (x .:? "treeId")
                     <*> (x .:? "additionalData")
                     <*> (x .:? "parents" .!= mempty)
                     <*> (x .:? "author")
                     <*> (x .:? "message"))

instance Hashable Commit where

instance NFData Commit where

-- | Returns information about a set of differences for a commit specifier.
--
--
--
-- /See:/ 'difference' smart constructor.
data Difference = Difference'
  { _dAfterBlob  :: !(Maybe BlobMetadata)
  , _dBeforeBlob :: !(Maybe BlobMetadata)
  , _dChangeType :: !(Maybe ChangeTypeEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Difference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAfterBlob' - Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dBeforeBlob' - Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- * 'dChangeType' - Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
difference
    :: Difference
difference =
  Difference'
    {_dAfterBlob = Nothing, _dBeforeBlob = Nothing, _dChangeType = Nothing}


-- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
dAfterBlob :: Lens' Difference (Maybe BlobMetadata)
dAfterBlob = lens _dAfterBlob (\ s a -> s{_dAfterBlob = a})

-- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
dBeforeBlob :: Lens' Difference (Maybe BlobMetadata)
dBeforeBlob = lens _dBeforeBlob (\ s a -> s{_dBeforeBlob = a})

-- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
dChangeType :: Lens' Difference (Maybe ChangeTypeEnum)
dChangeType = lens _dChangeType (\ s a -> s{_dChangeType = a})

instance FromJSON Difference where
        parseJSON
          = withObject "Difference"
              (\ x ->
                 Difference' <$>
                   (x .:? "afterBlob") <*> (x .:? "beforeBlob") <*>
                     (x .:? "changeType"))

instance Hashable Difference where

instance NFData Difference where

-- | Returns information about the location of a change or comment in the comparison between two commits or a pull request.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lRelativeFileVersion :: !(Maybe RelativeFileVersionEnum)
  , _lFilePath            :: !(Maybe Text)
  , _lFilePosition        :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lRelativeFileVersion' - In a comparison of commits or a pull request, whether the change is in the 'before' or 'after' of that comparison.
--
-- * 'lFilePath' - The name of the file being compared, including its extension and subdirectory, if any.
--
-- * 'lFilePosition' - The position of a change within a compared file, in line number format.
location
    :: Location
location =
  Location'
    { _lRelativeFileVersion = Nothing
    , _lFilePath = Nothing
    , _lFilePosition = Nothing
    }


-- | In a comparison of commits or a pull request, whether the change is in the 'before' or 'after' of that comparison.
lRelativeFileVersion :: Lens' Location (Maybe RelativeFileVersionEnum)
lRelativeFileVersion = lens _lRelativeFileVersion (\ s a -> s{_lRelativeFileVersion = a})

-- | The name of the file being compared, including its extension and subdirectory, if any.
lFilePath :: Lens' Location (Maybe Text)
lFilePath = lens _lFilePath (\ s a -> s{_lFilePath = a})

-- | The position of a change within a compared file, in line number format.
lFilePosition :: Lens' Location (Maybe Integer)
lFilePosition = lens _lFilePosition (\ s a -> s{_lFilePosition = a})

instance FromJSON Location where
        parseJSON
          = withObject "Location"
              (\ x ->
                 Location' <$>
                   (x .:? "relativeFileVersion") <*> (x .:? "filePath")
                     <*> (x .:? "filePosition"))

instance Hashable Location where

instance NFData Location where

instance ToJSON Location where
        toJSON Location'{..}
          = object
              (catMaybes
                 [("relativeFileVersion" .=) <$>
                    _lRelativeFileVersion,
                  ("filePath" .=) <$> _lFilePath,
                  ("filePosition" .=) <$> _lFilePosition])

-- | Returns information about a merge or potential merge between a source reference and a destination reference in a pull request.
--
--
--
-- /See:/ 'mergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { _mmMergedBy :: !(Maybe Text)
  , _mmIsMerged :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergeMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmMergedBy' - The Amazon Resource Name (ARN) of the user who merged the branches.
--
-- * 'mmIsMerged' - A Boolean value indicating whether the merge has been made.
mergeMetadata
    :: MergeMetadata
mergeMetadata = MergeMetadata' {_mmMergedBy = Nothing, _mmIsMerged = Nothing}


-- | The Amazon Resource Name (ARN) of the user who merged the branches.
mmMergedBy :: Lens' MergeMetadata (Maybe Text)
mmMergedBy = lens _mmMergedBy (\ s a -> s{_mmMergedBy = a})

-- | A Boolean value indicating whether the merge has been made.
mmIsMerged :: Lens' MergeMetadata (Maybe Bool)
mmIsMerged = lens _mmIsMerged (\ s a -> s{_mmIsMerged = a})

instance FromJSON MergeMetadata where
        parseJSON
          = withObject "MergeMetadata"
              (\ x ->
                 MergeMetadata' <$>
                   (x .:? "mergedBy") <*> (x .:? "isMerged"))

instance Hashable MergeMetadata where

instance NFData MergeMetadata where

-- | Returns information about a pull request.
--
--
--
-- /See:/ 'pullRequest' smart constructor.
data PullRequest = PullRequest'
  { _prAuthorARN          :: !(Maybe Text)
  , _prPullRequestId      :: !(Maybe Text)
  , _prCreationDate       :: !(Maybe POSIX)
  , _prPullRequestStatus  :: !(Maybe PullRequestStatusEnum)
  , _prTitle              :: !(Maybe Text)
  , _prClientRequestToken :: !(Maybe Text)
  , _prLastActivityDate   :: !(Maybe POSIX)
  , _prPullRequestTargets :: !(Maybe [PullRequestTarget])
  , _prDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prAuthorARN' - The Amazon Resource Name (ARN) of the user who created the pull request.
--
-- * 'prPullRequestId' - The system-generated ID of the pull request.
--
-- * 'prCreationDate' - The date and time the pull request was originally created, in timestamp format.
--
-- * 'prPullRequestStatus' - The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
--
-- * 'prTitle' - The user-defined title of the pull request. This title is displayed in the list of pull requests to other users of the repository.
--
-- * 'prClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'prLastActivityDate' - The day and time of the last user or system activity on the pull request, in timestamp format.
--
-- * 'prPullRequestTargets' - The targets of the pull request, including the source branch and destination branch for the pull request.
--
-- * 'prDescription' - The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
pullRequest
    :: PullRequest
pullRequest =
  PullRequest'
    { _prAuthorARN = Nothing
    , _prPullRequestId = Nothing
    , _prCreationDate = Nothing
    , _prPullRequestStatus = Nothing
    , _prTitle = Nothing
    , _prClientRequestToken = Nothing
    , _prLastActivityDate = Nothing
    , _prPullRequestTargets = Nothing
    , _prDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the user who created the pull request.
prAuthorARN :: Lens' PullRequest (Maybe Text)
prAuthorARN = lens _prAuthorARN (\ s a -> s{_prAuthorARN = a})

-- | The system-generated ID of the pull request.
prPullRequestId :: Lens' PullRequest (Maybe Text)
prPullRequestId = lens _prPullRequestId (\ s a -> s{_prPullRequestId = a})

-- | The date and time the pull request was originally created, in timestamp format.
prCreationDate :: Lens' PullRequest (Maybe UTCTime)
prCreationDate = lens _prCreationDate (\ s a -> s{_prCreationDate = a}) . mapping _Time

-- | The status of the pull request. Pull request status can only change from @OPEN@ to @CLOSED@ .
prPullRequestStatus :: Lens' PullRequest (Maybe PullRequestStatusEnum)
prPullRequestStatus = lens _prPullRequestStatus (\ s a -> s{_prPullRequestStatus = a})

-- | The user-defined title of the pull request. This title is displayed in the list of pull requests to other users of the repository.
prTitle :: Lens' PullRequest (Maybe Text)
prTitle = lens _prTitle (\ s a -> s{_prTitle = a})

-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
prClientRequestToken :: Lens' PullRequest (Maybe Text)
prClientRequestToken = lens _prClientRequestToken (\ s a -> s{_prClientRequestToken = a})

-- | The day and time of the last user or system activity on the pull request, in timestamp format.
prLastActivityDate :: Lens' PullRequest (Maybe UTCTime)
prLastActivityDate = lens _prLastActivityDate (\ s a -> s{_prLastActivityDate = a}) . mapping _Time

-- | The targets of the pull request, including the source branch and destination branch for the pull request.
prPullRequestTargets :: Lens' PullRequest [PullRequestTarget]
prPullRequestTargets = lens _prPullRequestTargets (\ s a -> s{_prPullRequestTargets = a}) . _Default . _Coerce

-- | The user-defined description of the pull request. This description can be used to clarify what should be reviewed and other details of the request.
prDescription :: Lens' PullRequest (Maybe Text)
prDescription = lens _prDescription (\ s a -> s{_prDescription = a})

instance FromJSON PullRequest where
        parseJSON
          = withObject "PullRequest"
              (\ x ->
                 PullRequest' <$>
                   (x .:? "authorArn") <*> (x .:? "pullRequestId") <*>
                     (x .:? "creationDate")
                     <*> (x .:? "pullRequestStatus")
                     <*> (x .:? "title")
                     <*> (x .:? "clientRequestToken")
                     <*> (x .:? "lastActivityDate")
                     <*> (x .:? "pullRequestTargets" .!= mempty)
                     <*> (x .:? "description"))

instance Hashable PullRequest where

instance NFData PullRequest where

-- | Returns information about a pull request event.
--
--
--
-- /See:/ 'pullRequestEvent' smart constructor.
data PullRequestEvent = PullRequestEvent'
  { _prePullRequestMergedStateChangedEventMetadata :: !(Maybe PullRequestMergedStateChangedEventMetadata)
  , _prePullRequestEventType :: !(Maybe PullRequestEventType)
  , _prePullRequestStatusChangedEventMetadata :: !(Maybe PullRequestStatusChangedEventMetadata)
  , _preActorARN :: !(Maybe Text)
  , _prePullRequestId :: !(Maybe Text)
  , _preEventDate :: !(Maybe POSIX)
  , _prePullRequestSourceReferenceUpdatedEventMetadata :: !(Maybe PullRequestSourceReferenceUpdatedEventMetadata)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequestEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prePullRequestMergedStateChangedEventMetadata' - Information about the change in mergability state for the pull request event.
--
-- * 'prePullRequestEventType' - The type of the pull request event, for example a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED).
--
-- * 'prePullRequestStatusChangedEventMetadata' - Information about the change in status for the pull request event.
--
-- * 'preActorARN' - The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with additional commits or changing the status of a pull request.
--
-- * 'prePullRequestId' - The system-generated ID of the pull request.
--
-- * 'preEventDate' - The day and time of the pull request event, in timestamp format.
--
-- * 'prePullRequestSourceReferenceUpdatedEventMetadata' - Information about the updated source branch for the pull request event.
pullRequestEvent
    :: PullRequestEvent
pullRequestEvent =
  PullRequestEvent'
    { _prePullRequestMergedStateChangedEventMetadata = Nothing
    , _prePullRequestEventType = Nothing
    , _prePullRequestStatusChangedEventMetadata = Nothing
    , _preActorARN = Nothing
    , _prePullRequestId = Nothing
    , _preEventDate = Nothing
    , _prePullRequestSourceReferenceUpdatedEventMetadata = Nothing
    }


-- | Information about the change in mergability state for the pull request event.
prePullRequestMergedStateChangedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestMergedStateChangedEventMetadata)
prePullRequestMergedStateChangedEventMetadata = lens _prePullRequestMergedStateChangedEventMetadata (\ s a -> s{_prePullRequestMergedStateChangedEventMetadata = a})

-- | The type of the pull request event, for example a status change event (PULL_REQUEST_STATUS_CHANGED) or update event (PULL_REQUEST_SOURCE_REFERENCE_UPDATED).
prePullRequestEventType :: Lens' PullRequestEvent (Maybe PullRequestEventType)
prePullRequestEventType = lens _prePullRequestEventType (\ s a -> s{_prePullRequestEventType = a})

-- | Information about the change in status for the pull request event.
prePullRequestStatusChangedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestStatusChangedEventMetadata)
prePullRequestStatusChangedEventMetadata = lens _prePullRequestStatusChangedEventMetadata (\ s a -> s{_prePullRequestStatusChangedEventMetadata = a})

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with additional commits or changing the status of a pull request.
preActorARN :: Lens' PullRequestEvent (Maybe Text)
preActorARN = lens _preActorARN (\ s a -> s{_preActorARN = a})

-- | The system-generated ID of the pull request.
prePullRequestId :: Lens' PullRequestEvent (Maybe Text)
prePullRequestId = lens _prePullRequestId (\ s a -> s{_prePullRequestId = a})

-- | The day and time of the pull request event, in timestamp format.
preEventDate :: Lens' PullRequestEvent (Maybe UTCTime)
preEventDate = lens _preEventDate (\ s a -> s{_preEventDate = a}) . mapping _Time

-- | Information about the updated source branch for the pull request event.
prePullRequestSourceReferenceUpdatedEventMetadata :: Lens' PullRequestEvent (Maybe PullRequestSourceReferenceUpdatedEventMetadata)
prePullRequestSourceReferenceUpdatedEventMetadata = lens _prePullRequestSourceReferenceUpdatedEventMetadata (\ s a -> s{_prePullRequestSourceReferenceUpdatedEventMetadata = a})

instance FromJSON PullRequestEvent where
        parseJSON
          = withObject "PullRequestEvent"
              (\ x ->
                 PullRequestEvent' <$>
                   (x .:? "pullRequestMergedStateChangedEventMetadata")
                     <*> (x .:? "pullRequestEventType")
                     <*> (x .:? "pullRequestStatusChangedEventMetadata")
                     <*> (x .:? "actorArn")
                     <*> (x .:? "pullRequestId")
                     <*> (x .:? "eventDate")
                     <*>
                     (x .:?
                        "pullRequestSourceReferenceUpdatedEventMetadata"))

instance Hashable PullRequestEvent where

instance NFData PullRequestEvent where

-- | Returns information about the change in the merge state for a pull request event.
--
--
--
-- /See:/ 'pullRequestMergedStateChangedEventMetadata' smart constructor.
data PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata'
  { _prmscemDestinationReference :: !(Maybe Text)
  , _prmscemMergeMetadata        :: !(Maybe MergeMetadata)
  , _prmscemRepositoryName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequestMergedStateChangedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prmscemDestinationReference' - The name of the branch that the pull request will be merged into.
--
-- * 'prmscemMergeMetadata' - Information about the merge state change event.
--
-- * 'prmscemRepositoryName' - The name of the repository where the pull request was created.
pullRequestMergedStateChangedEventMetadata
    :: PullRequestMergedStateChangedEventMetadata
pullRequestMergedStateChangedEventMetadata =
  PullRequestMergedStateChangedEventMetadata'
    { _prmscemDestinationReference = Nothing
    , _prmscemMergeMetadata = Nothing
    , _prmscemRepositoryName = Nothing
    }


-- | The name of the branch that the pull request will be merged into.
prmscemDestinationReference :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe Text)
prmscemDestinationReference = lens _prmscemDestinationReference (\ s a -> s{_prmscemDestinationReference = a})

-- | Information about the merge state change event.
prmscemMergeMetadata :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe MergeMetadata)
prmscemMergeMetadata = lens _prmscemMergeMetadata (\ s a -> s{_prmscemMergeMetadata = a})

-- | The name of the repository where the pull request was created.
prmscemRepositoryName :: Lens' PullRequestMergedStateChangedEventMetadata (Maybe Text)
prmscemRepositoryName = lens _prmscemRepositoryName (\ s a -> s{_prmscemRepositoryName = a})

instance FromJSON
           PullRequestMergedStateChangedEventMetadata
         where
        parseJSON
          = withObject
              "PullRequestMergedStateChangedEventMetadata"
              (\ x ->
                 PullRequestMergedStateChangedEventMetadata' <$>
                   (x .:? "destinationReference") <*>
                     (x .:? "mergeMetadata")
                     <*> (x .:? "repositoryName"))

instance Hashable
           PullRequestMergedStateChangedEventMetadata
         where

instance NFData
           PullRequestMergedStateChangedEventMetadata
         where

-- | Information about an update to the source branch of a pull request.
--
--
--
-- /See:/ 'pullRequestSourceReferenceUpdatedEventMetadata' smart constructor.
data PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata'
  { _prsruemAfterCommitId  :: !(Maybe Text)
  , _prsruemBeforeCommitId :: !(Maybe Text)
  , _prsruemRepositoryName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequestSourceReferenceUpdatedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsruemAfterCommitId' - The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
--
-- * 'prsruemBeforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
--
-- * 'prsruemRepositoryName' - The name of the repository where the pull request was updated.
pullRequestSourceReferenceUpdatedEventMetadata
    :: PullRequestSourceReferenceUpdatedEventMetadata
pullRequestSourceReferenceUpdatedEventMetadata =
  PullRequestSourceReferenceUpdatedEventMetadata'
    { _prsruemAfterCommitId = Nothing
    , _prsruemBeforeCommitId = Nothing
    , _prsruemRepositoryName = Nothing
    }


-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the pull request was updated.
prsruemAfterCommitId :: Lens' PullRequestSourceReferenceUpdatedEventMetadata (Maybe Text)
prsruemAfterCommitId = lens _prsruemAfterCommitId (\ s a -> s{_prsruemAfterCommitId = a})

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was updated.
prsruemBeforeCommitId :: Lens' PullRequestSourceReferenceUpdatedEventMetadata (Maybe Text)
prsruemBeforeCommitId = lens _prsruemBeforeCommitId (\ s a -> s{_prsruemBeforeCommitId = a})

-- | The name of the repository where the pull request was updated.
prsruemRepositoryName :: Lens' PullRequestSourceReferenceUpdatedEventMetadata (Maybe Text)
prsruemRepositoryName = lens _prsruemRepositoryName (\ s a -> s{_prsruemRepositoryName = a})

instance FromJSON
           PullRequestSourceReferenceUpdatedEventMetadata
         where
        parseJSON
          = withObject
              "PullRequestSourceReferenceUpdatedEventMetadata"
              (\ x ->
                 PullRequestSourceReferenceUpdatedEventMetadata' <$>
                   (x .:? "afterCommitId") <*> (x .:? "beforeCommitId")
                     <*> (x .:? "repositoryName"))

instance Hashable
           PullRequestSourceReferenceUpdatedEventMetadata
         where

instance NFData
           PullRequestSourceReferenceUpdatedEventMetadata
         where

-- | Information about a change to the status of a pull request.
--
--
--
-- /See:/ 'pullRequestStatusChangedEventMetadata' smart constructor.
newtype PullRequestStatusChangedEventMetadata = PullRequestStatusChangedEventMetadata'
  { _prscemPullRequestStatus :: Maybe PullRequestStatusEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequestStatusChangedEventMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prscemPullRequestStatus' - The changed status of the pull request.
pullRequestStatusChangedEventMetadata
    :: PullRequestStatusChangedEventMetadata
pullRequestStatusChangedEventMetadata =
  PullRequestStatusChangedEventMetadata' {_prscemPullRequestStatus = Nothing}


-- | The changed status of the pull request.
prscemPullRequestStatus :: Lens' PullRequestStatusChangedEventMetadata (Maybe PullRequestStatusEnum)
prscemPullRequestStatus = lens _prscemPullRequestStatus (\ s a -> s{_prscemPullRequestStatus = a})

instance FromJSON
           PullRequestStatusChangedEventMetadata
         where
        parseJSON
          = withObject "PullRequestStatusChangedEventMetadata"
              (\ x ->
                 PullRequestStatusChangedEventMetadata' <$>
                   (x .:? "pullRequestStatus"))

instance Hashable
           PullRequestStatusChangedEventMetadata
         where

instance NFData PullRequestStatusChangedEventMetadata
         where

-- | Returns information about a pull request target.
--
--
--
-- /See:/ 'pullRequestTarget' smart constructor.
data PullRequestTarget = PullRequestTarget'
  { _prtSourceCommit         :: !(Maybe Text)
  , _prtDestinationReference :: !(Maybe Text)
  , _prtMergeMetadata        :: !(Maybe MergeMetadata)
  , _prtDestinationCommit    :: !(Maybe Text)
  , _prtRepositoryName       :: !(Maybe Text)
  , _prtSourceReference      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PullRequestTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prtSourceCommit' - The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID will change to reflect the new tip of the branch.
--
-- * 'prtDestinationReference' - The branch of the repository where the pull request changes will be merged into. Also known as the destination branch.
--
-- * 'prtMergeMetadata' - Returns metadata about the state of the merge, including whether the merge has been made.
--
-- * 'prtDestinationCommit' - The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
--
-- * 'prtRepositoryName' - The name of the repository that contains the pull request source and destination branches.
--
-- * 'prtSourceReference' - The branch of the repository that contains the changes for the pull request. Also known as the source branch.
pullRequestTarget
    :: PullRequestTarget
pullRequestTarget =
  PullRequestTarget'
    { _prtSourceCommit = Nothing
    , _prtDestinationReference = Nothing
    , _prtMergeMetadata = Nothing
    , _prtDestinationCommit = Nothing
    , _prtRepositoryName = Nothing
    , _prtSourceReference = Nothing
    }


-- | The full commit ID of the tip of the source branch used to create the pull request. If the pull request branch is updated by a push while the pull request is open, the commit ID will change to reflect the new tip of the branch.
prtSourceCommit :: Lens' PullRequestTarget (Maybe Text)
prtSourceCommit = lens _prtSourceCommit (\ s a -> s{_prtSourceCommit = a})

-- | The branch of the repository where the pull request changes will be merged into. Also known as the destination branch.
prtDestinationReference :: Lens' PullRequestTarget (Maybe Text)
prtDestinationReference = lens _prtDestinationReference (\ s a -> s{_prtDestinationReference = a})

-- | Returns metadata about the state of the merge, including whether the merge has been made.
prtMergeMetadata :: Lens' PullRequestTarget (Maybe MergeMetadata)
prtMergeMetadata = lens _prtMergeMetadata (\ s a -> s{_prtMergeMetadata = a})

-- | The full commit ID that is the tip of the destination branch. This is the commit where the pull request was or will be merged.
prtDestinationCommit :: Lens' PullRequestTarget (Maybe Text)
prtDestinationCommit = lens _prtDestinationCommit (\ s a -> s{_prtDestinationCommit = a})

-- | The name of the repository that contains the pull request source and destination branches.
prtRepositoryName :: Lens' PullRequestTarget (Maybe Text)
prtRepositoryName = lens _prtRepositoryName (\ s a -> s{_prtRepositoryName = a})

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
prtSourceReference :: Lens' PullRequestTarget (Maybe Text)
prtSourceReference = lens _prtSourceReference (\ s a -> s{_prtSourceReference = a})

instance FromJSON PullRequestTarget where
        parseJSON
          = withObject "PullRequestTarget"
              (\ x ->
                 PullRequestTarget' <$>
                   (x .:? "sourceCommit") <*>
                     (x .:? "destinationReference")
                     <*> (x .:? "mergeMetadata")
                     <*> (x .:? "destinationCommit")
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "sourceReference"))

instance Hashable PullRequestTarget where

instance NFData PullRequestTarget where

-- | Information about a repository.
--
--
--
-- /See:/ 'repositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { _rmRepositoryDescription :: !(Maybe Text)
  , _rmLastModifiedDate      :: !(Maybe POSIX)
  , _rmARN                   :: !(Maybe Text)
  , _rmCloneURLHTTP          :: !(Maybe Text)
  , _rmAccountId             :: !(Maybe Text)
  , _rmDefaultBranch         :: !(Maybe Text)
  , _rmRepositoryId          :: !(Maybe Text)
  , _rmRepositoryName        :: !(Maybe Text)
  , _rmCreationDate          :: !(Maybe POSIX)
  , _rmCloneURLSSH           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmRepositoryDescription' - A comment or description about the repository.
--
-- * 'rmLastModifiedDate' - The date and time the repository was last modified, in timestamp format.
--
-- * 'rmARN' - The Amazon Resource Name (ARN) of the repository.
--
-- * 'rmCloneURLHTTP' - The URL to use for cloning the repository over HTTPS.
--
-- * 'rmAccountId' - The ID of the AWS account associated with the repository.
--
-- * 'rmDefaultBranch' - The repository's default branch name.
--
-- * 'rmRepositoryId' - The ID of the repository.
--
-- * 'rmRepositoryName' - The repository's name.
--
-- * 'rmCreationDate' - The date and time the repository was created, in timestamp format.
--
-- * 'rmCloneURLSSH' - The URL to use for cloning the repository over SSH.
repositoryMetadata
    :: RepositoryMetadata
repositoryMetadata =
  RepositoryMetadata'
    { _rmRepositoryDescription = Nothing
    , _rmLastModifiedDate = Nothing
    , _rmARN = Nothing
    , _rmCloneURLHTTP = Nothing
    , _rmAccountId = Nothing
    , _rmDefaultBranch = Nothing
    , _rmRepositoryId = Nothing
    , _rmRepositoryName = Nothing
    , _rmCreationDate = Nothing
    , _rmCloneURLSSH = Nothing
    }


-- | A comment or description about the repository.
rmRepositoryDescription :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryDescription = lens _rmRepositoryDescription (\ s a -> s{_rmRepositoryDescription = a})

-- | The date and time the repository was last modified, in timestamp format.
rmLastModifiedDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmLastModifiedDate = lens _rmLastModifiedDate (\ s a -> s{_rmLastModifiedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the repository.
rmARN :: Lens' RepositoryMetadata (Maybe Text)
rmARN = lens _rmARN (\ s a -> s{_rmARN = a})

-- | The URL to use for cloning the repository over HTTPS.
rmCloneURLHTTP :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLHTTP = lens _rmCloneURLHTTP (\ s a -> s{_rmCloneURLHTTP = a})

-- | The ID of the AWS account associated with the repository.
rmAccountId :: Lens' RepositoryMetadata (Maybe Text)
rmAccountId = lens _rmAccountId (\ s a -> s{_rmAccountId = a})

-- | The repository's default branch name.
rmDefaultBranch :: Lens' RepositoryMetadata (Maybe Text)
rmDefaultBranch = lens _rmDefaultBranch (\ s a -> s{_rmDefaultBranch = a})

-- | The ID of the repository.
rmRepositoryId :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryId = lens _rmRepositoryId (\ s a -> s{_rmRepositoryId = a})

-- | The repository's name.
rmRepositoryName :: Lens' RepositoryMetadata (Maybe Text)
rmRepositoryName = lens _rmRepositoryName (\ s a -> s{_rmRepositoryName = a})

-- | The date and time the repository was created, in timestamp format.
rmCreationDate :: Lens' RepositoryMetadata (Maybe UTCTime)
rmCreationDate = lens _rmCreationDate (\ s a -> s{_rmCreationDate = a}) . mapping _Time

-- | The URL to use for cloning the repository over SSH.
rmCloneURLSSH :: Lens' RepositoryMetadata (Maybe Text)
rmCloneURLSSH = lens _rmCloneURLSSH (\ s a -> s{_rmCloneURLSSH = a})

instance FromJSON RepositoryMetadata where
        parseJSON
          = withObject "RepositoryMetadata"
              (\ x ->
                 RepositoryMetadata' <$>
                   (x .:? "repositoryDescription") <*>
                     (x .:? "lastModifiedDate")
                     <*> (x .:? "Arn")
                     <*> (x .:? "cloneUrlHttp")
                     <*> (x .:? "accountId")
                     <*> (x .:? "defaultBranch")
                     <*> (x .:? "repositoryId")
                     <*> (x .:? "repositoryName")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "cloneUrlSsh"))

instance Hashable RepositoryMetadata where

instance NFData RepositoryMetadata where

-- | Information about a repository name and ID.
--
--
--
-- /See:/ 'repositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { _rnipRepositoryId   :: !(Maybe Text)
  , _rnipRepositoryName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryNameIdPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnipRepositoryId' - The ID associated with the repository.
--
-- * 'rnipRepositoryName' - The name associated with the repository.
repositoryNameIdPair
    :: RepositoryNameIdPair
repositoryNameIdPair =
  RepositoryNameIdPair'
    {_rnipRepositoryId = Nothing, _rnipRepositoryName = Nothing}


-- | The ID associated with the repository.
rnipRepositoryId :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryId = lens _rnipRepositoryId (\ s a -> s{_rnipRepositoryId = a})

-- | The name associated with the repository.
rnipRepositoryName :: Lens' RepositoryNameIdPair (Maybe Text)
rnipRepositoryName = lens _rnipRepositoryName (\ s a -> s{_rnipRepositoryName = a})

instance FromJSON RepositoryNameIdPair where
        parseJSON
          = withObject "RepositoryNameIdPair"
              (\ x ->
                 RepositoryNameIdPair' <$>
                   (x .:? "repositoryId") <*> (x .:? "repositoryName"))

instance Hashable RepositoryNameIdPair where

instance NFData RepositoryNameIdPair where

-- | Information about a trigger for a repository.
--
--
--
-- /See:/ 'repositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { _rtBranches       :: !(Maybe [Text])
  , _rtCustomData     :: !(Maybe Text)
  , _rtName           :: !Text
  , _rtDestinationARN :: !Text
  , _rtEvents         :: ![RepositoryTriggerEventEnum]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtBranches' - The branches that will be included in the trigger configuration. If you specify an empty array, the trigger will apply to all branches.
--
-- * 'rtCustomData' - Any custom data associated with the trigger that will be included in the information sent to the target of the trigger.
--
-- * 'rtName' - The name of the trigger.
--
-- * 'rtDestinationARN' - The ARN of the resource that is the target for a trigger. For example, the ARN of a topic in Amazon Simple Notification Service (SNS).
--
-- * 'rtEvents' - The repository events that will cause the trigger to run actions in another service, such as sending a notification through Amazon Simple Notification Service (SNS).
repositoryTrigger
    :: Text -- ^ 'rtName'
    -> Text -- ^ 'rtDestinationARN'
    -> RepositoryTrigger
repositoryTrigger pName_ pDestinationARN_ =
  RepositoryTrigger'
    { _rtBranches = Nothing
    , _rtCustomData = Nothing
    , _rtName = pName_
    , _rtDestinationARN = pDestinationARN_
    , _rtEvents = mempty
    }


-- | The branches that will be included in the trigger configuration. If you specify an empty array, the trigger will apply to all branches.
rtBranches :: Lens' RepositoryTrigger [Text]
rtBranches = lens _rtBranches (\ s a -> s{_rtBranches = a}) . _Default . _Coerce

-- | Any custom data associated with the trigger that will be included in the information sent to the target of the trigger.
rtCustomData :: Lens' RepositoryTrigger (Maybe Text)
rtCustomData = lens _rtCustomData (\ s a -> s{_rtCustomData = a})

-- | The name of the trigger.
rtName :: Lens' RepositoryTrigger Text
rtName = lens _rtName (\ s a -> s{_rtName = a})

-- | The ARN of the resource that is the target for a trigger. For example, the ARN of a topic in Amazon Simple Notification Service (SNS).
rtDestinationARN :: Lens' RepositoryTrigger Text
rtDestinationARN = lens _rtDestinationARN (\ s a -> s{_rtDestinationARN = a})

-- | The repository events that will cause the trigger to run actions in another service, such as sending a notification through Amazon Simple Notification Service (SNS).
rtEvents :: Lens' RepositoryTrigger [RepositoryTriggerEventEnum]
rtEvents = lens _rtEvents (\ s a -> s{_rtEvents = a}) . _Coerce

instance FromJSON RepositoryTrigger where
        parseJSON
          = withObject "RepositoryTrigger"
              (\ x ->
                 RepositoryTrigger' <$>
                   (x .:? "branches" .!= mempty) <*>
                     (x .:? "customData")
                     <*> (x .: "name")
                     <*> (x .: "destinationArn")
                     <*> (x .:? "events" .!= mempty))

instance Hashable RepositoryTrigger where

instance NFData RepositoryTrigger where

instance ToJSON RepositoryTrigger where
        toJSON RepositoryTrigger'{..}
          = object
              (catMaybes
                 [("branches" .=) <$> _rtBranches,
                  ("customData" .=) <$> _rtCustomData,
                  Just ("name" .= _rtName),
                  Just ("destinationArn" .= _rtDestinationARN),
                  Just ("events" .= _rtEvents)])

-- | A trigger failed to run.
--
--
--
-- /See:/ 'repositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { _rtefFailureMessage :: !(Maybe Text)
  , _rtefTrigger        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RepositoryTriggerExecutionFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtefFailureMessage' - Additional message information about the trigger that did not run.
--
-- * 'rtefTrigger' - The name of the trigger that did not run.
repositoryTriggerExecutionFailure
    :: RepositoryTriggerExecutionFailure
repositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
    {_rtefFailureMessage = Nothing, _rtefTrigger = Nothing}


-- | Additional message information about the trigger that did not run.
rtefFailureMessage :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefFailureMessage = lens _rtefFailureMessage (\ s a -> s{_rtefFailureMessage = a})

-- | The name of the trigger that did not run.
rtefTrigger :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefTrigger = lens _rtefTrigger (\ s a -> s{_rtefTrigger = a})

instance FromJSON RepositoryTriggerExecutionFailure
         where
        parseJSON
          = withObject "RepositoryTriggerExecutionFailure"
              (\ x ->
                 RepositoryTriggerExecutionFailure' <$>
                   (x .:? "failureMessage") <*> (x .:? "trigger"))

instance Hashable RepositoryTriggerExecutionFailure
         where

instance NFData RepositoryTriggerExecutionFailure
         where

-- | Returns information about a target for a pull request.
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'
  { _tDestinationReference :: !(Maybe Text)
  , _tRepositoryName       :: !Text
  , _tSourceReference      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDestinationReference' - The branch of the repository where the pull request changes will be merged into. Also known as the destination branch.
--
-- * 'tRepositoryName' - The name of the repository that contains the pull request.
--
-- * 'tSourceReference' - The branch of the repository that contains the changes for the pull request. Also known as the source branch.
target
    :: Text -- ^ 'tRepositoryName'
    -> Text -- ^ 'tSourceReference'
    -> Target
target pRepositoryName_ pSourceReference_ =
  Target'
    { _tDestinationReference = Nothing
    , _tRepositoryName = pRepositoryName_
    , _tSourceReference = pSourceReference_
    }


-- | The branch of the repository where the pull request changes will be merged into. Also known as the destination branch.
tDestinationReference :: Lens' Target (Maybe Text)
tDestinationReference = lens _tDestinationReference (\ s a -> s{_tDestinationReference = a})

-- | The name of the repository that contains the pull request.
tRepositoryName :: Lens' Target Text
tRepositoryName = lens _tRepositoryName (\ s a -> s{_tRepositoryName = a})

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
tSourceReference :: Lens' Target Text
tSourceReference = lens _tSourceReference (\ s a -> s{_tSourceReference = a})

instance Hashable Target where

instance NFData Target where

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("destinationReference" .=) <$>
                    _tDestinationReference,
                  Just ("repositoryName" .= _tRepositoryName),
                  Just ("sourceReference" .= _tSourceReference)])

-- | Information about the user who made a specified commit.
--
--
--
-- /See:/ 'userInfo' smart constructor.
data UserInfo = UserInfo'
  { _uiEmail :: !(Maybe Text)
  , _uiDate  :: !(Maybe Text)
  , _uiName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiEmail' - The email address associated with the user who made the commit, if any.
--
-- * 'uiDate' - The date when the specified commit was commited, in timestamp format with GMT offset.
--
-- * 'uiName' - The name of the user who made the specified commit.
userInfo
    :: UserInfo
userInfo = UserInfo' {_uiEmail = Nothing, _uiDate = Nothing, _uiName = Nothing}


-- | The email address associated with the user who made the commit, if any.
uiEmail :: Lens' UserInfo (Maybe Text)
uiEmail = lens _uiEmail (\ s a -> s{_uiEmail = a})

-- | The date when the specified commit was commited, in timestamp format with GMT offset.
uiDate :: Lens' UserInfo (Maybe Text)
uiDate = lens _uiDate (\ s a -> s{_uiDate = a})

-- | The name of the user who made the specified commit.
uiName :: Lens' UserInfo (Maybe Text)
uiName = lens _uiName (\ s a -> s{_uiName = a})

instance FromJSON UserInfo where
        parseJSON
          = withObject "UserInfo"
              (\ x ->
                 UserInfo' <$>
                   (x .:? "email") <*> (x .:? "date") <*>
                     (x .:? "name"))

instance Hashable UserInfo where

instance NFData UserInfo where
