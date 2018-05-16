{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.Sum

-- | Describes the activity information.
--
--
--
-- /See:/ 'activity' smart constructor.
data Activity = Activity'
  { _aResourceMetadata :: !(Maybe ResourceMetadata)
  , _aInitiator        :: !(Maybe UserMetadata)
  , _aParticipants     :: !(Maybe Participants)
  , _aOriginalParent   :: !(Maybe ResourceMetadata)
  , _aType             :: !(Maybe ActivityType)
  , _aCommentMetadata  :: !(Maybe CommentMetadata)
  , _aTimeStamp        :: !(Maybe POSIX)
  , _aOrganizationId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Activity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aResourceMetadata' - The metadata of the resource involved in the user action.
--
-- * 'aInitiator' - The user who performed the action.
--
-- * 'aParticipants' - The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
--
-- * 'aOriginalParent' - The original parent of the resource. This is an optional field and is filled for move activities.
--
-- * 'aType' - The activity type.
--
-- * 'aCommentMetadata' - Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
--
-- * 'aTimeStamp' - The timestamp when the action was performed.
--
-- * 'aOrganizationId' - The ID of the organization.
activity
    :: Activity
activity =
  Activity'
    { _aResourceMetadata = Nothing
    , _aInitiator = Nothing
    , _aParticipants = Nothing
    , _aOriginalParent = Nothing
    , _aType = Nothing
    , _aCommentMetadata = Nothing
    , _aTimeStamp = Nothing
    , _aOrganizationId = Nothing
    }


-- | The metadata of the resource involved in the user action.
aResourceMetadata :: Lens' Activity (Maybe ResourceMetadata)
aResourceMetadata = lens _aResourceMetadata (\ s a -> s{_aResourceMetadata = a})

-- | The user who performed the action.
aInitiator :: Lens' Activity (Maybe UserMetadata)
aInitiator = lens _aInitiator (\ s a -> s{_aInitiator = a})

-- | The list of users or groups impacted by this action. This is an optional field and is filled for the following sharing activities: DOCUMENT_SHARED, DOCUMENT_SHARED, DOCUMENT_UNSHARED, FOLDER_SHARED, FOLDER_UNSHARED.
aParticipants :: Lens' Activity (Maybe Participants)
aParticipants = lens _aParticipants (\ s a -> s{_aParticipants = a})

-- | The original parent of the resource. This is an optional field and is filled for move activities.
aOriginalParent :: Lens' Activity (Maybe ResourceMetadata)
aOriginalParent = lens _aOriginalParent (\ s a -> s{_aOriginalParent = a})

-- | The activity type.
aType :: Lens' Activity (Maybe ActivityType)
aType = lens _aType (\ s a -> s{_aType = a})

-- | Metadata of the commenting activity. This is an optional field and is filled for commenting activities.
aCommentMetadata :: Lens' Activity (Maybe CommentMetadata)
aCommentMetadata = lens _aCommentMetadata (\ s a -> s{_aCommentMetadata = a})

-- | The timestamp when the action was performed.
aTimeStamp :: Lens' Activity (Maybe UTCTime)
aTimeStamp = lens _aTimeStamp (\ s a -> s{_aTimeStamp = a}) . mapping _Time

-- | The ID of the organization.
aOrganizationId :: Lens' Activity (Maybe Text)
aOrganizationId = lens _aOrganizationId (\ s a -> s{_aOrganizationId = a})

instance FromJSON Activity where
        parseJSON
          = withObject "Activity"
              (\ x ->
                 Activity' <$>
                   (x .:? "ResourceMetadata") <*> (x .:? "Initiator")
                     <*> (x .:? "Participants")
                     <*> (x .:? "OriginalParent")
                     <*> (x .:? "Type")
                     <*> (x .:? "CommentMetadata")
                     <*> (x .:? "TimeStamp")
                     <*> (x .:? "OrganizationId"))

instance Hashable Activity where

instance NFData Activity where

-- | Describes a comment.
--
--
--
-- /See:/ 'comment' smart constructor.
data Comment = Comment'
  { _cStatus           :: !(Maybe CommentStatusType)
  , _cText             :: !(Maybe (Sensitive Text))
  , _cVisibility       :: !(Maybe CommentVisibilityType)
  , _cThreadId         :: !(Maybe Text)
  , _cContributor      :: !(Maybe User)
  , _cCreatedTimestamp :: !(Maybe POSIX)
  , _cRecipientId      :: !(Maybe Text)
  , _cParentId         :: !(Maybe Text)
  , _cCommentId        :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Comment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the comment.
--
-- * 'cText' - The text of the comment.
--
-- * 'cVisibility' - The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- * 'cThreadId' - The ID of the root comment in the thread.
--
-- * 'cContributor' - The details of the user who made the comment.
--
-- * 'cCreatedTimestamp' - The time that the comment was created.
--
-- * 'cRecipientId' - If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
--
-- * 'cParentId' - The ID of the parent comment.
--
-- * 'cCommentId' - The ID of the comment.
comment
    :: Text -- ^ 'cCommentId'
    -> Comment
comment pCommentId_ =
  Comment'
    { _cStatus = Nothing
    , _cText = Nothing
    , _cVisibility = Nothing
    , _cThreadId = Nothing
    , _cContributor = Nothing
    , _cCreatedTimestamp = Nothing
    , _cRecipientId = Nothing
    , _cParentId = Nothing
    , _cCommentId = pCommentId_
    }


-- | The status of the comment.
cStatus :: Lens' Comment (Maybe CommentStatusType)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The text of the comment.
cText :: Lens' Comment (Maybe Text)
cText = lens _cText (\ s a -> s{_cText = a}) . mapping _Sensitive

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
cVisibility :: Lens' Comment (Maybe CommentVisibilityType)
cVisibility = lens _cVisibility (\ s a -> s{_cVisibility = a})

-- | The ID of the root comment in the thread.
cThreadId :: Lens' Comment (Maybe Text)
cThreadId = lens _cThreadId (\ s a -> s{_cThreadId = a})

-- | The details of the user who made the comment.
cContributor :: Lens' Comment (Maybe User)
cContributor = lens _cContributor (\ s a -> s{_cContributor = a})

-- | The time that the comment was created.
cCreatedTimestamp :: Lens' Comment (Maybe UTCTime)
cCreatedTimestamp = lens _cCreatedTimestamp (\ s a -> s{_cCreatedTimestamp = a}) . mapping _Time

-- | If the comment is a reply to another user's comment, this field contains the user ID of the user being replied to.
cRecipientId :: Lens' Comment (Maybe Text)
cRecipientId = lens _cRecipientId (\ s a -> s{_cRecipientId = a})

-- | The ID of the parent comment.
cParentId :: Lens' Comment (Maybe Text)
cParentId = lens _cParentId (\ s a -> s{_cParentId = a})

-- | The ID of the comment.
cCommentId :: Lens' Comment Text
cCommentId = lens _cCommentId (\ s a -> s{_cCommentId = a})

instance FromJSON Comment where
        parseJSON
          = withObject "Comment"
              (\ x ->
                 Comment' <$>
                   (x .:? "Status") <*> (x .:? "Text") <*>
                     (x .:? "Visibility")
                     <*> (x .:? "ThreadId")
                     <*> (x .:? "Contributor")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "RecipientId")
                     <*> (x .:? "ParentId")
                     <*> (x .: "CommentId"))

instance Hashable Comment where

instance NFData Comment where

-- | Describes the metadata of a comment.
--
--
--
-- /See:/ 'commentMetadata' smart constructor.
data CommentMetadata = CommentMetadata'
  { _cmCommentStatus    :: !(Maybe CommentStatusType)
  , _cmContributor      :: !(Maybe User)
  , _cmCommentId        :: !(Maybe Text)
  , _cmCreatedTimestamp :: !(Maybe POSIX)
  , _cmRecipientId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CommentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmCommentStatus' - The status of the comment.
--
-- * 'cmContributor' - The user who made the comment.
--
-- * 'cmCommentId' - The ID of the comment.
--
-- * 'cmCreatedTimestamp' - The timestamp that the comment was created.
--
-- * 'cmRecipientId' - The ID of the user being replied to.
commentMetadata
    :: CommentMetadata
commentMetadata =
  CommentMetadata'
    { _cmCommentStatus = Nothing
    , _cmContributor = Nothing
    , _cmCommentId = Nothing
    , _cmCreatedTimestamp = Nothing
    , _cmRecipientId = Nothing
    }


-- | The status of the comment.
cmCommentStatus :: Lens' CommentMetadata (Maybe CommentStatusType)
cmCommentStatus = lens _cmCommentStatus (\ s a -> s{_cmCommentStatus = a})

-- | The user who made the comment.
cmContributor :: Lens' CommentMetadata (Maybe User)
cmContributor = lens _cmContributor (\ s a -> s{_cmContributor = a})

-- | The ID of the comment.
cmCommentId :: Lens' CommentMetadata (Maybe Text)
cmCommentId = lens _cmCommentId (\ s a -> s{_cmCommentId = a})

-- | The timestamp that the comment was created.
cmCreatedTimestamp :: Lens' CommentMetadata (Maybe UTCTime)
cmCreatedTimestamp = lens _cmCreatedTimestamp (\ s a -> s{_cmCreatedTimestamp = a}) . mapping _Time

-- | The ID of the user being replied to.
cmRecipientId :: Lens' CommentMetadata (Maybe Text)
cmRecipientId = lens _cmRecipientId (\ s a -> s{_cmRecipientId = a})

instance FromJSON CommentMetadata where
        parseJSON
          = withObject "CommentMetadata"
              (\ x ->
                 CommentMetadata' <$>
                   (x .:? "CommentStatus") <*> (x .:? "Contributor") <*>
                     (x .:? "CommentId")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "RecipientId"))

instance Hashable CommentMetadata where

instance NFData CommentMetadata where

-- | Describes the document.
--
--
--
-- /See:/ 'documentMetadata' smart constructor.
data DocumentMetadata = DocumentMetadata'
  { _dmLatestVersionMetadata :: !(Maybe DocumentVersionMetadata)
  , _dmParentFolderId        :: !(Maybe Text)
  , _dmModifiedTimestamp     :: !(Maybe POSIX)
  , _dmId                    :: !(Maybe Text)
  , _dmLabels                :: !(Maybe [Text])
  , _dmResourceState         :: !(Maybe ResourceStateType)
  , _dmCreatedTimestamp      :: !(Maybe POSIX)
  , _dmCreatorId             :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmLatestVersionMetadata' - The latest version of the document.
--
-- * 'dmParentFolderId' - The ID of the parent folder.
--
-- * 'dmModifiedTimestamp' - The time when the document was updated.
--
-- * 'dmId' - The ID of the document.
--
-- * 'dmLabels' - List of labels on the document.
--
-- * 'dmResourceState' - The resource state.
--
-- * 'dmCreatedTimestamp' - The time when the document was created.
--
-- * 'dmCreatorId' - The ID of the creator.
documentMetadata
    :: DocumentMetadata
documentMetadata =
  DocumentMetadata'
    { _dmLatestVersionMetadata = Nothing
    , _dmParentFolderId = Nothing
    , _dmModifiedTimestamp = Nothing
    , _dmId = Nothing
    , _dmLabels = Nothing
    , _dmResourceState = Nothing
    , _dmCreatedTimestamp = Nothing
    , _dmCreatorId = Nothing
    }


-- | The latest version of the document.
dmLatestVersionMetadata :: Lens' DocumentMetadata (Maybe DocumentVersionMetadata)
dmLatestVersionMetadata = lens _dmLatestVersionMetadata (\ s a -> s{_dmLatestVersionMetadata = a})

-- | The ID of the parent folder.
dmParentFolderId :: Lens' DocumentMetadata (Maybe Text)
dmParentFolderId = lens _dmParentFolderId (\ s a -> s{_dmParentFolderId = a})

-- | The time when the document was updated.
dmModifiedTimestamp :: Lens' DocumentMetadata (Maybe UTCTime)
dmModifiedTimestamp = lens _dmModifiedTimestamp (\ s a -> s{_dmModifiedTimestamp = a}) . mapping _Time

-- | The ID of the document.
dmId :: Lens' DocumentMetadata (Maybe Text)
dmId = lens _dmId (\ s a -> s{_dmId = a})

-- | List of labels on the document.
dmLabels :: Lens' DocumentMetadata [Text]
dmLabels = lens _dmLabels (\ s a -> s{_dmLabels = a}) . _Default . _Coerce

-- | The resource state.
dmResourceState :: Lens' DocumentMetadata (Maybe ResourceStateType)
dmResourceState = lens _dmResourceState (\ s a -> s{_dmResourceState = a})

-- | The time when the document was created.
dmCreatedTimestamp :: Lens' DocumentMetadata (Maybe UTCTime)
dmCreatedTimestamp = lens _dmCreatedTimestamp (\ s a -> s{_dmCreatedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
dmCreatorId :: Lens' DocumentMetadata (Maybe Text)
dmCreatorId = lens _dmCreatorId (\ s a -> s{_dmCreatorId = a})

instance FromJSON DocumentMetadata where
        parseJSON
          = withObject "DocumentMetadata"
              (\ x ->
                 DocumentMetadata' <$>
                   (x .:? "LatestVersionMetadata") <*>
                     (x .:? "ParentFolderId")
                     <*> (x .:? "ModifiedTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "Labels" .!= mempty)
                     <*> (x .:? "ResourceState")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "CreatorId"))

instance Hashable DocumentMetadata where

instance NFData DocumentMetadata where

-- | Describes a version of a document.
--
--
--
-- /See:/ 'documentVersionMetadata' smart constructor.
data DocumentVersionMetadata = DocumentVersionMetadata'
  { _dvmThumbnail :: !(Maybe (Map DocumentThumbnailType (Sensitive Text)))
  , _dvmStatus :: !(Maybe DocumentStatusType)
  , _dvmSignature :: !(Maybe Text)
  , _dvmContentCreatedTimestamp :: !(Maybe POSIX)
  , _dvmSize :: !(Maybe Integer)
  , _dvmName :: !(Maybe Text)
  , _dvmModifiedTimestamp :: !(Maybe POSIX)
  , _dvmSource :: !(Maybe (Map DocumentSourceType (Sensitive Text)))
  , _dvmId :: !(Maybe Text)
  , _dvmCreatedTimestamp :: !(Maybe POSIX)
  , _dvmContentModifiedTimestamp :: !(Maybe POSIX)
  , _dvmCreatorId :: !(Maybe Text)
  , _dvmContentType :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DocumentVersionMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvmThumbnail' - The thumbnail of the document.
--
-- * 'dvmStatus' - The status of the document.
--
-- * 'dvmSignature' - The signature of the document.
--
-- * 'dvmContentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- * 'dvmSize' - The size of the document, in bytes.
--
-- * 'dvmName' - The name of the version.
--
-- * 'dvmModifiedTimestamp' - The timestamp when the document was last uploaded.
--
-- * 'dvmSource' - The source of the document.
--
-- * 'dvmId' - The ID of the version.
--
-- * 'dvmCreatedTimestamp' - The timestamp when the document was first uploaded.
--
-- * 'dvmContentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- * 'dvmCreatorId' - The ID of the creator.
--
-- * 'dvmContentType' - The content type of the document.
documentVersionMetadata
    :: DocumentVersionMetadata
documentVersionMetadata =
  DocumentVersionMetadata'
    { _dvmThumbnail = Nothing
    , _dvmStatus = Nothing
    , _dvmSignature = Nothing
    , _dvmContentCreatedTimestamp = Nothing
    , _dvmSize = Nothing
    , _dvmName = Nothing
    , _dvmModifiedTimestamp = Nothing
    , _dvmSource = Nothing
    , _dvmId = Nothing
    , _dvmCreatedTimestamp = Nothing
    , _dvmContentModifiedTimestamp = Nothing
    , _dvmCreatorId = Nothing
    , _dvmContentType = Nothing
    }


-- | The thumbnail of the document.
dvmThumbnail :: Lens' DocumentVersionMetadata (HashMap DocumentThumbnailType Text)
dvmThumbnail = lens _dvmThumbnail (\ s a -> s{_dvmThumbnail = a}) . _Default . _Map

-- | The status of the document.
dvmStatus :: Lens' DocumentVersionMetadata (Maybe DocumentStatusType)
dvmStatus = lens _dvmStatus (\ s a -> s{_dvmStatus = a})

-- | The signature of the document.
dvmSignature :: Lens' DocumentVersionMetadata (Maybe Text)
dvmSignature = lens _dvmSignature (\ s a -> s{_dvmSignature = a})

-- | The timestamp when the content of the document was originally created.
dvmContentCreatedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmContentCreatedTimestamp = lens _dvmContentCreatedTimestamp (\ s a -> s{_dvmContentCreatedTimestamp = a}) . mapping _Time

-- | The size of the document, in bytes.
dvmSize :: Lens' DocumentVersionMetadata (Maybe Integer)
dvmSize = lens _dvmSize (\ s a -> s{_dvmSize = a})

-- | The name of the version.
dvmName :: Lens' DocumentVersionMetadata (Maybe Text)
dvmName = lens _dvmName (\ s a -> s{_dvmName = a})

-- | The timestamp when the document was last uploaded.
dvmModifiedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmModifiedTimestamp = lens _dvmModifiedTimestamp (\ s a -> s{_dvmModifiedTimestamp = a}) . mapping _Time

-- | The source of the document.
dvmSource :: Lens' DocumentVersionMetadata (HashMap DocumentSourceType Text)
dvmSource = lens _dvmSource (\ s a -> s{_dvmSource = a}) . _Default . _Map

-- | The ID of the version.
dvmId :: Lens' DocumentVersionMetadata (Maybe Text)
dvmId = lens _dvmId (\ s a -> s{_dvmId = a})

-- | The timestamp when the document was first uploaded.
dvmCreatedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmCreatedTimestamp = lens _dvmCreatedTimestamp (\ s a -> s{_dvmCreatedTimestamp = a}) . mapping _Time

-- | The timestamp when the content of the document was modified.
dvmContentModifiedTimestamp :: Lens' DocumentVersionMetadata (Maybe UTCTime)
dvmContentModifiedTimestamp = lens _dvmContentModifiedTimestamp (\ s a -> s{_dvmContentModifiedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
dvmCreatorId :: Lens' DocumentVersionMetadata (Maybe Text)
dvmCreatorId = lens _dvmCreatorId (\ s a -> s{_dvmCreatorId = a})

-- | The content type of the document.
dvmContentType :: Lens' DocumentVersionMetadata (Maybe Text)
dvmContentType = lens _dvmContentType (\ s a -> s{_dvmContentType = a})

instance FromJSON DocumentVersionMetadata where
        parseJSON
          = withObject "DocumentVersionMetadata"
              (\ x ->
                 DocumentVersionMetadata' <$>
                   (x .:? "Thumbnail" .!= mempty) <*> (x .:? "Status")
                     <*> (x .:? "Signature")
                     <*> (x .:? "ContentCreatedTimestamp")
                     <*> (x .:? "Size")
                     <*> (x .:? "Name")
                     <*> (x .:? "ModifiedTimestamp")
                     <*> (x .:? "Source" .!= mempty)
                     <*> (x .:? "Id")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "ContentModifiedTimestamp")
                     <*> (x .:? "CreatorId")
                     <*> (x .:? "ContentType"))

instance Hashable DocumentVersionMetadata where

instance NFData DocumentVersionMetadata where

-- | Describes a folder.
--
--
--
-- /See:/ 'folderMetadata' smart constructor.
data FolderMetadata = FolderMetadata'
  { _fmSignature         :: !(Maybe Text)
  , _fmParentFolderId    :: !(Maybe Text)
  , _fmSize              :: !(Maybe Integer)
  , _fmLatestVersionSize :: !(Maybe Integer)
  , _fmName              :: !(Maybe Text)
  , _fmModifiedTimestamp :: !(Maybe POSIX)
  , _fmId                :: !(Maybe Text)
  , _fmLabels            :: !(Maybe [Text])
  , _fmResourceState     :: !(Maybe ResourceStateType)
  , _fmCreatedTimestamp  :: !(Maybe POSIX)
  , _fmCreatorId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FolderMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmSignature' - The unique identifier created from the subfolders and documents of the folder.
--
-- * 'fmParentFolderId' - The ID of the parent folder.
--
-- * 'fmSize' - The size of the folder metadata.
--
-- * 'fmLatestVersionSize' - The size of the latest version of the folder metadata.
--
-- * 'fmName' - The name of the folder.
--
-- * 'fmModifiedTimestamp' - The time when the folder was updated.
--
-- * 'fmId' - The ID of the folder.
--
-- * 'fmLabels' - List of labels on the folder.
--
-- * 'fmResourceState' - The resource state of the folder.
--
-- * 'fmCreatedTimestamp' - The time when the folder was created.
--
-- * 'fmCreatorId' - The ID of the creator.
folderMetadata
    :: FolderMetadata
folderMetadata =
  FolderMetadata'
    { _fmSignature = Nothing
    , _fmParentFolderId = Nothing
    , _fmSize = Nothing
    , _fmLatestVersionSize = Nothing
    , _fmName = Nothing
    , _fmModifiedTimestamp = Nothing
    , _fmId = Nothing
    , _fmLabels = Nothing
    , _fmResourceState = Nothing
    , _fmCreatedTimestamp = Nothing
    , _fmCreatorId = Nothing
    }


-- | The unique identifier created from the subfolders and documents of the folder.
fmSignature :: Lens' FolderMetadata (Maybe Text)
fmSignature = lens _fmSignature (\ s a -> s{_fmSignature = a})

-- | The ID of the parent folder.
fmParentFolderId :: Lens' FolderMetadata (Maybe Text)
fmParentFolderId = lens _fmParentFolderId (\ s a -> s{_fmParentFolderId = a})

-- | The size of the folder metadata.
fmSize :: Lens' FolderMetadata (Maybe Integer)
fmSize = lens _fmSize (\ s a -> s{_fmSize = a})

-- | The size of the latest version of the folder metadata.
fmLatestVersionSize :: Lens' FolderMetadata (Maybe Integer)
fmLatestVersionSize = lens _fmLatestVersionSize (\ s a -> s{_fmLatestVersionSize = a})

-- | The name of the folder.
fmName :: Lens' FolderMetadata (Maybe Text)
fmName = lens _fmName (\ s a -> s{_fmName = a})

-- | The time when the folder was updated.
fmModifiedTimestamp :: Lens' FolderMetadata (Maybe UTCTime)
fmModifiedTimestamp = lens _fmModifiedTimestamp (\ s a -> s{_fmModifiedTimestamp = a}) . mapping _Time

-- | The ID of the folder.
fmId :: Lens' FolderMetadata (Maybe Text)
fmId = lens _fmId (\ s a -> s{_fmId = a})

-- | List of labels on the folder.
fmLabels :: Lens' FolderMetadata [Text]
fmLabels = lens _fmLabels (\ s a -> s{_fmLabels = a}) . _Default . _Coerce

-- | The resource state of the folder.
fmResourceState :: Lens' FolderMetadata (Maybe ResourceStateType)
fmResourceState = lens _fmResourceState (\ s a -> s{_fmResourceState = a})

-- | The time when the folder was created.
fmCreatedTimestamp :: Lens' FolderMetadata (Maybe UTCTime)
fmCreatedTimestamp = lens _fmCreatedTimestamp (\ s a -> s{_fmCreatedTimestamp = a}) . mapping _Time

-- | The ID of the creator.
fmCreatorId :: Lens' FolderMetadata (Maybe Text)
fmCreatorId = lens _fmCreatorId (\ s a -> s{_fmCreatorId = a})

instance FromJSON FolderMetadata where
        parseJSON
          = withObject "FolderMetadata"
              (\ x ->
                 FolderMetadata' <$>
                   (x .:? "Signature") <*> (x .:? "ParentFolderId") <*>
                     (x .:? "Size")
                     <*> (x .:? "LatestVersionSize")
                     <*> (x .:? "Name")
                     <*> (x .:? "ModifiedTimestamp")
                     <*> (x .:? "Id")
                     <*> (x .:? "Labels" .!= mempty)
                     <*> (x .:? "ResourceState")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "CreatorId"))

instance Hashable FolderMetadata where

instance NFData FolderMetadata where

-- | Describes the metadata of a user group.
--
--
--
-- /See:/ 'groupMetadata' smart constructor.
data GroupMetadata = GroupMetadata'
  { _gmName :: !(Maybe Text)
  , _gmId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GroupMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmName' - The name of the group.
--
-- * 'gmId' - The ID of the user group.
groupMetadata
    :: GroupMetadata
groupMetadata = GroupMetadata' {_gmName = Nothing, _gmId = Nothing}


-- | The name of the group.
gmName :: Lens' GroupMetadata (Maybe Text)
gmName = lens _gmName (\ s a -> s{_gmName = a})

-- | The ID of the user group.
gmId :: Lens' GroupMetadata (Maybe Text)
gmId = lens _gmId (\ s a -> s{_gmId = a})

instance FromJSON GroupMetadata where
        parseJSON
          = withObject "GroupMetadata"
              (\ x ->
                 GroupMetadata' <$> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable GroupMetadata where

instance NFData GroupMetadata where

-- | Set of options which defines notification preferences of given action.
--
--
--
-- /See:/ 'notificationOptions' smart constructor.
data NotificationOptions = NotificationOptions'
  { _noEmailMessage :: !(Maybe (Sensitive Text))
  , _noSendEmail    :: !(Maybe Bool)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'noEmailMessage' - Text value to be included in the email body.
--
-- * 'noSendEmail' - Boolean value to indicate an email notification should be sent to the receipients.
notificationOptions
    :: NotificationOptions
notificationOptions =
  NotificationOptions' {_noEmailMessage = Nothing, _noSendEmail = Nothing}


-- | Text value to be included in the email body.
noEmailMessage :: Lens' NotificationOptions (Maybe Text)
noEmailMessage = lens _noEmailMessage (\ s a -> s{_noEmailMessage = a}) . mapping _Sensitive

-- | Boolean value to indicate an email notification should be sent to the receipients.
noSendEmail :: Lens' NotificationOptions (Maybe Bool)
noSendEmail = lens _noSendEmail (\ s a -> s{_noSendEmail = a})

instance Hashable NotificationOptions where

instance NFData NotificationOptions where

instance ToJSON NotificationOptions where
        toJSON NotificationOptions'{..}
          = object
              (catMaybes
                 [("EmailMessage" .=) <$> _noEmailMessage,
                  ("SendEmail" .=) <$> _noSendEmail])

-- | Describes the users or user groups.
--
--
--
-- /See:/ 'participants' smart constructor.
data Participants = Participants'
  { _pGroups :: !(Maybe [GroupMetadata])
  , _pUsers  :: !(Maybe [UserMetadata])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Participants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pGroups' - The list of user groups.
--
-- * 'pUsers' - The list of users.
participants
    :: Participants
participants = Participants' {_pGroups = Nothing, _pUsers = Nothing}


-- | The list of user groups.
pGroups :: Lens' Participants [GroupMetadata]
pGroups = lens _pGroups (\ s a -> s{_pGroups = a}) . _Default . _Coerce

-- | The list of users.
pUsers :: Lens' Participants [UserMetadata]
pUsers = lens _pUsers (\ s a -> s{_pUsers = a}) . _Default . _Coerce

instance FromJSON Participants where
        parseJSON
          = withObject "Participants"
              (\ x ->
                 Participants' <$>
                   (x .:? "Groups" .!= mempty) <*>
                     (x .:? "Users" .!= mempty))

instance Hashable Participants where

instance NFData Participants where

-- | Describes the permissions.
--
--
--
-- /See:/ 'permissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { _piRole :: !(Maybe RoleType)
  , _piType :: !(Maybe RolePermissionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PermissionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piRole' - The role of the user.
--
-- * 'piType' - The type of permissions.
permissionInfo
    :: PermissionInfo
permissionInfo = PermissionInfo' {_piRole = Nothing, _piType = Nothing}


-- | The role of the user.
piRole :: Lens' PermissionInfo (Maybe RoleType)
piRole = lens _piRole (\ s a -> s{_piRole = a})

-- | The type of permissions.
piType :: Lens' PermissionInfo (Maybe RolePermissionType)
piType = lens _piType (\ s a -> s{_piType = a})

instance FromJSON PermissionInfo where
        parseJSON
          = withObject "PermissionInfo"
              (\ x ->
                 PermissionInfo' <$>
                   (x .:? "Role") <*> (x .:? "Type"))

instance Hashable PermissionInfo where

instance NFData PermissionInfo where

-- | Describes a resource.
--
--
--
-- /See:/ 'principal' smart constructor.
data Principal = Principal'
  { _pRoles :: !(Maybe [PermissionInfo])
  , _pId    :: !(Maybe Text)
  , _pType  :: !(Maybe PrincipalType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pRoles' - The permission information for the resource.
--
-- * 'pId' - The ID of the resource.
--
-- * 'pType' - The type of resource.
principal
    :: Principal
principal = Principal' {_pRoles = Nothing, _pId = Nothing, _pType = Nothing}


-- | The permission information for the resource.
pRoles :: Lens' Principal [PermissionInfo]
pRoles = lens _pRoles (\ s a -> s{_pRoles = a}) . _Default . _Coerce

-- | The ID of the resource.
pId :: Lens' Principal (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

-- | The type of resource.
pType :: Lens' Principal (Maybe PrincipalType)
pType = lens _pType (\ s a -> s{_pType = a})

instance FromJSON Principal where
        parseJSON
          = withObject "Principal"
              (\ x ->
                 Principal' <$>
                   (x .:? "Roles" .!= mempty) <*> (x .:? "Id") <*>
                     (x .:? "Type"))

instance Hashable Principal where

instance NFData Principal where

-- | Describes the metadata of a resource.
--
--
--
-- /See:/ 'resourceMetadata' smart constructor.
data ResourceMetadata = ResourceMetadata'
  { _rmVersionId    :: !(Maybe Text)
  , _rmOwner        :: !(Maybe UserMetadata)
  , _rmName         :: !(Maybe Text)
  , _rmId           :: !(Maybe Text)
  , _rmType         :: !(Maybe ResourceType)
  , _rmOriginalName :: !(Maybe Text)
  , _rmParentId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmVersionId' - The version ID of the resource. This is an optional field and is filled for action on document version.
--
-- * 'rmOwner' - The owner of the resource.
--
-- * 'rmName' - The name of the resource.
--
-- * 'rmId' - The ID of the resource.
--
-- * 'rmType' - The type of resource.
--
-- * 'rmOriginalName' - The original name of the resource before a rename operation.
--
-- * 'rmParentId' - The parent ID of the resource before a rename operation.
resourceMetadata
    :: ResourceMetadata
resourceMetadata =
  ResourceMetadata'
    { _rmVersionId = Nothing
    , _rmOwner = Nothing
    , _rmName = Nothing
    , _rmId = Nothing
    , _rmType = Nothing
    , _rmOriginalName = Nothing
    , _rmParentId = Nothing
    }


-- | The version ID of the resource. This is an optional field and is filled for action on document version.
rmVersionId :: Lens' ResourceMetadata (Maybe Text)
rmVersionId = lens _rmVersionId (\ s a -> s{_rmVersionId = a})

-- | The owner of the resource.
rmOwner :: Lens' ResourceMetadata (Maybe UserMetadata)
rmOwner = lens _rmOwner (\ s a -> s{_rmOwner = a})

-- | The name of the resource.
rmName :: Lens' ResourceMetadata (Maybe Text)
rmName = lens _rmName (\ s a -> s{_rmName = a})

-- | The ID of the resource.
rmId :: Lens' ResourceMetadata (Maybe Text)
rmId = lens _rmId (\ s a -> s{_rmId = a})

-- | The type of resource.
rmType :: Lens' ResourceMetadata (Maybe ResourceType)
rmType = lens _rmType (\ s a -> s{_rmType = a})

-- | The original name of the resource before a rename operation.
rmOriginalName :: Lens' ResourceMetadata (Maybe Text)
rmOriginalName = lens _rmOriginalName (\ s a -> s{_rmOriginalName = a})

-- | The parent ID of the resource before a rename operation.
rmParentId :: Lens' ResourceMetadata (Maybe Text)
rmParentId = lens _rmParentId (\ s a -> s{_rmParentId = a})

instance FromJSON ResourceMetadata where
        parseJSON
          = withObject "ResourceMetadata"
              (\ x ->
                 ResourceMetadata' <$>
                   (x .:? "VersionId") <*> (x .:? "Owner") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "OriginalName")
                     <*> (x .:? "ParentId"))

instance Hashable ResourceMetadata where

instance NFData ResourceMetadata where

-- | Describes the path information of a resource.
--
--
--
-- /See:/ 'resourcePath' smart constructor.
newtype ResourcePath = ResourcePath'
  { _rpComponents :: Maybe [ResourcePathComponent]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourcePath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpComponents' - The components of the resource path.
resourcePath
    :: ResourcePath
resourcePath = ResourcePath' {_rpComponents = Nothing}


-- | The components of the resource path.
rpComponents :: Lens' ResourcePath [ResourcePathComponent]
rpComponents = lens _rpComponents (\ s a -> s{_rpComponents = a}) . _Default . _Coerce

instance FromJSON ResourcePath where
        parseJSON
          = withObject "ResourcePath"
              (\ x ->
                 ResourcePath' <$> (x .:? "Components" .!= mempty))

instance Hashable ResourcePath where

instance NFData ResourcePath where

-- | Describes the resource path.
--
--
--
-- /See:/ 'resourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { _rpcName :: !(Maybe Text)
  , _rpcId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourcePathComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcName' - The name of the resource path.
--
-- * 'rpcId' - The ID of the resource path.
resourcePathComponent
    :: ResourcePathComponent
resourcePathComponent =
  ResourcePathComponent' {_rpcName = Nothing, _rpcId = Nothing}


-- | The name of the resource path.
rpcName :: Lens' ResourcePathComponent (Maybe Text)
rpcName = lens _rpcName (\ s a -> s{_rpcName = a})

-- | The ID of the resource path.
rpcId :: Lens' ResourcePathComponent (Maybe Text)
rpcId = lens _rpcId (\ s a -> s{_rpcId = a})

instance FromJSON ResourcePathComponent where
        parseJSON
          = withObject "ResourcePathComponent"
              (\ x ->
                 ResourcePathComponent' <$>
                   (x .:? "Name") <*> (x .:? "Id"))

instance Hashable ResourcePathComponent where

instance NFData ResourcePathComponent where

-- | Describes the recipient type and ID, if available.
--
--
--
-- /See:/ 'sharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { _spId   :: !Text
  , _spType :: !PrincipalType
  , _spRole :: !RoleType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SharePrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spId' - The ID of the recipient.
--
-- * 'spType' - The type of the recipient.
--
-- * 'spRole' - The role of the recipient.
sharePrincipal
    :: Text -- ^ 'spId'
    -> PrincipalType -- ^ 'spType'
    -> RoleType -- ^ 'spRole'
    -> SharePrincipal
sharePrincipal pId_ pType_ pRole_ =
  SharePrincipal' {_spId = pId_, _spType = pType_, _spRole = pRole_}


-- | The ID of the recipient.
spId :: Lens' SharePrincipal Text
spId = lens _spId (\ s a -> s{_spId = a})

-- | The type of the recipient.
spType :: Lens' SharePrincipal PrincipalType
spType = lens _spType (\ s a -> s{_spType = a})

-- | The role of the recipient.
spRole :: Lens' SharePrincipal RoleType
spRole = lens _spRole (\ s a -> s{_spRole = a})

instance Hashable SharePrincipal where

instance NFData SharePrincipal where

instance ToJSON SharePrincipal where
        toJSON SharePrincipal'{..}
          = object
              (catMaybes
                 [Just ("Id" .= _spId), Just ("Type" .= _spType),
                  Just ("Role" .= _spRole)])

-- | Describes the share results of a resource.
--
--
--
-- /See:/ 'shareResult' smart constructor.
data ShareResult = ShareResult'
  { _srStatus        :: !(Maybe ShareStatusType)
  , _srPrincipalId   :: !(Maybe Text)
  , _srRole          :: !(Maybe RoleType)
  , _srStatusMessage :: !(Maybe (Sensitive Text))
  , _srShareId       :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShareResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - The status.
--
-- * 'srPrincipalId' - The ID of the principal.
--
-- * 'srRole' - The role.
--
-- * 'srStatusMessage' - The status message.
--
-- * 'srShareId' - The ID of the resource that was shared.
shareResult
    :: ShareResult
shareResult =
  ShareResult'
    { _srStatus = Nothing
    , _srPrincipalId = Nothing
    , _srRole = Nothing
    , _srStatusMessage = Nothing
    , _srShareId = Nothing
    }


-- | The status.
srStatus :: Lens' ShareResult (Maybe ShareStatusType)
srStatus = lens _srStatus (\ s a -> s{_srStatus = a})

-- | The ID of the principal.
srPrincipalId :: Lens' ShareResult (Maybe Text)
srPrincipalId = lens _srPrincipalId (\ s a -> s{_srPrincipalId = a})

-- | The role.
srRole :: Lens' ShareResult (Maybe RoleType)
srRole = lens _srRole (\ s a -> s{_srRole = a})

-- | The status message.
srStatusMessage :: Lens' ShareResult (Maybe Text)
srStatusMessage = lens _srStatusMessage (\ s a -> s{_srStatusMessage = a}) . mapping _Sensitive

-- | The ID of the resource that was shared.
srShareId :: Lens' ShareResult (Maybe Text)
srShareId = lens _srShareId (\ s a -> s{_srShareId = a})

instance FromJSON ShareResult where
        parseJSON
          = withObject "ShareResult"
              (\ x ->
                 ShareResult' <$>
                   (x .:? "Status") <*> (x .:? "PrincipalId") <*>
                     (x .:? "Role")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "ShareId"))

instance Hashable ShareResult where

instance NFData ShareResult where

-- | Describes the storage for a user.
--
--
--
-- /See:/ 'storageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { _srtStorageAllocatedInBytes :: !(Maybe Nat)
  , _srtStorageType             :: !(Maybe StorageType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageRuleType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtStorageAllocatedInBytes' - The amount of storage allocated, in bytes.
--
-- * 'srtStorageType' - The type of storage.
storageRuleType
    :: StorageRuleType
storageRuleType =
  StorageRuleType'
    {_srtStorageAllocatedInBytes = Nothing, _srtStorageType = Nothing}


-- | The amount of storage allocated, in bytes.
srtStorageAllocatedInBytes :: Lens' StorageRuleType (Maybe Natural)
srtStorageAllocatedInBytes = lens _srtStorageAllocatedInBytes (\ s a -> s{_srtStorageAllocatedInBytes = a}) . mapping _Nat

-- | The type of storage.
srtStorageType :: Lens' StorageRuleType (Maybe StorageType)
srtStorageType = lens _srtStorageType (\ s a -> s{_srtStorageType = a})

instance FromJSON StorageRuleType where
        parseJSON
          = withObject "StorageRuleType"
              (\ x ->
                 StorageRuleType' <$>
                   (x .:? "StorageAllocatedInBytes") <*>
                     (x .:? "StorageType"))

instance Hashable StorageRuleType where

instance NFData StorageRuleType where

instance ToJSON StorageRuleType where
        toJSON StorageRuleType'{..}
          = object
              (catMaybes
                 [("StorageAllocatedInBytes" .=) <$>
                    _srtStorageAllocatedInBytes,
                  ("StorageType" .=) <$> _srtStorageType])

-- | Describes a subscription.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sProtocol       :: !(Maybe SubscriptionProtocolType)
  , _sEndPoint       :: !(Maybe Text)
  , _sSubscriptionId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProtocol' - The protocol of the subscription.
--
-- * 'sEndPoint' - The endpoint of the subscription.
--
-- * 'sSubscriptionId' - The ID of the subscription.
subscription
    :: Subscription
subscription =
  Subscription'
    {_sProtocol = Nothing, _sEndPoint = Nothing, _sSubscriptionId = Nothing}


-- | The protocol of the subscription.
sProtocol :: Lens' Subscription (Maybe SubscriptionProtocolType)
sProtocol = lens _sProtocol (\ s a -> s{_sProtocol = a})

-- | The endpoint of the subscription.
sEndPoint :: Lens' Subscription (Maybe Text)
sEndPoint = lens _sEndPoint (\ s a -> s{_sEndPoint = a})

-- | The ID of the subscription.
sSubscriptionId :: Lens' Subscription (Maybe Text)
sSubscriptionId = lens _sSubscriptionId (\ s a -> s{_sSubscriptionId = a})

instance FromJSON Subscription where
        parseJSON
          = withObject "Subscription"
              (\ x ->
                 Subscription' <$>
                   (x .:? "Protocol") <*> (x .:? "EndPoint") <*>
                     (x .:? "SubscriptionId"))

instance Hashable Subscription where

instance NFData Subscription where

-- | Describes the upload.
--
--
--
-- /See:/ 'uploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { _umUploadURL     :: !(Maybe (Sensitive Text))
  , _umSignedHeaders :: !(Maybe (Map Text Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umUploadURL' - The URL of the upload.
--
-- * 'umSignedHeaders' - The signed headers.
uploadMetadata
    :: UploadMetadata
uploadMetadata =
  UploadMetadata' {_umUploadURL = Nothing, _umSignedHeaders = Nothing}


-- | The URL of the upload.
umUploadURL :: Lens' UploadMetadata (Maybe Text)
umUploadURL = lens _umUploadURL (\ s a -> s{_umUploadURL = a}) . mapping _Sensitive

-- | The signed headers.
umSignedHeaders :: Lens' UploadMetadata (HashMap Text Text)
umSignedHeaders = lens _umSignedHeaders (\ s a -> s{_umSignedHeaders = a}) . _Default . _Map

instance FromJSON UploadMetadata where
        parseJSON
          = withObject "UploadMetadata"
              (\ x ->
                 UploadMetadata' <$>
                   (x .:? "UploadUrl") <*>
                     (x .:? "SignedHeaders" .!= mempty))

instance Hashable UploadMetadata where

instance NFData UploadMetadata where

-- | Describes a user.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uGivenName          :: !(Maybe Text)
  , _uStatus             :: !(Maybe UserStatusType)
  , _uLocale             :: !(Maybe LocaleType)
  , _uUsername           :: !(Maybe Text)
  , _uStorage            :: !(Maybe UserStorageMetadata)
  , _uModifiedTimestamp  :: !(Maybe POSIX)
  , _uEmailAddress       :: !(Maybe Text)
  , _uId                 :: !(Maybe Text)
  , _uRootFolderId       :: !(Maybe Text)
  , _uType               :: !(Maybe UserType)
  , _uSurname            :: !(Maybe Text)
  , _uTimeZoneId         :: !(Maybe Text)
  , _uCreatedTimestamp   :: !(Maybe POSIX)
  , _uOrganizationId     :: !(Maybe Text)
  , _uRecycleBinFolderId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uGivenName' - The given name of the user.
--
-- * 'uStatus' - The status of the user.
--
-- * 'uLocale' - The locale of the user.
--
-- * 'uUsername' - The login name of the user.
--
-- * 'uStorage' - The storage for the user.
--
-- * 'uModifiedTimestamp' - The time when the user was modified.
--
-- * 'uEmailAddress' - The email address of the user.
--
-- * 'uId' - The ID of the user.
--
-- * 'uRootFolderId' - The ID of the root folder.
--
-- * 'uType' - The type of user.
--
-- * 'uSurname' - The surname of the user.
--
-- * 'uTimeZoneId' - The time zone ID of the user.
--
-- * 'uCreatedTimestamp' - The time when the user was created.
--
-- * 'uOrganizationId' - The ID of the organization.
--
-- * 'uRecycleBinFolderId' - The ID of the recycle bin folder.
user
    :: User
user =
  User'
    { _uGivenName = Nothing
    , _uStatus = Nothing
    , _uLocale = Nothing
    , _uUsername = Nothing
    , _uStorage = Nothing
    , _uModifiedTimestamp = Nothing
    , _uEmailAddress = Nothing
    , _uId = Nothing
    , _uRootFolderId = Nothing
    , _uType = Nothing
    , _uSurname = Nothing
    , _uTimeZoneId = Nothing
    , _uCreatedTimestamp = Nothing
    , _uOrganizationId = Nothing
    , _uRecycleBinFolderId = Nothing
    }


-- | The given name of the user.
uGivenName :: Lens' User (Maybe Text)
uGivenName = lens _uGivenName (\ s a -> s{_uGivenName = a})

-- | The status of the user.
uStatus :: Lens' User (Maybe UserStatusType)
uStatus = lens _uStatus (\ s a -> s{_uStatus = a})

-- | The locale of the user.
uLocale :: Lens' User (Maybe LocaleType)
uLocale = lens _uLocale (\ s a -> s{_uLocale = a})

-- | The login name of the user.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\ s a -> s{_uUsername = a})

-- | The storage for the user.
uStorage :: Lens' User (Maybe UserStorageMetadata)
uStorage = lens _uStorage (\ s a -> s{_uStorage = a})

-- | The time when the user was modified.
uModifiedTimestamp :: Lens' User (Maybe UTCTime)
uModifiedTimestamp = lens _uModifiedTimestamp (\ s a -> s{_uModifiedTimestamp = a}) . mapping _Time

-- | The email address of the user.
uEmailAddress :: Lens' User (Maybe Text)
uEmailAddress = lens _uEmailAddress (\ s a -> s{_uEmailAddress = a})

-- | The ID of the user.
uId :: Lens' User (Maybe Text)
uId = lens _uId (\ s a -> s{_uId = a})

-- | The ID of the root folder.
uRootFolderId :: Lens' User (Maybe Text)
uRootFolderId = lens _uRootFolderId (\ s a -> s{_uRootFolderId = a})

-- | The type of user.
uType :: Lens' User (Maybe UserType)
uType = lens _uType (\ s a -> s{_uType = a})

-- | The surname of the user.
uSurname :: Lens' User (Maybe Text)
uSurname = lens _uSurname (\ s a -> s{_uSurname = a})

-- | The time zone ID of the user.
uTimeZoneId :: Lens' User (Maybe Text)
uTimeZoneId = lens _uTimeZoneId (\ s a -> s{_uTimeZoneId = a})

-- | The time when the user was created.
uCreatedTimestamp :: Lens' User (Maybe UTCTime)
uCreatedTimestamp = lens _uCreatedTimestamp (\ s a -> s{_uCreatedTimestamp = a}) . mapping _Time

-- | The ID of the organization.
uOrganizationId :: Lens' User (Maybe Text)
uOrganizationId = lens _uOrganizationId (\ s a -> s{_uOrganizationId = a})

-- | The ID of the recycle bin folder.
uRecycleBinFolderId :: Lens' User (Maybe Text)
uRecycleBinFolderId = lens _uRecycleBinFolderId (\ s a -> s{_uRecycleBinFolderId = a})

instance FromJSON User where
        parseJSON
          = withObject "User"
              (\ x ->
                 User' <$>
                   (x .:? "GivenName") <*> (x .:? "Status") <*>
                     (x .:? "Locale")
                     <*> (x .:? "Username")
                     <*> (x .:? "Storage")
                     <*> (x .:? "ModifiedTimestamp")
                     <*> (x .:? "EmailAddress")
                     <*> (x .:? "Id")
                     <*> (x .:? "RootFolderId")
                     <*> (x .:? "Type")
                     <*> (x .:? "Surname")
                     <*> (x .:? "TimeZoneId")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "OrganizationId")
                     <*> (x .:? "RecycleBinFolderId"))

instance Hashable User where

instance NFData User where

-- | Describes the metadata of the user.
--
--
--
-- /See:/ 'userMetadata' smart constructor.
data UserMetadata = UserMetadata'
  { _umGivenName    :: !(Maybe Text)
  , _umUsername     :: !(Maybe Text)
  , _umEmailAddress :: !(Maybe Text)
  , _umId           :: !(Maybe Text)
  , _umSurname      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umGivenName' - The given name of the user before a rename operation.
--
-- * 'umUsername' - The name of the user.
--
-- * 'umEmailAddress' - The email address of the user.
--
-- * 'umId' - The ID of the user.
--
-- * 'umSurname' - The surname of the user.
userMetadata
    :: UserMetadata
userMetadata =
  UserMetadata'
    { _umGivenName = Nothing
    , _umUsername = Nothing
    , _umEmailAddress = Nothing
    , _umId = Nothing
    , _umSurname = Nothing
    }


-- | The given name of the user before a rename operation.
umGivenName :: Lens' UserMetadata (Maybe Text)
umGivenName = lens _umGivenName (\ s a -> s{_umGivenName = a})

-- | The name of the user.
umUsername :: Lens' UserMetadata (Maybe Text)
umUsername = lens _umUsername (\ s a -> s{_umUsername = a})

-- | The email address of the user.
umEmailAddress :: Lens' UserMetadata (Maybe Text)
umEmailAddress = lens _umEmailAddress (\ s a -> s{_umEmailAddress = a})

-- | The ID of the user.
umId :: Lens' UserMetadata (Maybe Text)
umId = lens _umId (\ s a -> s{_umId = a})

-- | The surname of the user.
umSurname :: Lens' UserMetadata (Maybe Text)
umSurname = lens _umSurname (\ s a -> s{_umSurname = a})

instance FromJSON UserMetadata where
        parseJSON
          = withObject "UserMetadata"
              (\ x ->
                 UserMetadata' <$>
                   (x .:? "GivenName") <*> (x .:? "Username") <*>
                     (x .:? "EmailAddress")
                     <*> (x .:? "Id")
                     <*> (x .:? "Surname"))

instance Hashable UserMetadata where

instance NFData UserMetadata where

-- | Describes the storage for a user.
--
--
--
-- /See:/ 'userStorageMetadata' smart constructor.
data UserStorageMetadata = UserStorageMetadata'
  { _usmStorageUtilizedInBytes :: !(Maybe Integer)
  , _usmStorageRule            :: !(Maybe StorageRuleType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserStorageMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usmStorageUtilizedInBytes' - The amount of storage used, in bytes.
--
-- * 'usmStorageRule' - The storage for a user.
userStorageMetadata
    :: UserStorageMetadata
userStorageMetadata =
  UserStorageMetadata'
    {_usmStorageUtilizedInBytes = Nothing, _usmStorageRule = Nothing}


-- | The amount of storage used, in bytes.
usmStorageUtilizedInBytes :: Lens' UserStorageMetadata (Maybe Integer)
usmStorageUtilizedInBytes = lens _usmStorageUtilizedInBytes (\ s a -> s{_usmStorageUtilizedInBytes = a})

-- | The storage for a user.
usmStorageRule :: Lens' UserStorageMetadata (Maybe StorageRuleType)
usmStorageRule = lens _usmStorageRule (\ s a -> s{_usmStorageRule = a})

instance FromJSON UserStorageMetadata where
        parseJSON
          = withObject "UserStorageMetadata"
              (\ x ->
                 UserStorageMetadata' <$>
                   (x .:? "StorageUtilizedInBytes") <*>
                     (x .:? "StorageRule"))

instance Hashable UserStorageMetadata where

instance NFData UserStorageMetadata where
