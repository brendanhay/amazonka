{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListParts
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the parts of an archive that have been uploaded in
-- a specific multipart upload. You can make this request at any time
-- during an in-progress multipart upload before you complete the upload
-- (see CompleteMultipartUpload. List Parts returns an error for completed
-- uploads. The list returned in the List Parts response is sorted by part
-- range.
--
-- The List Parts operation supports pagination. By default, this operation
-- returns up to 1,000 uploaded parts in the response. You should always
-- check the response for a @marker@ at which to continue the list; if
-- there are no more items the @marker@ is @null@. To return a list of
-- parts that begins at a specific part, set the @marker@ request parameter
-- to the value you obtained from a previous List Parts request. You can
-- also limit the number of parts returned in the response by specifying
-- the @limit@ parameter in the request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html List Parts>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListParts.html>
module Network.AWS.Glacier.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lpMarker
    , lpLimit
    , lpAccountId
    , lpVaultName
    , lpUploadId

    -- * Response
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprsParts
    , lprsMultipartUploadId
    , lprsArchiveDescription
    , lprsPartSizeInBytes
    , lprsVaultARN
    , lprsMarker
    , lprsCreationDate
    , lprsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for retrieving a list of parts of an archive that have
-- been uploaded in a specific multipart upload.
--
-- /See:/ 'listParts' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpMarker'
--
-- * 'lpLimit'
--
-- * 'lpAccountId'
--
-- * 'lpVaultName'
--
-- * 'lpUploadId'
data ListParts = ListParts'
    { _lpMarker    :: !(Maybe Text)
    , _lpLimit     :: !(Maybe Text)
    , _lpAccountId :: !Text
    , _lpVaultName :: !Text
    , _lpUploadId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListParts' smart constructor.
listParts :: Text -> Text -> Text -> ListParts
listParts pAccountId_ pVaultName_ pUploadId_ =
    ListParts'
    { _lpMarker = Nothing
    , _lpLimit = Nothing
    , _lpAccountId = pAccountId_
    , _lpVaultName = pVaultName_
    , _lpUploadId = pUploadId_
    }

-- | An opaque string used for pagination. This value specifies the part at
-- which the listing of parts should begin. Get the marker value from the
-- response of a previous List Parts response. You need only include the
-- marker if you are continuing the pagination of results started in a
-- previous List Parts request.
lpMarker :: Lens' ListParts (Maybe Text)
lpMarker = lens _lpMarker (\ s a -> s{_lpMarker = a});

-- | Specifies the maximum number of parts returned in the response body. If
-- this value is not specified, the List Parts operation returns up to
-- 1,000 uploads.
lpLimit :: Lens' ListParts (Maybe Text)
lpLimit = lens _lpLimit (\ s a -> s{_lpLimit = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
lpAccountId :: Lens' ListParts Text
lpAccountId = lens _lpAccountId (\ s a -> s{_lpAccountId = a});

-- | The name of the vault.
lpVaultName :: Lens' ListParts Text
lpVaultName = lens _lpVaultName (\ s a -> s{_lpVaultName = a});

-- | The upload ID of the multipart upload.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\ s a -> s{_lpUploadId = a});

instance AWSRequest ListParts where
        type Sv ListParts = Glacier
        type Rs ListParts = ListPartsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 ListPartsResponse' <$>
                   (x .?> "Parts" .!@ mempty) <*>
                     (x .?> "MultipartUploadId")
                     <*> (x .?> "ArchiveDescription")
                     <*> (x .?> "PartSizeInBytes")
                     <*> (x .?> "VaultARN")
                     <*> (x .?> "Marker")
                     <*> (x .?> "CreationDate")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListParts where
        toHeaders = const mempty

instance ToPath ListParts where
        toPath ListParts'{..}
          = mconcat
              ["/", toPath _lpAccountId, "/vaults/",
               toPath _lpVaultName, "/multipart-uploads/",
               toPath _lpUploadId]

instance ToQuery ListParts where
        toQuery ListParts'{..}
          = mconcat
              ["marker" =: _lpMarker, "limit" =: _lpLimit]

-- | Contains the Amazon Glacier response to your request.
--
-- /See:/ 'listPartsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprsParts'
--
-- * 'lprsMultipartUploadId'
--
-- * 'lprsArchiveDescription'
--
-- * 'lprsPartSizeInBytes'
--
-- * 'lprsVaultARN'
--
-- * 'lprsMarker'
--
-- * 'lprsCreationDate'
--
-- * 'lprsStatus'
data ListPartsResponse = ListPartsResponse'
    { _lprsParts              :: !(Maybe [PartListElement])
    , _lprsMultipartUploadId  :: !(Maybe Text)
    , _lprsArchiveDescription :: !(Maybe Text)
    , _lprsPartSizeInBytes    :: !(Maybe Integer)
    , _lprsVaultARN           :: !(Maybe Text)
    , _lprsMarker             :: !(Maybe Text)
    , _lprsCreationDate       :: !(Maybe Text)
    , _lprsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPartsResponse' smart constructor.
listPartsResponse :: Int -> ListPartsResponse
listPartsResponse pStatus_ =
    ListPartsResponse'
    { _lprsParts = Nothing
    , _lprsMultipartUploadId = Nothing
    , _lprsArchiveDescription = Nothing
    , _lprsPartSizeInBytes = Nothing
    , _lprsVaultARN = Nothing
    , _lprsMarker = Nothing
    , _lprsCreationDate = Nothing
    , _lprsStatus = pStatus_
    }

-- | A list of the part sizes of the multipart upload.
lprsParts :: Lens' ListPartsResponse [PartListElement]
lprsParts = lens _lprsParts (\ s a -> s{_lprsParts = a}) . _Default . _Coerce;

-- | The ID of the upload to which the parts are associated.
lprsMultipartUploadId :: Lens' ListPartsResponse (Maybe Text)
lprsMultipartUploadId = lens _lprsMultipartUploadId (\ s a -> s{_lprsMultipartUploadId = a});

-- | The description of the archive that was specified in the Initiate
-- Multipart Upload request.
lprsArchiveDescription :: Lens' ListPartsResponse (Maybe Text)
lprsArchiveDescription = lens _lprsArchiveDescription (\ s a -> s{_lprsArchiveDescription = a});

-- | The part size in bytes.
lprsPartSizeInBytes :: Lens' ListPartsResponse (Maybe Integer)
lprsPartSizeInBytes = lens _lprsPartSizeInBytes (\ s a -> s{_lprsPartSizeInBytes = a});

-- | The Amazon Resource Name (ARN) of the vault to which the multipart
-- upload was initiated.
lprsVaultARN :: Lens' ListPartsResponse (Maybe Text)
lprsVaultARN = lens _lprsVaultARN (\ s a -> s{_lprsVaultARN = a});

-- | An opaque string that represents where to continue pagination of the
-- results. You use the marker in a new List Parts request to obtain more
-- jobs in the list. If there are no more parts, this value is @null@.
lprsMarker :: Lens' ListPartsResponse (Maybe Text)
lprsMarker = lens _lprsMarker (\ s a -> s{_lprsMarker = a});

-- | The UTC time at which the multipart upload was initiated.
lprsCreationDate :: Lens' ListPartsResponse (Maybe Text)
lprsCreationDate = lens _lprsCreationDate (\ s a -> s{_lprsCreationDate = a});

-- | FIXME: Undocumented member.
lprsStatus :: Lens' ListPartsResponse Int
lprsStatus = lens _lprsStatus (\ s a -> s{_lprsStatus = a});
