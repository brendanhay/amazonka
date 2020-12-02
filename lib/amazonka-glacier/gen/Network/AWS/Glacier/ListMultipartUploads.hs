{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListMultipartUploads
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads for the specified vault. An in-progress multipart upload is a multipart upload that has been initiated by an 'InitiateMultipartUpload' request, but has not yet been completed or aborted. The list returned in the List Multipart Upload response has no guaranteed order.
--
--
-- The List Multipart Uploads operation supports pagination. By default, this operation returns up to 1,000 multipart uploads in the response. You should always check the response for a @marker@ at which to continue the list; if there are no more items the @marker@ is @null@ . To return a list of multipart uploads that begins at a specific upload, set the @marker@ request parameter to the value you obtained from a previous List Multipart Upload request. You can also limit the number of uploads returned in the response by specifying the @limit@ parameter in the request.
--
-- Note the difference between this operation and listing parts ('ListParts' ). The List Multipart Uploads operation lists all multipart uploads for a vault and does not require a multipart upload ID. The List Parts operation requires a multipart upload ID since parts are associated with a single upload.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and the underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html List Multipart Uploads > in the /Amazon Glacier Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListMultipartUploads
    (
    -- * Creating a Request
      listMultipartUploads
    , ListMultipartUploads
    -- * Request Lenses
    , lmuMarker
    , lmuLimit
    , lmuAccountId
    , lmuVaultName

    -- * Destructuring the Response
    , listMultipartUploadsResponse
    , ListMultipartUploadsResponse
    -- * Response Lenses
    , lmursUploadsList
    , lmursMarker
    , lmursResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for retrieving list of in-progress multipart uploads for an Amazon Glacier vault.
--
--
--
-- /See:/ 'listMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { _lmuMarker    :: !(Maybe Text)
  , _lmuLimit     :: !(Maybe Text)
  , _lmuAccountId :: !Text
  , _lmuVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMultipartUploads' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmuMarker' - An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
--
-- * 'lmuLimit' - Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 1,000 uploads.
--
-- * 'lmuAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'lmuVaultName' - The name of the vault.
listMultipartUploads
    :: Text -- ^ 'lmuAccountId'
    -> Text -- ^ 'lmuVaultName'
    -> ListMultipartUploads
listMultipartUploads pAccountId_ pVaultName_ =
  ListMultipartUploads'
    { _lmuMarker = Nothing
    , _lmuLimit = Nothing
    , _lmuAccountId = pAccountId_
    , _lmuVaultName = pVaultName_
    }


-- | An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
lmuMarker :: Lens' ListMultipartUploads (Maybe Text)
lmuMarker = lens _lmuMarker (\ s a -> s{_lmuMarker = a})

-- | Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 1,000 uploads.
lmuLimit :: Lens' ListMultipartUploads (Maybe Text)
lmuLimit = lens _lmuLimit (\ s a -> s{_lmuLimit = a})

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
lmuAccountId :: Lens' ListMultipartUploads Text
lmuAccountId = lens _lmuAccountId (\ s a -> s{_lmuAccountId = a})

-- | The name of the vault.
lmuVaultName :: Lens' ListMultipartUploads Text
lmuVaultName = lens _lmuVaultName (\ s a -> s{_lmuVaultName = a})

instance AWSPager ListMultipartUploads where
        page rq rs
          | stop (rs ^. lmursMarker) = Nothing
          | stop (rs ^. lmursUploadsList) = Nothing
          | otherwise =
            Just $ rq & lmuMarker .~ rs ^. lmursMarker

instance AWSRequest ListMultipartUploads where
        type Rs ListMultipartUploads =
             ListMultipartUploadsResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 ListMultipartUploadsResponse' <$>
                   (x .?> "UploadsList" .!@ mempty) <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable ListMultipartUploads where

instance NFData ListMultipartUploads where

instance ToHeaders ListMultipartUploads where
        toHeaders = const mempty

instance ToPath ListMultipartUploads where
        toPath ListMultipartUploads'{..}
          = mconcat
              ["/", toBS _lmuAccountId, "/vaults/",
               toBS _lmuVaultName, "/multipart-uploads"]

instance ToQuery ListMultipartUploads where
        toQuery ListMultipartUploads'{..}
          = mconcat
              ["marker" =: _lmuMarker, "limit" =: _lmuLimit]

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'listMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { _lmursUploadsList    :: !(Maybe [UploadListElement])
  , _lmursMarker         :: !(Maybe Text)
  , _lmursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMultipartUploadsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmursUploadsList' - A list of in-progress multipart uploads.
--
-- * 'lmursMarker' - An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
--
-- * 'lmursResponseStatus' - -- | The response status code.
listMultipartUploadsResponse
    :: Int -- ^ 'lmursResponseStatus'
    -> ListMultipartUploadsResponse
listMultipartUploadsResponse pResponseStatus_ =
  ListMultipartUploadsResponse'
    { _lmursUploadsList = Nothing
    , _lmursMarker = Nothing
    , _lmursResponseStatus = pResponseStatus_
    }


-- | A list of in-progress multipart uploads.
lmursUploadsList :: Lens' ListMultipartUploadsResponse [UploadListElement]
lmursUploadsList = lens _lmursUploadsList (\ s a -> s{_lmursUploadsList = a}) . _Default . _Coerce

-- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
lmursMarker :: Lens' ListMultipartUploadsResponse (Maybe Text)
lmursMarker = lens _lmursMarker (\ s a -> s{_lmursMarker = a})

-- | -- | The response status code.
lmursResponseStatus :: Lens' ListMultipartUploadsResponse Int
lmursResponseStatus = lens _lmursResponseStatus (\ s a -> s{_lmursResponseStatus = a})

instance NFData ListMultipartUploadsResponse where
