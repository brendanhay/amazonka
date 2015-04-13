{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation lists the parts of an archive that have been uploaded in a
-- specific multipart upload. You can make this request at any time during an
-- in-progress multipart upload before you complete the upload (see 'CompleteMultipartUpload'. List Parts returns an error for completed uploads. The list returned in the
-- List Parts response is sorted by part range.
--
-- The List Parts operation supports pagination. By default, this operation
-- returns up to 1,000 uploaded parts in the response. You should always check
-- the response for a 'marker' at which to continue the list; if there are no more
-- items the 'marker' is 'null'. To return a list of parts that begins at a specific
-- part, set the 'marker' request parameter to the value you obtained from a
-- previous List Parts request. You can also limit the number of parts returned
-- in the response by specifying the 'limit' parameter in the request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working withArchives in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html List Parts> in the /Amazon Glacier DeveloperGuide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-ListParts.html>
module Network.AWS.Glacier.ListParts
    (
    -- * Request
      ListParts
    -- ** Request constructor
    , listParts
    -- ** Request lenses
    , lpAccountId
    , lpLimit
    , lpMarker
    , lpUploadId
    , lpVaultName

    -- * Response
    , ListPartsResponse
    -- ** Response constructor
    , listPartsResponse
    -- ** Response lenses
    , lprArchiveDescription
    , lprCreationDate
    , lprMarker
    , lprMultipartUploadId
    , lprPartSizeInBytes
    , lprParts
    , lprVaultARN
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data ListParts = ListParts
    { _lpAccountId :: Text
    , _lpLimit     :: Maybe Text
    , _lpMarker    :: Maybe Text
    , _lpUploadId  :: Text
    , _lpVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListParts' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpAccountId' @::@ 'Text'
--
-- * 'lpLimit' @::@ 'Maybe' 'Text'
--
-- * 'lpMarker' @::@ 'Maybe' 'Text'
--
-- * 'lpUploadId' @::@ 'Text'
--
-- * 'lpVaultName' @::@ 'Text'
--
listParts :: Text -- ^ 'lpAccountId'
          -> Text -- ^ 'lpVaultName'
          -> Text -- ^ 'lpUploadId'
          -> ListParts
listParts p1 p2 p3 = ListParts
    { _lpAccountId = p1
    , _lpVaultName = p2
    , _lpUploadId  = p3
    , _lpMarker    = Nothing
    , _lpLimit     = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
lpAccountId :: Lens' ListParts Text
lpAccountId = lens _lpAccountId (\s a -> s { _lpAccountId = a })

-- | Specifies the maximum number of parts returned in the response body. If this
-- value is not specified, the List Parts operation returns up to 1,000 uploads.
lpLimit :: Lens' ListParts (Maybe Text)
lpLimit = lens _lpLimit (\s a -> s { _lpLimit = a })

-- | An opaque string used for pagination. This value specifies the part at which
-- the listing of parts should begin. Get the marker value from the response of
-- a previous List Parts response. You need only include the marker if you are
-- continuing the pagination of results started in a previous List Parts request.
lpMarker :: Lens' ListParts (Maybe Text)
lpMarker = lens _lpMarker (\s a -> s { _lpMarker = a })

-- | The upload ID of the multipart upload.
lpUploadId :: Lens' ListParts Text
lpUploadId = lens _lpUploadId (\s a -> s { _lpUploadId = a })

-- | The name of the vault.
lpVaultName :: Lens' ListParts Text
lpVaultName = lens _lpVaultName (\s a -> s { _lpVaultName = a })

data ListPartsResponse = ListPartsResponse
    { _lprArchiveDescription :: Maybe Text
    , _lprCreationDate       :: Maybe Text
    , _lprMarker             :: Maybe Text
    , _lprMultipartUploadId  :: Maybe Text
    , _lprPartSizeInBytes    :: Maybe Integer
    , _lprParts              :: List "Parts" PartListElement
    , _lprVaultARN           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListPartsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lprArchiveDescription' @::@ 'Maybe' 'Text'
--
-- * 'lprCreationDate' @::@ 'Maybe' 'Text'
--
-- * 'lprMarker' @::@ 'Maybe' 'Text'
--
-- * 'lprMultipartUploadId' @::@ 'Maybe' 'Text'
--
-- * 'lprPartSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'lprParts' @::@ ['PartListElement']
--
-- * 'lprVaultARN' @::@ 'Maybe' 'Text'
--
listPartsResponse :: ListPartsResponse
listPartsResponse = ListPartsResponse
    { _lprMultipartUploadId  = Nothing
    , _lprVaultARN           = Nothing
    , _lprArchiveDescription = Nothing
    , _lprPartSizeInBytes    = Nothing
    , _lprCreationDate       = Nothing
    , _lprParts              = mempty
    , _lprMarker             = Nothing
    }

-- | The description of the archive that was specified in the Initiate Multipart
-- Upload request.
lprArchiveDescription :: Lens' ListPartsResponse (Maybe Text)
lprArchiveDescription =
    lens _lprArchiveDescription (\s a -> s { _lprArchiveDescription = a })

-- | The UTC time at which the multipart upload was initiated.
lprCreationDate :: Lens' ListPartsResponse (Maybe Text)
lprCreationDate = lens _lprCreationDate (\s a -> s { _lprCreationDate = a })

-- | An opaque string that represents where to continue pagination of the results.
-- You use the marker in a new List Parts request to obtain more jobs in the
-- list. If there are no more parts, this value is 'null'.
lprMarker :: Lens' ListPartsResponse (Maybe Text)
lprMarker = lens _lprMarker (\s a -> s { _lprMarker = a })

-- | The ID of the upload to which the parts are associated.
lprMultipartUploadId :: Lens' ListPartsResponse (Maybe Text)
lprMultipartUploadId =
    lens _lprMultipartUploadId (\s a -> s { _lprMultipartUploadId = a })

-- | The part size in bytes.
lprPartSizeInBytes :: Lens' ListPartsResponse (Maybe Integer)
lprPartSizeInBytes =
    lens _lprPartSizeInBytes (\s a -> s { _lprPartSizeInBytes = a })

-- | A list of the part sizes of the multipart upload.
lprParts :: Lens' ListPartsResponse [PartListElement]
lprParts = lens _lprParts (\s a -> s { _lprParts = a }) . _List

-- | The Amazon Resource Name (ARN) of the vault to which the multipart upload was
-- initiated.
lprVaultARN :: Lens' ListPartsResponse (Maybe Text)
lprVaultARN = lens _lprVaultARN (\s a -> s { _lprVaultARN = a })

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toText _lpAccountId
        , "/vaults/"
        , toText _lpVaultName
        , "/multipart-uploads/"
        , toText _lpUploadId
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = mconcat
        [ "marker" =? _lpMarker
        , "limit"  =? _lpLimit
        ]

instance ToHeaders ListParts

instance ToJSON ListParts where
    toJSON = const (toJSON Empty)

instance AWSRequest ListParts where
    type Sv ListParts = Glacier
    type Rs ListParts = ListPartsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListPartsResponse where
    parseJSON = withObject "ListPartsResponse" $ \o -> ListPartsResponse
        <$> o .:? "ArchiveDescription"
        <*> o .:? "CreationDate"
        <*> o .:? "Marker"
        <*> o .:? "MultipartUploadId"
        <*> o .:? "PartSizeInBytes"
        <*> o .:? "Parts" .!= mempty
        <*> o .:? "VaultARN"
