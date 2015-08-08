{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.S3.Internal
import           Network.AWS.S3.Types.Sum

-- | /See:/ 'accessControlPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acpGrants'
--
-- * 'acpOwner'
data AccessControlPolicy = AccessControlPolicy'
    { _acpGrants :: !(Maybe [Grant])
    , _acpOwner  :: !(Maybe Owner)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AccessControlPolicy' smart constructor.
accessControlPolicy :: AccessControlPolicy
accessControlPolicy =
    AccessControlPolicy'
    { _acpGrants = Nothing
    , _acpOwner = Nothing
    }

-- | A list of grants.
acpGrants :: Lens' AccessControlPolicy [Grant]
acpGrants = lens _acpGrants (\ s a -> s{_acpGrants = a}) . _Default . _Coerce;

-- | Undocumented member.
acpOwner :: Lens' AccessControlPolicy (Maybe Owner)
acpOwner = lens _acpOwner (\ s a -> s{_acpOwner = a});

instance ToXML AccessControlPolicy where
        toXML AccessControlPolicy'{..}
          = mconcat
              ["AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _acpGrants),
               "Owner" @= _acpOwner]

-- | /See:/ 'bucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bCreationDate'
--
-- * 'bName'
data Bucket = Bucket'
    { _bCreationDate :: !RFC822
    , _bName         :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Bucket' smart constructor.
bucket :: UTCTime -> BucketName -> Bucket
bucket pCreationDate_ pName_ =
    Bucket'
    { _bCreationDate = _Time # pCreationDate_
    , _bName = pName_
    }

-- | Date the bucket was created.
bCreationDate :: Lens' Bucket UTCTime
bCreationDate = lens _bCreationDate (\ s a -> s{_bCreationDate = a}) . _Time;

-- | The name of the bucket.
bName :: Lens' Bucket BucketName
bName = lens _bName (\ s a -> s{_bName = a});

instance FromXML Bucket where
        parseXML x
          = Bucket' <$> (x .@ "CreationDate") <*> (x .@ "Name")

-- | /See:/ 'bucketLoggingStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'blsLoggingEnabled'
newtype BucketLoggingStatus = BucketLoggingStatus'
    { _blsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BucketLoggingStatus' smart constructor.
bucketLoggingStatus :: BucketLoggingStatus
bucketLoggingStatus =
    BucketLoggingStatus'
    { _blsLoggingEnabled = Nothing
    }

-- | Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\ s a -> s{_blsLoggingEnabled = a});

instance ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus'{..}
          = mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

-- | /See:/ 'corsConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccCORSRules'
newtype CORSConfiguration = CORSConfiguration'
    { _ccCORSRules :: Maybe [CORSRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CORSConfiguration' smart constructor.
corsConfiguration :: CORSConfiguration
corsConfiguration =
    CORSConfiguration'
    { _ccCORSRules = Nothing
    }

-- | Undocumented member.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\ s a -> s{_ccCORSRules = a}) . _Default . _Coerce;

instance ToXML CORSConfiguration where
        toXML CORSConfiguration'{..}
          = mconcat
              [toXML (toXMLList "CORSRule" <$> _ccCORSRules)]

-- | /See:/ 'corsRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crAllowedMethods'
--
-- * 'crMaxAgeSeconds'
--
-- * 'crAllowedHeaders'
--
-- * 'crAllowedOrigins'
--
-- * 'crExposeHeaders'
data CORSRule = CORSRule'
    { _crAllowedMethods :: !(Maybe [Text])
    , _crMaxAgeSeconds  :: !(Maybe Int)
    , _crAllowedHeaders :: !(Maybe [Text])
    , _crAllowedOrigins :: !(Maybe [Text])
    , _crExposeHeaders  :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CORSRule' smart constructor.
corsRule :: CORSRule
corsRule =
    CORSRule'
    { _crAllowedMethods = Nothing
    , _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crAllowedOrigins = Nothing
    , _crExposeHeaders = Nothing
    }

-- | Identifies HTTP methods that the domain\/origin specified in the rule is
-- allowed to execute.
crAllowedMethods :: Lens' CORSRule [Text]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Default . _Coerce;

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a});

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default . _Coerce;

-- | One or more origins you want customers to be able to access the bucket
-- from.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Default . _Coerce;

-- | One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default . _Coerce;

instance FromXML CORSRule where
        parseXML x
          = CORSRule' <$>
              (may (parseXMLList "AllowedMethod") x) <*>
                (x .@? "MaxAgeSeconds")
                <*> (may (parseXMLList "AllowedHeader") x)
                <*> (may (parseXMLList "AllowedOrigin") x)
                <*> (may (parseXMLList "ExposeHeader") x)

instance ToXML CORSRule where
        toXML CORSRule'{..}
          = mconcat
              [toXML
                 (toXMLList "AllowedMethod" <$> _crAllowedMethods),
               "MaxAgeSeconds" @= _crMaxAgeSeconds,
               toXML
                 (toXMLList "AllowedHeader" <$> _crAllowedHeaders),
               toXML
                 (toXMLList "AllowedOrigin" <$> _crAllowedOrigins),
               toXML
                 (toXMLList "ExposeHeader" <$> _crExposeHeaders)]

-- | /See:/ 'commonPrefix' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPrefix'
newtype CommonPrefix = CommonPrefix'
    { _cpPrefix :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CommonPrefix' smart constructor.
commonPrefix :: CommonPrefix
commonPrefix =
    CommonPrefix'
    { _cpPrefix = Nothing
    }

-- | Undocumented member.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\ s a -> s{_cpPrefix = a});

instance FromXML CommonPrefix where
        parseXML x = CommonPrefix' <$> (x .@? "Prefix")

-- | /See:/ 'completedMultipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuParts'
newtype CompletedMultipartUpload = CompletedMultipartUpload'
    { _cmuParts :: Maybe (List1 CompletedPart)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompletedMultipartUpload' smart constructor.
completedMultipartUpload :: CompletedMultipartUpload
completedMultipartUpload =
    CompletedMultipartUpload'
    { _cmuParts = Nothing
    }

-- | Undocumented member.
cmuParts :: Lens' CompletedMultipartUpload (Maybe (NonEmpty CompletedPart))
cmuParts = lens _cmuParts (\ s a -> s{_cmuParts = a}) . mapping _List1;

instance ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload'{..}
          = mconcat [toXML (toXMLList "Part" <$> _cmuParts)]

-- | /See:/ 'completedPart' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPartNumber'
--
-- * 'cpETag'
data CompletedPart = CompletedPart'
    { _cpPartNumber :: !Int
    , _cpETag       :: !ETag
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CompletedPart' smart constructor.
completedPart :: Int -> ETag -> CompletedPart
completedPart pPartNumber_ pETag_ =
    CompletedPart'
    { _cpPartNumber = pPartNumber_
    , _cpETag = pETag_
    }

-- | Part number that identifies the part. This is a positive integer between
-- 1 and 10,000.
cpPartNumber :: Lens' CompletedPart Int
cpPartNumber = lens _cpPartNumber (\ s a -> s{_cpPartNumber = a});

-- | Entity tag returned when the part was uploaded.
cpETag :: Lens' CompletedPart ETag
cpETag = lens _cpETag (\ s a -> s{_cpETag = a});

instance ToXML CompletedPart where
        toXML CompletedPart'{..}
          = mconcat
              ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]

-- | /See:/ 'condition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cKeyPrefixEquals'
--
-- * 'cHTTPErrorCodeReturnedEquals'
data Condition = Condition'
    { _cKeyPrefixEquals             :: !(Maybe Text)
    , _cHTTPErrorCodeReturnedEquals :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Condition' smart constructor.
condition :: Condition
condition =
    Condition'
    { _cKeyPrefixEquals = Nothing
    , _cHTTPErrorCodeReturnedEquals = Nothing
    }

-- | The object key name prefix when the redirect is applied. For example, to
-- redirect requests for ExamplePage.html, the key prefix will be
-- ExamplePage.html. To redirect request for all pages with the prefix
-- docs\/, the key prefix will be \/docs, which identifies all objects in
-- the docs\/ folder. Required when the parent element Condition is
-- specified and sibling HttpErrorCodeReturnedEquals is not specified. If
-- both conditions are specified, both must be true for the redirect to be
-- applied.
cKeyPrefixEquals :: Lens' Condition (Maybe Text)
cKeyPrefixEquals = lens _cKeyPrefixEquals (\ s a -> s{_cKeyPrefixEquals = a});

-- | The HTTP error code when the redirect is applied. In the event of an
-- error, if the error code equals this value, then the specified redirect
-- is applied. Required when parent element Condition is specified and
-- sibling KeyPrefixEquals is not specified. If both are specified, then
-- both must be true for the redirect to be applied.
cHTTPErrorCodeReturnedEquals :: Lens' Condition (Maybe Text)
cHTTPErrorCodeReturnedEquals = lens _cHTTPErrorCodeReturnedEquals (\ s a -> s{_cHTTPErrorCodeReturnedEquals = a});

instance FromXML Condition where
        parseXML x
          = Condition' <$>
              (x .@? "KeyPrefixEquals") <*>
                (x .@? "HttpErrorCodeReturnedEquals")

instance ToXML Condition where
        toXML Condition'{..}
          = mconcat
              ["KeyPrefixEquals" @= _cKeyPrefixEquals,
               "HttpErrorCodeReturnedEquals" @=
                 _cHTTPErrorCodeReturnedEquals]

-- | /See:/ 'copyObjectResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corETag'
--
-- * 'corLastModified'
data CopyObjectResult = CopyObjectResult'
    { _corETag         :: !(Maybe ETag)
    , _corLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyObjectResult' smart constructor.
copyObjectResult :: CopyObjectResult
copyObjectResult =
    CopyObjectResult'
    { _corETag = Nothing
    , _corLastModified = Nothing
    }

-- | Undocumented member.
corETag :: Lens' CopyObjectResult (Maybe ETag)
corETag = lens _corETag (\ s a -> s{_corETag = a});

-- | Undocumented member.
corLastModified :: Lens' CopyObjectResult (Maybe UTCTime)
corLastModified = lens _corLastModified (\ s a -> s{_corLastModified = a}) . mapping _Time;

instance FromXML CopyObjectResult where
        parseXML x
          = CopyObjectResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

-- | /See:/ 'copyPartResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprETag'
--
-- * 'cprLastModified'
data CopyPartResult = CopyPartResult'
    { _cprETag         :: !(Maybe ETag)
    , _cprLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyPartResult' smart constructor.
copyPartResult :: CopyPartResult
copyPartResult =
    CopyPartResult'
    { _cprETag = Nothing
    , _cprLastModified = Nothing
    }

-- | Entity tag of the object.
cprETag :: Lens' CopyPartResult (Maybe ETag)
cprETag = lens _cprETag (\ s a -> s{_cprETag = a});

-- | Date and time at which the object was uploaded.
cprLastModified :: Lens' CopyPartResult (Maybe UTCTime)
cprLastModified = lens _cprLastModified (\ s a -> s{_cprLastModified = a}) . mapping _Time;

instance FromXML CopyPartResult where
        parseXML x
          = CopyPartResult' <$>
              (x .@? "ETag") <*> (x .@? "LastModified")

-- | /See:/ 'createBucketConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbcLocationConstraint'
newtype CreateBucketConfiguration = CreateBucketConfiguration'
    { _cbcLocationConstraint :: Maybe Region
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBucketConfiguration' smart constructor.
createBucketConfiguration :: CreateBucketConfiguration
createBucketConfiguration =
    CreateBucketConfiguration'
    { _cbcLocationConstraint = Nothing
    }

-- | Specifies the region where the bucket will be created. If you don\'t
-- specify a region, the bucket will be created in US Standard.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe Region)
cbcLocationConstraint = lens _cbcLocationConstraint (\ s a -> s{_cbcLocationConstraint = a});

instance ToXML CreateBucketConfiguration where
        toXML CreateBucketConfiguration'{..}
          = mconcat
              ["LocationConstraint" @= _cbcLocationConstraint]

-- | /See:/ 'delete'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dQuiet'
--
-- * 'dObjects'
data Delete = Delete'
    { _dQuiet   :: !(Maybe Bool)
    , _dObjects :: ![ObjectIdentifier]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Delete' smart constructor.
delete' :: Delete
delete' =
    Delete'
    { _dQuiet = Nothing
    , _dObjects = mempty
    }

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
dQuiet :: Lens' Delete (Maybe Bool)
dQuiet = lens _dQuiet (\ s a -> s{_dQuiet = a});

-- | Undocumented member.
dObjects :: Lens' Delete [ObjectIdentifier]
dObjects = lens _dObjects (\ s a -> s{_dObjects = a}) . _Coerce;

instance ToXML Delete where
        toXML Delete'{..}
          = mconcat
              ["Quiet" @= _dQuiet, toXMLList "Object" _dObjects]

-- | /See:/ 'deleteMarkerEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmeVersionId'
--
-- * 'dmeIsLatest'
--
-- * 'dmeOwner'
--
-- * 'dmeKey'
--
-- * 'dmeLastModified'
data DeleteMarkerEntry = DeleteMarkerEntry'
    { _dmeVersionId    :: !(Maybe ObjectVersionId)
    , _dmeIsLatest     :: !(Maybe Bool)
    , _dmeOwner        :: !(Maybe Owner)
    , _dmeKey          :: !(Maybe ObjectKey)
    , _dmeLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteMarkerEntry' smart constructor.
deleteMarkerEntry :: DeleteMarkerEntry
deleteMarkerEntry =
    DeleteMarkerEntry'
    { _dmeVersionId = Nothing
    , _dmeIsLatest = Nothing
    , _dmeOwner = Nothing
    , _dmeKey = Nothing
    , _dmeLastModified = Nothing
    }

-- | Version ID of an object.
dmeVersionId :: Lens' DeleteMarkerEntry (Maybe ObjectVersionId)
dmeVersionId = lens _dmeVersionId (\ s a -> s{_dmeVersionId = a});

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
dmeIsLatest :: Lens' DeleteMarkerEntry (Maybe Bool)
dmeIsLatest = lens _dmeIsLatest (\ s a -> s{_dmeIsLatest = a});

-- | Undocumented member.
dmeOwner :: Lens' DeleteMarkerEntry (Maybe Owner)
dmeOwner = lens _dmeOwner (\ s a -> s{_dmeOwner = a});

-- | The object key.
dmeKey :: Lens' DeleteMarkerEntry (Maybe ObjectKey)
dmeKey = lens _dmeKey (\ s a -> s{_dmeKey = a});

-- | Date and time the object was last modified.
dmeLastModified :: Lens' DeleteMarkerEntry (Maybe UTCTime)
dmeLastModified = lens _dmeLastModified (\ s a -> s{_dmeLastModified = a}) . mapping _Time;

instance FromXML DeleteMarkerEntry where
        parseXML x
          = DeleteMarkerEntry' <$>
              (x .@? "VersionId") <*> (x .@? "IsLatest") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "LastModified")

-- | /See:/ 'deletedObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dVersionId'
--
-- * 'dDeleteMarker'
--
-- * 'dDeleteMarkerVersionId'
--
-- * 'dKey'
data DeletedObject = DeletedObject'
    { _dVersionId             :: !(Maybe ObjectVersionId)
    , _dDeleteMarker          :: !(Maybe Bool)
    , _dDeleteMarkerVersionId :: !(Maybe Text)
    , _dKey                   :: !(Maybe ObjectKey)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletedObject' smart constructor.
deletedObject :: DeletedObject
deletedObject =
    DeletedObject'
    { _dVersionId = Nothing
    , _dDeleteMarker = Nothing
    , _dDeleteMarkerVersionId = Nothing
    , _dKey = Nothing
    }

-- | Undocumented member.
dVersionId :: Lens' DeletedObject (Maybe ObjectVersionId)
dVersionId = lens _dVersionId (\ s a -> s{_dVersionId = a});

-- | Undocumented member.
dDeleteMarker :: Lens' DeletedObject (Maybe Bool)
dDeleteMarker = lens _dDeleteMarker (\ s a -> s{_dDeleteMarker = a});

-- | Undocumented member.
dDeleteMarkerVersionId :: Lens' DeletedObject (Maybe Text)
dDeleteMarkerVersionId = lens _dDeleteMarkerVersionId (\ s a -> s{_dDeleteMarkerVersionId = a});

-- | Undocumented member.
dKey :: Lens' DeletedObject (Maybe ObjectKey)
dKey = lens _dKey (\ s a -> s{_dKey = a});

instance FromXML DeletedObject where
        parseXML x
          = DeletedObject' <$>
              (x .@? "VersionId") <*> (x .@? "DeleteMarker") <*>
                (x .@? "DeleteMarkerVersionId")
                <*> (x .@? "Key")

-- | /See:/ 'destination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dBucket'
newtype Destination = Destination'
    { _dBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Destination' smart constructor.
destination :: BucketName -> Destination
destination pBucket_ =
    Destination'
    { _dBucket = pBucket_
    }

-- | Amazon resource name (ARN) of the bucket where you want Amazon S3 to
-- store replicas of the object identified by the rule.
dBucket :: Lens' Destination BucketName
dBucket = lens _dBucket (\ s a -> s{_dBucket = a});

instance FromXML Destination where
        parseXML x = Destination' <$> (x .@ "Bucket")

instance ToXML Destination where
        toXML Destination'{..}
          = mconcat ["Bucket" @= _dBucket]

-- | /See:/ 'errorDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edKey'
newtype ErrorDocument = ErrorDocument'
    { _edKey :: ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ErrorDocument' smart constructor.
errorDocument :: ObjectKey -> ErrorDocument
errorDocument pKey_ =
    ErrorDocument'
    { _edKey = pKey_
    }

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\ s a -> s{_edKey = a});

instance FromXML ErrorDocument where
        parseXML x = ErrorDocument' <$> (x .@ "Key")

instance ToXML ErrorDocument where
        toXML ErrorDocument'{..} = mconcat ["Key" @= _edKey]

-- | /See:/ 'grant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gPermission'
--
-- * 'gGrantee'
data Grant = Grant'
    { _gPermission :: !(Maybe Permission)
    , _gGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Grant' smart constructor.
grant :: Grant
grant =
    Grant'
    { _gPermission = Nothing
    , _gGrantee = Nothing
    }

-- | Specifies the permission given to the grantee.
gPermission :: Lens' Grant (Maybe Permission)
gPermission = lens _gPermission (\ s a -> s{_gPermission = a});

-- | Undocumented member.
gGrantee :: Lens' Grant (Maybe Grantee)
gGrantee = lens _gGrantee (\ s a -> s{_gGrantee = a});

instance FromXML Grant where
        parseXML x
          = Grant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance ToXML Grant where
        toXML Grant'{..}
          = mconcat
              ["Permission" @= _gPermission,
               "Grantee" @= _gGrantee]

-- | /See:/ 'grantee' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gURI'
--
-- * 'gEmailAddress'
--
-- * 'gId'
--
-- * 'gDisplayName'
--
-- * 'gType'
data Grantee = Grantee'
    { _gURI          :: !(Maybe Text)
    , _gEmailAddress :: !(Maybe Text)
    , _gId           :: !(Maybe Text)
    , _gDisplayName  :: !(Maybe Text)
    , _gType         :: !Type
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Grantee' smart constructor.
grantee :: Type -> Grantee
grantee pType_ =
    Grantee'
    { _gURI = Nothing
    , _gEmailAddress = Nothing
    , _gId = Nothing
    , _gDisplayName = Nothing
    , _gType = pType_
    }

-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\ s a -> s{_gURI = a});

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\ s a -> s{_gEmailAddress = a});

-- | The canonical user ID of the grantee.
gId :: Lens' Grantee (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a});

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\ s a -> s{_gDisplayName = a});

-- | Type of grantee
gType :: Lens' Grantee Type
gType = lens _gType (\ s a -> s{_gType = a});

instance FromXML Grantee where
        parseXML x
          = Grantee' <$>
              (x .@? "URI") <*> (x .@? "EmailAddress") <*>
                (x .@? "ID")
                <*> (x .@? "DisplayName")
                <*> (x .@ "xsi:type")

instance ToXML Grantee where
        toXML Grantee'{..}
          = mconcat
              ["URI" @= _gURI, "EmailAddress" @= _gEmailAddress,
               "ID" @= _gId, "DisplayName" @= _gDisplayName,
               "xsi:type" @= _gType]

-- | /See:/ 'indexDocument' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idSuffix'
newtype IndexDocument = IndexDocument'
    { _idSuffix :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IndexDocument' smart constructor.
indexDocument :: Text -> IndexDocument
indexDocument pSuffix_ =
    IndexDocument'
    { _idSuffix = pSuffix_
    }

-- | A suffix that is appended to a request that is for a directory on the
-- website endpoint (e.g. if the suffix is index.html and you make a
-- request to samplebucket\/images\/ the data that is returned will be for
-- the object with the key name images\/index.html) The suffix must not be
-- empty and must not include a slash character.
idSuffix :: Lens' IndexDocument Text
idSuffix = lens _idSuffix (\ s a -> s{_idSuffix = a});

instance FromXML IndexDocument where
        parseXML x = IndexDocument' <$> (x .@ "Suffix")

instance ToXML IndexDocument where
        toXML IndexDocument'{..}
          = mconcat ["Suffix" @= _idSuffix]

-- | /See:/ 'initiator' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iId'
--
-- * 'iDisplayName'
data Initiator = Initiator'
    { _iId          :: !(Maybe Text)
    , _iDisplayName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Initiator' smart constructor.
initiator :: Initiator
initiator =
    Initiator'
    { _iId = Nothing
    , _iDisplayName = Nothing
    }

-- | If the principal is an AWS account, it provides the Canonical User ID.
-- If the principal is an IAM User, it provides a user ARN value.
iId :: Lens' Initiator (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a});

-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a});

instance FromXML Initiator where
        parseXML x
          = Initiator' <$>
              (x .@? "ID") <*> (x .@? "DisplayName")

-- | Container for specifying the AWS Lambda notification configuration.
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfcId'
--
-- * 'lfcLambdaFunctionARN'
--
-- * 'lfcEvents'
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
    { _lfcId                :: !(Maybe Text)
    , _lfcLambdaFunctionARN :: !Text
    , _lfcEvents            :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LambdaFunctionConfiguration' smart constructor.
lambdaFunctionConfiguration :: Text -> LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN_ =
    LambdaFunctionConfiguration'
    { _lfcId = Nothing
    , _lfcLambdaFunctionARN = pLambdaFunctionARN_
    , _lfcEvents = mempty
    }

-- | Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\ s a -> s{_lfcId = a});

-- | Lambda cloud function ARN that Amazon S3 can invoke when it detects
-- events of the specified type.
lfcLambdaFunctionARN :: Lens' LambdaFunctionConfiguration Text
lfcLambdaFunctionARN = lens _lfcLambdaFunctionARN (\ s a -> s{_lfcLambdaFunctionARN = a});

-- | Undocumented member.
lfcEvents :: Lens' LambdaFunctionConfiguration [Event]
lfcEvents = lens _lfcEvents (\ s a -> s{_lfcEvents = a}) . _Coerce;

instance FromXML LambdaFunctionConfiguration where
        parseXML x
          = LambdaFunctionConfiguration' <$>
              (x .@? "Id") <*> (x .@ "CloudFunction") <*>
                (parseXMLList "Event" x)

instance ToXML LambdaFunctionConfiguration where
        toXML LambdaFunctionConfiguration'{..}
          = mconcat
              ["Id" @= _lfcId,
               "CloudFunction" @= _lfcLambdaFunctionARN,
               toXMLList "Event" _lfcEvents]

-- | /See:/ 'lifecycleConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcRules'
newtype LifecycleConfiguration = LifecycleConfiguration'
    { _lcRules :: [Rule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LifecycleConfiguration' smart constructor.
lifecycleConfiguration :: LifecycleConfiguration
lifecycleConfiguration =
    LifecycleConfiguration'
    { _lcRules = mempty
    }

-- | Undocumented member.
lcRules :: Lens' LifecycleConfiguration [Rule]
lcRules = lens _lcRules (\ s a -> s{_lcRules = a}) . _Coerce;

instance ToXML LifecycleConfiguration where
        toXML LifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _lcRules]

-- | /See:/ 'lifecycleExpiration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leDays'
--
-- * 'leDate'
data LifecycleExpiration = LifecycleExpiration'
    { _leDays :: !(Maybe Int)
    , _leDate :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LifecycleExpiration' smart constructor.
lifecycleExpiration :: LifecycleExpiration
lifecycleExpiration =
    LifecycleExpiration'
    { _leDays = Nothing
    , _leDate = Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\ s a -> s{_leDays = a});

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\ s a -> s{_leDate = a}) . mapping _Time;

instance FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' <$>
              (x .@? "Days") <*> (x .@? "Date")

instance ToXML LifecycleExpiration where
        toXML LifecycleExpiration'{..}
          = mconcat ["Days" @= _leDays, "Date" @= _leDate]

-- | /See:/ 'loggingEnabled' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leTargetBucket'
--
-- * 'leTargetGrants'
--
-- * 'leTargetPrefix'
data LoggingEnabled = LoggingEnabled'
    { _leTargetBucket :: !(Maybe Text)
    , _leTargetGrants :: !(Maybe [TargetGrant])
    , _leTargetPrefix :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoggingEnabled' smart constructor.
loggingEnabled :: LoggingEnabled
loggingEnabled =
    LoggingEnabled'
    { _leTargetBucket = Nothing
    , _leTargetGrants = Nothing
    , _leTargetPrefix = Nothing
    }

-- | Specifies the bucket where you want Amazon S3 to store server access
-- logs. You can have your logs delivered to any bucket that you own,
-- including the same bucket that is being logged. You can also configure
-- multiple buckets to deliver their logs to the same target bucket. In
-- this case you should choose a different TargetPrefix for each source
-- bucket so that the delivered log files can be distinguished by key.
leTargetBucket :: Lens' LoggingEnabled (Maybe Text)
leTargetBucket = lens _leTargetBucket (\ s a -> s{_leTargetBucket = a});

-- | Undocumented member.
leTargetGrants :: Lens' LoggingEnabled [TargetGrant]
leTargetGrants = lens _leTargetGrants (\ s a -> s{_leTargetGrants = a}) . _Default . _Coerce;

-- | This element lets you specify a prefix for the keys that the log files
-- will be stored under.
leTargetPrefix :: Lens' LoggingEnabled (Maybe Text)
leTargetPrefix = lens _leTargetPrefix (\ s a -> s{_leTargetPrefix = a});

instance FromXML LoggingEnabled where
        parseXML x
          = LoggingEnabled' <$>
              (x .@? "TargetBucket") <*>
                (x .@? "TargetGrants" .!@ mempty >>=
                   may (parseXMLList "Grant"))
                <*> (x .@? "TargetPrefix")

instance ToXML LoggingEnabled where
        toXML LoggingEnabled'{..}
          = mconcat
              ["TargetBucket" @= _leTargetBucket,
               "TargetGrants" @=
                 toXML (toXMLList "Grant" <$> _leTargetGrants),
               "TargetPrefix" @= _leTargetPrefix]

-- | /See:/ 'multipartUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'muInitiated'
--
-- * 'muInitiator'
--
-- * 'muOwner'
--
-- * 'muKey'
--
-- * 'muStorageClass'
--
-- * 'muUploadId'
data MultipartUpload = MultipartUpload'
    { _muInitiated    :: !(Maybe RFC822)
    , _muInitiator    :: !(Maybe Initiator)
    , _muOwner        :: !(Maybe Owner)
    , _muKey          :: !(Maybe ObjectKey)
    , _muStorageClass :: !(Maybe StorageClass)
    , _muUploadId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MultipartUpload' smart constructor.
multipartUpload :: MultipartUpload
multipartUpload =
    MultipartUpload'
    { _muInitiated = Nothing
    , _muInitiator = Nothing
    , _muOwner = Nothing
    , _muKey = Nothing
    , _muStorageClass = Nothing
    , _muUploadId = Nothing
    }

-- | Date and time at which the multipart upload was initiated.
muInitiated :: Lens' MultipartUpload (Maybe UTCTime)
muInitiated = lens _muInitiated (\ s a -> s{_muInitiated = a}) . mapping _Time;

-- | Identifies who initiated the multipart upload.
muInitiator :: Lens' MultipartUpload (Maybe Initiator)
muInitiator = lens _muInitiator (\ s a -> s{_muInitiator = a});

-- | Undocumented member.
muOwner :: Lens' MultipartUpload (Maybe Owner)
muOwner = lens _muOwner (\ s a -> s{_muOwner = a});

-- | Key of the object for which the multipart upload was initiated.
muKey :: Lens' MultipartUpload (Maybe ObjectKey)
muKey = lens _muKey (\ s a -> s{_muKey = a});

-- | The class of storage used to store the object.
muStorageClass :: Lens' MultipartUpload (Maybe StorageClass)
muStorageClass = lens _muStorageClass (\ s a -> s{_muStorageClass = a});

-- | Upload ID that identifies the multipart upload.
muUploadId :: Lens' MultipartUpload (Maybe Text)
muUploadId = lens _muUploadId (\ s a -> s{_muUploadId = a});

instance FromXML MultipartUpload where
        parseXML x
          = MultipartUpload' <$>
              (x .@? "Initiated") <*> (x .@? "Initiator") <*>
                (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "UploadId")

-- | Specifies when noncurrent object versions expire. Upon expiration,
-- Amazon S3 permanently deletes the noncurrent object versions. You set
-- this lifecycle configuration action on a bucket that has versioning
-- enabled (or suspended) to request that Amazon S3 delete noncurrent
-- object versions at a specific period in the object\'s lifetime.
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nveNoncurrentDays'
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
    { _nveNoncurrentDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NoncurrentVersionExpiration' smart constructor.
noncurrentVersionExpiration :: Int -> NoncurrentVersionExpiration
noncurrentVersionExpiration pNoncurrentDays_ =
    NoncurrentVersionExpiration'
    { _nveNoncurrentDays = pNoncurrentDays_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- </AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the Amazon Simple Storage Service Developer Guide.
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays = lens _nveNoncurrentDays (\ s a -> s{_nveNoncurrentDays = a});

instance FromXML NoncurrentVersionExpiration where
        parseXML x
          = NoncurrentVersionExpiration' <$>
              (x .@ "NoncurrentDays")

instance ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration'{..}
          = mconcat ["NoncurrentDays" @= _nveNoncurrentDays]

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the GLACIER storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action
-- to request that Amazon S3 transition noncurrent object versions to the
-- GLACIER storage class at a specific period in the object\'s lifetime.
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nvtNoncurrentDays'
--
-- * 'nvtStorageClass'
data NoncurrentVersionTransition = NoncurrentVersionTransition'
    { _nvtNoncurrentDays :: !Int
    , _nvtStorageClass   :: !TransitionStorageClass
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NoncurrentVersionTransition' smart constructor.
noncurrentVersionTransition :: Int -> TransitionStorageClass -> NoncurrentVersionTransition
noncurrentVersionTransition pNoncurrentDays_ pStorageClass_ =
    NoncurrentVersionTransition'
    { _nvtNoncurrentDays = pNoncurrentDays_
    , _nvtStorageClass = pStorageClass_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- </AmazonS3/latest/dev/s3-access-control.html How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the Amazon Simple Storage Service Developer Guide.
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays = lens _nvtNoncurrentDays (\ s a -> s{_nvtNoncurrentDays = a});

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = lens _nvtStorageClass (\ s a -> s{_nvtStorageClass = a});

instance FromXML NoncurrentVersionTransition where
        parseXML x
          = NoncurrentVersionTransition' <$>
              (x .@ "NoncurrentDays") <*> (x .@ "StorageClass")

instance ToXML NoncurrentVersionTransition where
        toXML NoncurrentVersionTransition'{..}
          = mconcat
              ["NoncurrentDays" @= _nvtNoncurrentDays,
               "StorageClass" @= _nvtStorageClass]

-- | Container for specifying the notification configuration of the bucket.
-- If this element is empty, notifications are turned off on the bucket.
--
-- /See:/ 'notificationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncQueueConfigurations'
--
-- * 'ncTopicConfigurations'
--
-- * 'ncLambdaFunctionConfigurations'
data NotificationConfiguration = NotificationConfiguration'
    { _ncQueueConfigurations          :: !(Maybe [QueueConfiguration])
    , _ncTopicConfigurations          :: !(Maybe [TopicConfiguration])
    , _ncLambdaFunctionConfigurations :: !(Maybe [LambdaFunctionConfiguration])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'NotificationConfiguration' smart constructor.
notificationConfiguration :: NotificationConfiguration
notificationConfiguration =
    NotificationConfiguration'
    { _ncQueueConfigurations = Nothing
    , _ncTopicConfigurations = Nothing
    , _ncLambdaFunctionConfigurations = Nothing
    }

-- | Undocumented member.
ncQueueConfigurations :: Lens' NotificationConfiguration [QueueConfiguration]
ncQueueConfigurations = lens _ncQueueConfigurations (\ s a -> s{_ncQueueConfigurations = a}) . _Default . _Coerce;

-- | Undocumented member.
ncTopicConfigurations :: Lens' NotificationConfiguration [TopicConfiguration]
ncTopicConfigurations = lens _ncTopicConfigurations (\ s a -> s{_ncTopicConfigurations = a}) . _Default . _Coerce;

-- | Undocumented member.
ncLambdaFunctionConfigurations :: Lens' NotificationConfiguration [LambdaFunctionConfiguration]
ncLambdaFunctionConfigurations = lens _ncLambdaFunctionConfigurations (\ s a -> s{_ncLambdaFunctionConfigurations = a}) . _Default . _Coerce;

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (may (parseXMLList "QueueConfiguration") x) <*>
                (may (parseXMLList "TopicConfiguration") x)
                <*>
                (may (parseXMLList "CloudFunctionConfiguration") x)

instance ToXML NotificationConfiguration where
        toXML NotificationConfiguration'{..}
          = mconcat
              [toXML
                 (toXMLList "QueueConfiguration" <$>
                    _ncQueueConfigurations),
               toXML
                 (toXMLList "TopicConfiguration" <$>
                    _ncTopicConfigurations),
               toXML
                 (toXMLList "CloudFunctionConfiguration" <$>
                    _ncLambdaFunctionConfigurations)]

-- | /See:/ 'object'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oOwner'
--
-- * 'oETag'
--
-- * 'oSize'
--
-- * 'oKey'
--
-- * 'oStorageClass'
--
-- * 'oLastModified'
data Object = Object'
    { _oOwner        :: !(Maybe Owner)
    , _oETag         :: !ETag
    , _oSize         :: !Int
    , _oKey          :: !ObjectKey
    , _oStorageClass :: !ObjectStorageClass
    , _oLastModified :: !RFC822
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Object' smart constructor.
object' :: ETag -> Int -> ObjectKey -> ObjectStorageClass -> UTCTime -> Object
object' pETag_ pSize_ pKey_ pStorageClass_ pLastModified_ =
    Object'
    { _oOwner = Nothing
    , _oETag = pETag_
    , _oSize = pSize_
    , _oKey = pKey_
    , _oStorageClass = pStorageClass_
    , _oLastModified = _Time # pLastModified_
    }

-- | Undocumented member.
oOwner :: Lens' Object (Maybe Owner)
oOwner = lens _oOwner (\ s a -> s{_oOwner = a});

-- | Undocumented member.
oETag :: Lens' Object ETag
oETag = lens _oETag (\ s a -> s{_oETag = a});

-- | Undocumented member.
oSize :: Lens' Object Int
oSize = lens _oSize (\ s a -> s{_oSize = a});

-- | Undocumented member.
oKey :: Lens' Object ObjectKey
oKey = lens _oKey (\ s a -> s{_oKey = a});

-- | The class of storage used to store the object.
oStorageClass :: Lens' Object ObjectStorageClass
oStorageClass = lens _oStorageClass (\ s a -> s{_oStorageClass = a});

-- | Undocumented member.
oLastModified :: Lens' Object UTCTime
oLastModified = lens _oLastModified (\ s a -> s{_oLastModified = a}) . _Time;

instance FromXML Object where
        parseXML x
          = Object' <$>
              (x .@? "Owner") <*> (x .@ "ETag") <*> (x .@ "Size")
                <*> (x .@ "Key")
                <*> (x .@ "StorageClass")
                <*> (x .@ "LastModified")

-- | /See:/ 'objectIdentifier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oiVersionId'
--
-- * 'oiKey'
data ObjectIdentifier = ObjectIdentifier'
    { _oiVersionId :: !(Maybe ObjectVersionId)
    , _oiKey       :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ObjectIdentifier' smart constructor.
objectIdentifier :: ObjectKey -> ObjectIdentifier
objectIdentifier pKey_ =
    ObjectIdentifier'
    { _oiVersionId = Nothing
    , _oiKey = pKey_
    }

-- | VersionId for the specific version of the object to delete.
oiVersionId :: Lens' ObjectIdentifier (Maybe ObjectVersionId)
oiVersionId = lens _oiVersionId (\ s a -> s{_oiVersionId = a});

-- | Key name of the object to delete.
oiKey :: Lens' ObjectIdentifier ObjectKey
oiKey = lens _oiKey (\ s a -> s{_oiKey = a});

instance ToXML ObjectIdentifier where
        toXML ObjectIdentifier'{..}
          = mconcat
              ["VersionId" @= _oiVersionId, "Key" @= _oiKey]

-- | /See:/ 'objectVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ovVersionId'
--
-- * 'ovETag'
--
-- * 'ovSize'
--
-- * 'ovIsLatest'
--
-- * 'ovOwner'
--
-- * 'ovKey'
--
-- * 'ovStorageClass'
--
-- * 'ovLastModified'
data ObjectVersion = ObjectVersion'
    { _ovVersionId    :: !(Maybe ObjectVersionId)
    , _ovETag         :: !(Maybe ETag)
    , _ovSize         :: !(Maybe Int)
    , _ovIsLatest     :: !(Maybe Bool)
    , _ovOwner        :: !(Maybe Owner)
    , _ovKey          :: !(Maybe ObjectKey)
    , _ovStorageClass :: !(Maybe ObjectVersionStorageClass)
    , _ovLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ObjectVersion' smart constructor.
objectVersion :: ObjectVersion
objectVersion =
    ObjectVersion'
    { _ovVersionId = Nothing
    , _ovETag = Nothing
    , _ovSize = Nothing
    , _ovIsLatest = Nothing
    , _ovOwner = Nothing
    , _ovKey = Nothing
    , _ovStorageClass = Nothing
    , _ovLastModified = Nothing
    }

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\ s a -> s{_ovVersionId = a});

-- | Undocumented member.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\ s a -> s{_ovETag = a});

-- | Size in bytes of the object.
ovSize :: Lens' ObjectVersion (Maybe Int)
ovSize = lens _ovSize (\ s a -> s{_ovSize = a});

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
ovIsLatest :: Lens' ObjectVersion (Maybe Bool)
ovIsLatest = lens _ovIsLatest (\ s a -> s{_ovIsLatest = a});

-- | Undocumented member.
ovOwner :: Lens' ObjectVersion (Maybe Owner)
ovOwner = lens _ovOwner (\ s a -> s{_ovOwner = a});

-- | The object key.
ovKey :: Lens' ObjectVersion (Maybe ObjectKey)
ovKey = lens _ovKey (\ s a -> s{_ovKey = a});

-- | The class of storage used to store the object.
ovStorageClass :: Lens' ObjectVersion (Maybe ObjectVersionStorageClass)
ovStorageClass = lens _ovStorageClass (\ s a -> s{_ovStorageClass = a});

-- | Date and time the object was last modified.
ovLastModified :: Lens' ObjectVersion (Maybe UTCTime)
ovLastModified = lens _ovLastModified (\ s a -> s{_ovLastModified = a}) . mapping _Time;

instance FromXML ObjectVersion where
        parseXML x
          = ObjectVersion' <$>
              (x .@? "VersionId") <*> (x .@? "ETag") <*>
                (x .@? "Size")
                <*> (x .@? "IsLatest")
                <*> (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "LastModified")

-- | /See:/ 'owner' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oId'
--
-- * 'oDisplayName'
data Owner = Owner'
    { _oId          :: !(Maybe Text)
    , _oDisplayName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Owner' smart constructor.
owner :: Owner
owner =
    Owner'
    { _oId = Nothing
    , _oDisplayName = Nothing
    }

-- | Undocumented member.
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a});

-- | Undocumented member.
oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\ s a -> s{_oDisplayName = a});

instance FromXML Owner where
        parseXML x
          = Owner' <$> (x .@? "ID") <*> (x .@? "DisplayName")

instance ToXML Owner where
        toXML Owner'{..}
          = mconcat
              ["ID" @= _oId, "DisplayName" @= _oDisplayName]

-- | /See:/ 'part' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pETag'
--
-- * 'pSize'
--
-- * 'pPartNumber'
--
-- * 'pLastModified'
data Part = Part'
    { _pETag         :: !(Maybe ETag)
    , _pSize         :: !(Maybe Int)
    , _pPartNumber   :: !(Maybe Int)
    , _pLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Part' smart constructor.
part :: Part
part =
    Part'
    { _pETag = Nothing
    , _pSize = Nothing
    , _pPartNumber = Nothing
    , _pLastModified = Nothing
    }

-- | Entity tag returned when the part was uploaded.
pETag :: Lens' Part (Maybe ETag)
pETag = lens _pETag (\ s a -> s{_pETag = a});

-- | Size of the uploaded part data.
pSize :: Lens' Part (Maybe Int)
pSize = lens _pSize (\ s a -> s{_pSize = a});

-- | Part number identifying the part. This is a positive integer between 1
-- and 10,000.
pPartNumber :: Lens' Part (Maybe Int)
pPartNumber = lens _pPartNumber (\ s a -> s{_pPartNumber = a});

-- | Date and time at which the part was uploaded.
pLastModified :: Lens' Part (Maybe UTCTime)
pLastModified = lens _pLastModified (\ s a -> s{_pLastModified = a}) . mapping _Time;

instance FromXML Part where
        parseXML x
          = Part' <$>
              (x .@? "ETag") <*> (x .@? "Size") <*>
                (x .@? "PartNumber")
                <*> (x .@? "LastModified")

-- | Container for specifying an configuration when you want Amazon S3 to
-- publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
--
-- /See:/ 'queueConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qcId'
--
-- * 'qcQueueARN'
--
-- * 'qcEvents'
data QueueConfiguration = QueueConfiguration'
    { _qcId       :: !(Maybe Text)
    , _qcQueueARN :: !Text
    , _qcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'QueueConfiguration' smart constructor.
queueConfiguration :: Text -> QueueConfiguration
queueConfiguration pQueueARN_ =
    QueueConfiguration'
    { _qcId = Nothing
    , _qcQueueARN = pQueueARN_
    , _qcEvents = mempty
    }

-- | Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\ s a -> s{_qcId = a});

-- | Amazon SQS queue ARN to which Amazon S3 will publish a message when it
-- detects events of specified type.
qcQueueARN :: Lens' QueueConfiguration Text
qcQueueARN = lens _qcQueueARN (\ s a -> s{_qcQueueARN = a});

-- | Undocumented member.
qcEvents :: Lens' QueueConfiguration [Event]
qcEvents = lens _qcEvents (\ s a -> s{_qcEvents = a}) . _Coerce;

instance FromXML QueueConfiguration where
        parseXML x
          = QueueConfiguration' <$>
              (x .@? "Id") <*> (x .@ "Queue") <*>
                (parseXMLList "Event" x)

instance ToXML QueueConfiguration where
        toXML QueueConfiguration'{..}
          = mconcat
              ["Id" @= _qcId, "Queue" @= _qcQueueARN,
               toXMLList "Event" _qcEvents]

-- | /See:/ 'redirect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rHostName'
--
-- * 'rProtocol'
--
-- * 'rHTTPRedirectCode'
--
-- * 'rReplaceKeyWith'
--
-- * 'rReplaceKeyPrefixWith'
data Redirect = Redirect'
    { _rHostName             :: !(Maybe Text)
    , _rProtocol             :: !(Maybe Protocol)
    , _rHTTPRedirectCode     :: !(Maybe Text)
    , _rReplaceKeyWith       :: !(Maybe Text)
    , _rReplaceKeyPrefixWith :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Redirect' smart constructor.
redirect :: Redirect
redirect =
    Redirect'
    { _rHostName = Nothing
    , _rProtocol = Nothing
    , _rHTTPRedirectCode = Nothing
    , _rReplaceKeyWith = Nothing
    , _rReplaceKeyPrefixWith = Nothing
    }

-- | The host name to use in the redirect request.
rHostName :: Lens' Redirect (Maybe Text)
rHostName = lens _rHostName (\ s a -> s{_rHostName = a});

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
rProtocol :: Lens' Redirect (Maybe Protocol)
rProtocol = lens _rProtocol (\ s a -> s{_rProtocol = a});

-- | The HTTP redirect code to use on the response. Not required if one of
-- the siblings is present.
rHTTPRedirectCode :: Lens' Redirect (Maybe Text)
rHTTPRedirectCode = lens _rHTTPRedirectCode (\ s a -> s{_rHTTPRedirectCode = a});

-- | The specific object key to use in the redirect request. For example,
-- redirect request to error.html. Not required if one of the sibling is
-- present. Can be present only if ReplaceKeyPrefixWith is not provided.
rReplaceKeyWith :: Lens' Redirect (Maybe Text)
rReplaceKeyWith = lens _rReplaceKeyWith (\ s a -> s{_rReplaceKeyWith = a});

-- | The object key prefix to use in the redirect request. For example, to
-- redirect requests for all pages with prefix docs\/ (objects in the
-- docs\/ folder) to documents\/, you can set a condition block with
-- KeyPrefixEquals set to docs\/ and in the Redirect set
-- ReplaceKeyPrefixWith to \/documents. Not required if one of the siblings
-- is present. Can be present only if ReplaceKeyWith is not provided.
rReplaceKeyPrefixWith :: Lens' Redirect (Maybe Text)
rReplaceKeyPrefixWith = lens _rReplaceKeyPrefixWith (\ s a -> s{_rReplaceKeyPrefixWith = a});

instance FromXML Redirect where
        parseXML x
          = Redirect' <$>
              (x .@? "HostName") <*> (x .@? "Protocol") <*>
                (x .@? "HttpRedirectCode")
                <*> (x .@? "ReplaceKeyWith")
                <*> (x .@? "ReplaceKeyPrefixWith")

instance ToXML Redirect where
        toXML Redirect'{..}
          = mconcat
              ["HostName" @= _rHostName, "Protocol" @= _rProtocol,
               "HttpRedirectCode" @= _rHTTPRedirectCode,
               "ReplaceKeyWith" @= _rReplaceKeyWith,
               "ReplaceKeyPrefixWith" @= _rReplaceKeyPrefixWith]

-- | /See:/ 'redirectAllRequestsTo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rartProtocol'
--
-- * 'rartHostName'
data RedirectAllRequestsTo = RedirectAllRequestsTo'
    { _rartProtocol :: !(Maybe Protocol)
    , _rartHostName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RedirectAllRequestsTo' smart constructor.
redirectAllRequestsTo :: Text -> RedirectAllRequestsTo
redirectAllRequestsTo pHostName_ =
    RedirectAllRequestsTo'
    { _rartProtocol = Nothing
    , _rartHostName = pHostName_
    }

-- | Protocol to use (http, https) when redirecting requests. The default is
-- the protocol that is used in the original request.
rartProtocol :: Lens' RedirectAllRequestsTo (Maybe Protocol)
rartProtocol = lens _rartProtocol (\ s a -> s{_rartProtocol = a});

-- | Name of the host where requests will be redirected.
rartHostName :: Lens' RedirectAllRequestsTo Text
rartHostName = lens _rartHostName (\ s a -> s{_rartHostName = a});

instance FromXML RedirectAllRequestsTo where
        parseXML x
          = RedirectAllRequestsTo' <$>
              (x .@? "Protocol") <*> (x .@ "HostName")

instance ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo'{..}
          = mconcat
              ["Protocol" @= _rartProtocol,
               "HostName" @= _rartHostName]

-- | Container for replication rules. You can add as many as 1,000 rules.
-- Total replication configuration size can be up to 2 MB.
--
-- /See:/ 'replicationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRole'
--
-- * 'rcRules'
data ReplicationConfiguration = ReplicationConfiguration'
    { _rcRole  :: !Text
    , _rcRules :: ![ReplicationRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplicationConfiguration' smart constructor.
replicationConfiguration :: Text -> ReplicationConfiguration
replicationConfiguration pRole_ =
    ReplicationConfiguration'
    { _rcRole = pRole_
    , _rcRules = mempty
    }

-- | Amazon Resource Name (ARN) of an IAM role for Amazon S3 to assume when
-- replicating the objects.
rcRole :: Lens' ReplicationConfiguration Text
rcRole = lens _rcRole (\ s a -> s{_rcRole = a});

-- | Container for information about a particular replication rule.
-- Replication configuration must have at least one rule and can contain up
-- to 1,000 rules.
rcRules :: Lens' ReplicationConfiguration [ReplicationRule]
rcRules = lens _rcRules (\ s a -> s{_rcRules = a}) . _Coerce;

instance FromXML ReplicationConfiguration where
        parseXML x
          = ReplicationConfiguration' <$>
              (x .@ "Role") <*> (parseXMLList "Rule" x)

instance ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration'{..}
          = mconcat
              ["Role" @= _rcRole, toXMLList "Rule" _rcRules]

-- | /See:/ 'replicationRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrId'
--
-- * 'rrPrefix'
--
-- * 'rrStatus'
--
-- * 'rrDestination'
data ReplicationRule = ReplicationRule'
    { _rrId          :: !(Maybe Text)
    , _rrPrefix      :: !Text
    , _rrStatus      :: !ReplicationRuleStatus
    , _rrDestination :: !Destination
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplicationRule' smart constructor.
replicationRule :: Text -> ReplicationRuleStatus -> Destination -> ReplicationRule
replicationRule pPrefix_ pStatus_ pDestination_ =
    ReplicationRule'
    { _rrId = Nothing
    , _rrPrefix = pPrefix_
    , _rrStatus = pStatus_
    , _rrDestination = pDestination_
    }

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rrId :: Lens' ReplicationRule (Maybe Text)
rrId = lens _rrId (\ s a -> s{_rrId = a});

-- | Object keyname prefix identifying one or more objects to which the rule
-- applies. Maximum prefix length can be up to 1,024 characters.
-- Overlapping prefixes are not supported.
rrPrefix :: Lens' ReplicationRule Text
rrPrefix = lens _rrPrefix (\ s a -> s{_rrPrefix = a});

-- | The rule is ignored if status is not Enabled.
rrStatus :: Lens' ReplicationRule ReplicationRuleStatus
rrStatus = lens _rrStatus (\ s a -> s{_rrStatus = a});

-- | Undocumented member.
rrDestination :: Lens' ReplicationRule Destination
rrDestination = lens _rrDestination (\ s a -> s{_rrDestination = a});

instance FromXML ReplicationRule where
        parseXML x
          = ReplicationRule' <$>
              (x .@? "ID") <*> (x .@ "Prefix") <*> (x .@ "Status")
                <*> (x .@ "Destination")

instance ToXML ReplicationRule where
        toXML ReplicationRule'{..}
          = mconcat
              ["ID" @= _rrId, "Prefix" @= _rrPrefix,
               "Status" @= _rrStatus,
               "Destination" @= _rrDestination]

-- | /See:/ 'requestPaymentConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpcPayer'
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
    { _rpcPayer :: Payer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestPaymentConfiguration' smart constructor.
requestPaymentConfiguration :: Payer -> RequestPaymentConfiguration
requestPaymentConfiguration pPayer_ =
    RequestPaymentConfiguration'
    { _rpcPayer = pPayer_
    }

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\ s a -> s{_rpcPayer = a});

instance ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration'{..}
          = mconcat ["Payer" @= _rpcPayer]

-- | /See:/ 'restoreRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrDays'
newtype RestoreRequest = RestoreRequest'
    { _rrDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreRequest' smart constructor.
restoreRequest :: Int -> RestoreRequest
restoreRequest pDays_ =
    RestoreRequest'
    { _rrDays = pDays_
    }

-- | Lifetime of the active copy in days
rrDays :: Lens' RestoreRequest Int
rrDays = lens _rrDays (\ s a -> s{_rrDays = a});

instance ToXML RestoreRequest where
        toXML RestoreRequest'{..}
          = mconcat ["Days" @= _rrDays]

-- | /See:/ 'routingRule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrCondition'
--
-- * 'rrRedirect'
data RoutingRule = RoutingRule'
    { _rrCondition :: !(Maybe Condition)
    , _rrRedirect  :: !Redirect
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RoutingRule' smart constructor.
routingRule :: Redirect -> RoutingRule
routingRule pRedirect_ =
    RoutingRule'
    { _rrCondition = Nothing
    , _rrRedirect = pRedirect_
    }

-- | A container for describing a condition that must be met for the
-- specified redirect to apply. For example, 1. If request is for pages in
-- the \/docs folder, redirect to the \/documents folder. 2. If request
-- results in HTTP error 4xx, redirect request to another host where you
-- might process the error.
rrCondition :: Lens' RoutingRule (Maybe Condition)
rrCondition = lens _rrCondition (\ s a -> s{_rrCondition = a});

-- | Container for redirect information. You can redirect requests to another
-- host, to another page, or with another protocol. In the event of an
-- error, you can can specify a different error code to return.
rrRedirect :: Lens' RoutingRule Redirect
rrRedirect = lens _rrRedirect (\ s a -> s{_rrRedirect = a});

instance FromXML RoutingRule where
        parseXML x
          = RoutingRule' <$>
              (x .@? "Condition") <*> (x .@ "Redirect")

instance ToXML RoutingRule where
        toXML RoutingRule'{..}
          = mconcat
              ["Condition" @= _rrCondition,
               "Redirect" @= _rrRedirect]

-- | /See:/ 'rule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rNoncurrentVersionExpiration'
--
-- * 'rTransition'
--
-- * 'rExpiration'
--
-- * 'rNoncurrentVersionTransition'
--
-- * 'rId'
--
-- * 'rPrefix'
--
-- * 'rStatus'
data Rule = Rule'
    { _rNoncurrentVersionExpiration :: !(Maybe NoncurrentVersionExpiration)
    , _rTransition                  :: !(Maybe Transition)
    , _rExpiration                  :: !(Maybe LifecycleExpiration)
    , _rNoncurrentVersionTransition :: !(Maybe NoncurrentVersionTransition)
    , _rId                          :: !(Maybe Text)
    , _rPrefix                      :: !Text
    , _rStatus                      :: !ExpirationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Rule' smart constructor.
rule :: Text -> ExpirationStatus -> Rule
rule pPrefix_ pStatus_ =
    Rule'
    { _rNoncurrentVersionExpiration = Nothing
    , _rTransition = Nothing
    , _rExpiration = Nothing
    , _rNoncurrentVersionTransition = Nothing
    , _rId = Nothing
    , _rPrefix = pPrefix_
    , _rStatus = pStatus_
    }

-- | Undocumented member.
rNoncurrentVersionExpiration :: Lens' Rule (Maybe NoncurrentVersionExpiration)
rNoncurrentVersionExpiration = lens _rNoncurrentVersionExpiration (\ s a -> s{_rNoncurrentVersionExpiration = a});

-- | Undocumented member.
rTransition :: Lens' Rule (Maybe Transition)
rTransition = lens _rTransition (\ s a -> s{_rTransition = a});

-- | Undocumented member.
rExpiration :: Lens' Rule (Maybe LifecycleExpiration)
rExpiration = lens _rExpiration (\ s a -> s{_rExpiration = a});

-- | Undocumented member.
rNoncurrentVersionTransition :: Lens' Rule (Maybe NoncurrentVersionTransition)
rNoncurrentVersionTransition = lens _rNoncurrentVersionTransition (\ s a -> s{_rNoncurrentVersionTransition = a});

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
rId :: Lens' Rule (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a});

-- | Prefix identifying one or more objects to which the rule applies.
rPrefix :: Lens' Rule Text
rPrefix = lens _rPrefix (\ s a -> s{_rPrefix = a});

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
rStatus :: Lens' Rule ExpirationStatus
rStatus = lens _rStatus (\ s a -> s{_rStatus = a});

instance FromXML Rule where
        parseXML x
          = Rule' <$>
              (x .@? "NoncurrentVersionExpiration") <*>
                (x .@? "Transition")
                <*> (x .@? "Expiration")
                <*> (x .@? "NoncurrentVersionTransition")
                <*> (x .@? "ID")
                <*> (x .@ "Prefix")
                <*> (x .@ "Status")

instance ToXML Rule where
        toXML Rule'{..}
          = mconcat
              ["NoncurrentVersionExpiration" @=
                 _rNoncurrentVersionExpiration,
               "Transition" @= _rTransition,
               "Expiration" @= _rExpiration,
               "NoncurrentVersionTransition" @=
                 _rNoncurrentVersionTransition,
               "ID" @= _rId, "Prefix" @= _rPrefix,
               "Status" @= _rStatus]

-- | /See:/ 's3ServiceError' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sseVersionId'
--
-- * 'sseKey'
--
-- * 'sseCode'
--
-- * 'sseMessage'
data S3ServiceError = S3ServiceError'
    { _sseVersionId :: !(Maybe ObjectVersionId)
    , _sseKey       :: !(Maybe ObjectKey)
    , _sseCode      :: !(Maybe Text)
    , _sseMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'S3ServiceError' smart constructor.
s3ServiceError :: S3ServiceError
s3ServiceError =
    S3ServiceError'
    { _sseVersionId = Nothing
    , _sseKey = Nothing
    , _sseCode = Nothing
    , _sseMessage = Nothing
    }

-- | Undocumented member.
sseVersionId :: Lens' S3ServiceError (Maybe ObjectVersionId)
sseVersionId = lens _sseVersionId (\ s a -> s{_sseVersionId = a});

-- | Undocumented member.
sseKey :: Lens' S3ServiceError (Maybe ObjectKey)
sseKey = lens _sseKey (\ s a -> s{_sseKey = a});

-- | Undocumented member.
sseCode :: Lens' S3ServiceError (Maybe Text)
sseCode = lens _sseCode (\ s a -> s{_sseCode = a});

-- | Undocumented member.
sseMessage :: Lens' S3ServiceError (Maybe Text)
sseMessage = lens _sseMessage (\ s a -> s{_sseMessage = a});

instance FromXML S3ServiceError where
        parseXML x
          = S3ServiceError' <$>
              (x .@? "VersionId") <*> (x .@? "Key") <*>
                (x .@? "Code")
                <*> (x .@? "Message")

-- | /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey'
--
-- * 'tagValue'
data Tag = Tag'
    { _tagKey   :: !ObjectKey
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: ObjectKey -> Text -> Tag
tag pKey_ pValue_ =
    Tag'
    { _tagKey = pKey_
    , _tagValue = pValue_
    }

-- | Name of the tag.
tagKey :: Lens' Tag ObjectKey
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | Value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromXML Tag where
        parseXML x = Tag' <$> (x .@ "Key") <*> (x .@ "Value")

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Key" @= _tagKey, "Value" @= _tagValue]

-- | /See:/ 'tagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tTagSet'
newtype Tagging = Tagging'
    { _tTagSet :: [Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tagging' smart constructor.
tagging :: Tagging
tagging =
    Tagging'
    { _tTagSet = mempty
    }

-- | Undocumented member.
tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\ s a -> s{_tTagSet = a}) . _Coerce;

instance ToXML Tagging where
        toXML Tagging'{..}
          = mconcat ["TagSet" @= toXMLList "Tag" _tTagSet]

-- | /See:/ 'targetGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tgPermission'
--
-- * 'tgGrantee'
data TargetGrant = TargetGrant'
    { _tgPermission :: !(Maybe BucketLogsPermission)
    , _tgGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TargetGrant' smart constructor.
targetGrant :: TargetGrant
targetGrant =
    TargetGrant'
    { _tgPermission = Nothing
    , _tgGrantee = Nothing
    }

-- | Logging permissions assigned to the Grantee for the bucket.
tgPermission :: Lens' TargetGrant (Maybe BucketLogsPermission)
tgPermission = lens _tgPermission (\ s a -> s{_tgPermission = a});

-- | Undocumented member.
tgGrantee :: Lens' TargetGrant (Maybe Grantee)
tgGrantee = lens _tgGrantee (\ s a -> s{_tgGrantee = a});

instance FromXML TargetGrant where
        parseXML x
          = TargetGrant' <$>
              (x .@? "Permission") <*> (x .@? "Grantee")

instance ToXML TargetGrant where
        toXML TargetGrant'{..}
          = mconcat
              ["Permission" @= _tgPermission,
               "Grantee" @= _tgGrantee]

-- | Container for specifying the configuration when you want Amazon S3 to
-- publish events to an Amazon Simple Notification Service (Amazon SNS)
-- topic.
--
-- /See:/ 'topicConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tcId'
--
-- * 'tcTopicARN'
--
-- * 'tcEvents'
data TopicConfiguration = TopicConfiguration'
    { _tcId       :: !(Maybe Text)
    , _tcTopicARN :: !Text
    , _tcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TopicConfiguration' smart constructor.
topicConfiguration :: Text -> TopicConfiguration
topicConfiguration pTopicARN_ =
    TopicConfiguration'
    { _tcId = Nothing
    , _tcTopicARN = pTopicARN_
    , _tcEvents = mempty
    }

-- | Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\ s a -> s{_tcId = a});

-- | Amazon SNS topic ARN to which Amazon S3 will publish a message when it
-- detects events of specified type.
tcTopicARN :: Lens' TopicConfiguration Text
tcTopicARN = lens _tcTopicARN (\ s a -> s{_tcTopicARN = a});

-- | Undocumented member.
tcEvents :: Lens' TopicConfiguration [Event]
tcEvents = lens _tcEvents (\ s a -> s{_tcEvents = a}) . _Coerce;

instance FromXML TopicConfiguration where
        parseXML x
          = TopicConfiguration' <$>
              (x .@? "Id") <*> (x .@ "Topic") <*>
                (parseXMLList "Event" x)

instance ToXML TopicConfiguration where
        toXML TopicConfiguration'{..}
          = mconcat
              ["Id" @= _tcId, "Topic" @= _tcTopicARN,
               toXMLList "Event" _tcEvents]

-- | /See:/ 'transition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tDays'
--
-- * 'tDate'
--
-- * 'tStorageClass'
data Transition = Transition'
    { _tDays         :: !(Maybe Int)
    , _tDate         :: !(Maybe RFC822)
    , _tStorageClass :: !(Maybe TransitionStorageClass)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Transition' smart constructor.
transition :: Transition
transition =
    Transition'
    { _tDays = Nothing
    , _tDate = Nothing
    , _tStorageClass = Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
tDays :: Lens' Transition (Maybe Int)
tDays = lens _tDays (\ s a -> s{_tDays = a});

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
tDate :: Lens' Transition (Maybe UTCTime)
tDate = lens _tDate (\ s a -> s{_tDate = a}) . mapping _Time;

-- | The class of storage used to store the object.
tStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
tStorageClass = lens _tStorageClass (\ s a -> s{_tStorageClass = a});

instance FromXML Transition where
        parseXML x
          = Transition' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "StorageClass")

instance ToXML Transition where
        toXML Transition'{..}
          = mconcat
              ["Days" @= _tDays, "Date" @= _tDate,
               "StorageClass" @= _tStorageClass]

-- | /See:/ 'versioningConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcStatus'
--
-- * 'vcMFADelete'
data VersioningConfiguration = VersioningConfiguration'
    { _vcStatus    :: !(Maybe BucketVersioningStatus)
    , _vcMFADelete :: !(Maybe MFADelete)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VersioningConfiguration' smart constructor.
versioningConfiguration :: VersioningConfiguration
versioningConfiguration =
    VersioningConfiguration'
    { _vcStatus = Nothing
    , _vcMFADelete = Nothing
    }

-- | The versioning state of the bucket.
vcStatus :: Lens' VersioningConfiguration (Maybe BucketVersioningStatus)
vcStatus = lens _vcStatus (\ s a -> s{_vcStatus = a});

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
vcMFADelete :: Lens' VersioningConfiguration (Maybe MFADelete)
vcMFADelete = lens _vcMFADelete (\ s a -> s{_vcMFADelete = a});

instance ToXML VersioningConfiguration where
        toXML VersioningConfiguration'{..}
          = mconcat
              ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]

-- | /See:/ 'websiteConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wcRedirectAllRequestsTo'
--
-- * 'wcErrorDocument'
--
-- * 'wcRoutingRules'
--
-- * 'wcIndexDocument'
data WebsiteConfiguration = WebsiteConfiguration'
    { _wcRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
    , _wcErrorDocument         :: !(Maybe ErrorDocument)
    , _wcRoutingRules          :: !(Maybe [RoutingRule])
    , _wcIndexDocument         :: !(Maybe IndexDocument)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'WebsiteConfiguration' smart constructor.
websiteConfiguration :: WebsiteConfiguration
websiteConfiguration =
    WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing
    , _wcErrorDocument = Nothing
    , _wcRoutingRules = Nothing
    , _wcIndexDocument = Nothing
    }

-- | Undocumented member.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\ s a -> s{_wcRedirectAllRequestsTo = a});

-- | Undocumented member.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\ s a -> s{_wcErrorDocument = a});

-- | Undocumented member.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\ s a -> s{_wcRoutingRules = a}) . _Default . _Coerce;

-- | Undocumented member.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\ s a -> s{_wcIndexDocument = a});

instance ToXML WebsiteConfiguration where
        toXML WebsiteConfiguration'{..}
          = mconcat
              ["RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
               "ErrorDocument" @= _wcErrorDocument,
               "RoutingRules" @=
                 toXML (toXMLList "RoutingRule" <$> _wcRoutingRules),
               "IndexDocument" @= _wcIndexDocument]
