{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.S3.Internal
import           Network.AWS.S3.Types.Sum

-- | Specifies the days since the initiation of an Incomplete Multipart
-- Upload that Lifecycle will wait before permanently removing all parts of
-- the upload.
--
-- /See:/ 'abortIncompleteMultipartUpload' smart constructor.
newtype AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
    { _aimuDaysAfterInitiation :: Maybe Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AbortIncompleteMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aimuDaysAfterInitiation'
abortIncompleteMultipartUpload
    :: AbortIncompleteMultipartUpload
abortIncompleteMultipartUpload =
    AbortIncompleteMultipartUpload'
    { _aimuDaysAfterInitiation = Nothing
    }

-- | Indicates the number of days that must pass since initiation for
-- Lifecycle to abort an Incomplete Multipart Upload.
aimuDaysAfterInitiation :: Lens' AbortIncompleteMultipartUpload (Maybe Int)
aimuDaysAfterInitiation = lens _aimuDaysAfterInitiation (\ s a -> s{_aimuDaysAfterInitiation = a});

instance FromXML AbortIncompleteMultipartUpload where
        parseXML x
          = AbortIncompleteMultipartUpload' <$>
              (x .@? "DaysAfterInitiation")

instance Hashable AbortIncompleteMultipartUpload

instance NFData AbortIncompleteMultipartUpload

instance ToXML AbortIncompleteMultipartUpload where
        toXML AbortIncompleteMultipartUpload'{..}
          = mconcat
              ["DaysAfterInitiation" @= _aimuDaysAfterInitiation]

-- | /See:/ 'accelerateConfiguration' smart constructor.
newtype AccelerateConfiguration = AccelerateConfiguration'
    { _acStatus :: Maybe BucketAccelerateStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acStatus'
accelerateConfiguration
    :: AccelerateConfiguration
accelerateConfiguration =
    AccelerateConfiguration'
    { _acStatus = Nothing
    }

-- | The accelerate configuration of the bucket.
acStatus :: Lens' AccelerateConfiguration (Maybe BucketAccelerateStatus)
acStatus = lens _acStatus (\ s a -> s{_acStatus = a});

instance Hashable AccelerateConfiguration

instance NFData AccelerateConfiguration

instance ToXML AccelerateConfiguration where
        toXML AccelerateConfiguration'{..}
          = mconcat ["Status" @= _acStatus]

-- | /See:/ 'accessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
    { _acpGrants :: !(Maybe [Grant])
    , _acpOwner  :: !(Maybe Owner)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccessControlPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acpGrants'
--
-- * 'acpOwner'
accessControlPolicy
    :: AccessControlPolicy
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

instance Hashable AccessControlPolicy

instance NFData AccessControlPolicy

instance ToXML AccessControlPolicy where
        toXML AccessControlPolicy'{..}
          = mconcat
              ["AccessControlList" @=
                 toXML (toXMLList "Grant" <$> _acpGrants),
               "Owner" @= _acpOwner]

-- | /See:/ 'bucket' smart constructor.
data Bucket = Bucket'
    { _bCreationDate :: !RFC822
    , _bName         :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationDate'
--
-- * 'bName'
bucket
    :: UTCTime -- ^ 'bCreationDate'
    -> BucketName -- ^ 'bName'
    -> Bucket
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

instance Hashable Bucket

instance NFData Bucket

-- | /See:/ 'bucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
    { _blcRules :: [LifecycleRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blcRules'
bucketLifecycleConfiguration
    :: BucketLifecycleConfiguration
bucketLifecycleConfiguration =
    BucketLifecycleConfiguration'
    { _blcRules = mempty
    }

-- | Undocumented member.
blcRules :: Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = lens _blcRules (\ s a -> s{_blcRules = a}) . _Coerce;

instance Hashable BucketLifecycleConfiguration

instance NFData BucketLifecycleConfiguration

instance ToXML BucketLifecycleConfiguration where
        toXML BucketLifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _blcRules]

-- | /See:/ 'bucketLoggingStatus' smart constructor.
newtype BucketLoggingStatus = BucketLoggingStatus'
    { _blsLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BucketLoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blsLoggingEnabled'
bucketLoggingStatus
    :: BucketLoggingStatus
bucketLoggingStatus =
    BucketLoggingStatus'
    { _blsLoggingEnabled = Nothing
    }

-- | Undocumented member.
blsLoggingEnabled :: Lens' BucketLoggingStatus (Maybe LoggingEnabled)
blsLoggingEnabled = lens _blsLoggingEnabled (\ s a -> s{_blsLoggingEnabled = a});

instance Hashable BucketLoggingStatus

instance NFData BucketLoggingStatus

instance ToXML BucketLoggingStatus where
        toXML BucketLoggingStatus'{..}
          = mconcat ["LoggingEnabled" @= _blsLoggingEnabled]

-- | /See:/ 'corsConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
    { _ccCORSRules :: [CORSRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CORSConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCORSRules'
corsConfiguration
    :: CORSConfiguration
corsConfiguration =
    CORSConfiguration'
    { _ccCORSRules = mempty
    }

-- | Undocumented member.
ccCORSRules :: Lens' CORSConfiguration [CORSRule]
ccCORSRules = lens _ccCORSRules (\ s a -> s{_ccCORSRules = a}) . _Coerce;

instance Hashable CORSConfiguration

instance NFData CORSConfiguration

instance ToXML CORSConfiguration where
        toXML CORSConfiguration'{..}
          = mconcat [toXMLList "CORSRule" _ccCORSRules]

-- | /See:/ 'corsRule' smart constructor.
data CORSRule = CORSRule'
    { _crMaxAgeSeconds  :: !(Maybe Int)
    , _crAllowedHeaders :: !(Maybe [Text])
    , _crExposeHeaders  :: !(Maybe [Text])
    , _crAllowedMethods :: ![Text]
    , _crAllowedOrigins :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crMaxAgeSeconds'
--
-- * 'crAllowedHeaders'
--
-- * 'crExposeHeaders'
--
-- * 'crAllowedMethods'
--
-- * 'crAllowedOrigins'
corsRule
    :: CORSRule
corsRule =
    CORSRule'
    { _crMaxAgeSeconds = Nothing
    , _crAllowedHeaders = Nothing
    , _crExposeHeaders = Nothing
    , _crAllowedMethods = mempty
    , _crAllowedOrigins = mempty
    }

-- | The time in seconds that your browser is to cache the preflight response
-- for the specified resource.
crMaxAgeSeconds :: Lens' CORSRule (Maybe Int)
crMaxAgeSeconds = lens _crMaxAgeSeconds (\ s a -> s{_crMaxAgeSeconds = a});

-- | Specifies which headers are allowed in a pre-flight OPTIONS request.
crAllowedHeaders :: Lens' CORSRule [Text]
crAllowedHeaders = lens _crAllowedHeaders (\ s a -> s{_crAllowedHeaders = a}) . _Default . _Coerce;

-- | One or more headers in the response that you want customers to be able
-- to access from their applications (for example, from a JavaScript
-- XMLHttpRequest object).
crExposeHeaders :: Lens' CORSRule [Text]
crExposeHeaders = lens _crExposeHeaders (\ s a -> s{_crExposeHeaders = a}) . _Default . _Coerce;

-- | Identifies HTTP methods that the domain\/origin specified in the rule is
-- allowed to execute.
crAllowedMethods :: Lens' CORSRule [Text]
crAllowedMethods = lens _crAllowedMethods (\ s a -> s{_crAllowedMethods = a}) . _Coerce;

-- | One or more origins you want customers to be able to access the bucket
-- from.
crAllowedOrigins :: Lens' CORSRule [Text]
crAllowedOrigins = lens _crAllowedOrigins (\ s a -> s{_crAllowedOrigins = a}) . _Coerce;

instance FromXML CORSRule where
        parseXML x
          = CORSRule' <$>
              (x .@? "MaxAgeSeconds") <*>
                (may (parseXMLList "AllowedHeader") x)
                <*> (may (parseXMLList "ExposeHeader") x)
                <*> (parseXMLList "AllowedMethod" x)
                <*> (parseXMLList "AllowedOrigin" x)

instance Hashable CORSRule

instance NFData CORSRule

instance ToXML CORSRule where
        toXML CORSRule'{..}
          = mconcat
              ["MaxAgeSeconds" @= _crMaxAgeSeconds,
               toXML
                 (toXMLList "AllowedHeader" <$> _crAllowedHeaders),
               toXML
                 (toXMLList "ExposeHeader" <$> _crExposeHeaders),
               toXMLList "AllowedMethod" _crAllowedMethods,
               toXMLList "AllowedOrigin" _crAllowedOrigins]

-- | /See:/ 'commonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix'
    { _cpPrefix :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CommonPrefix' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPrefix'
commonPrefix
    :: CommonPrefix
commonPrefix =
    CommonPrefix'
    { _cpPrefix = Nothing
    }

-- | Undocumented member.
cpPrefix :: Lens' CommonPrefix (Maybe Text)
cpPrefix = lens _cpPrefix (\ s a -> s{_cpPrefix = a});

instance FromXML CommonPrefix where
        parseXML x = CommonPrefix' <$> (x .@? "Prefix")

instance Hashable CommonPrefix

instance NFData CommonPrefix

-- | /See:/ 'completedMultipartUpload' smart constructor.
newtype CompletedMultipartUpload = CompletedMultipartUpload'
    { _cmuParts :: Maybe (List1 CompletedPart)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompletedMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuParts'
completedMultipartUpload
    :: CompletedMultipartUpload
completedMultipartUpload =
    CompletedMultipartUpload'
    { _cmuParts = Nothing
    }

-- | Undocumented member.
cmuParts :: Lens' CompletedMultipartUpload (Maybe (NonEmpty CompletedPart))
cmuParts = lens _cmuParts (\ s a -> s{_cmuParts = a}) . mapping _List1;

instance Hashable CompletedMultipartUpload

instance NFData CompletedMultipartUpload

instance ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload'{..}
          = mconcat [toXML (toXMLList "Part" <$> _cmuParts)]

-- | /See:/ 'completedPart' smart constructor.
data CompletedPart = CompletedPart'
    { _cpPartNumber :: !Int
    , _cpETag       :: !ETag
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CompletedPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPartNumber'
--
-- * 'cpETag'
completedPart
    :: Int -- ^ 'cpPartNumber'
    -> ETag -- ^ 'cpETag'
    -> CompletedPart
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

instance Hashable CompletedPart

instance NFData CompletedPart

instance ToXML CompletedPart where
        toXML CompletedPart'{..}
          = mconcat
              ["PartNumber" @= _cpPartNumber, "ETag" @= _cpETag]

-- | /See:/ 'condition' smart constructor.
data Condition = Condition'
    { _cKeyPrefixEquals             :: !(Maybe Text)
    , _cHTTPErrorCodeReturnedEquals :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cKeyPrefixEquals'
--
-- * 'cHTTPErrorCodeReturnedEquals'
condition
    :: Condition
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

instance Hashable Condition

instance NFData Condition

instance ToXML Condition where
        toXML Condition'{..}
          = mconcat
              ["KeyPrefixEquals" @= _cKeyPrefixEquals,
               "HttpErrorCodeReturnedEquals" @=
                 _cHTTPErrorCodeReturnedEquals]

-- | /See:/ 'copyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
    { _corETag         :: !(Maybe ETag)
    , _corLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyObjectResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corETag'
--
-- * 'corLastModified'
copyObjectResult
    :: CopyObjectResult
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

instance Hashable CopyObjectResult

instance NFData CopyObjectResult

-- | /See:/ 'copyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
    { _cprETag         :: !(Maybe ETag)
    , _cprLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyPartResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprETag'
--
-- * 'cprLastModified'
copyPartResult
    :: CopyPartResult
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

instance Hashable CopyPartResult

instance NFData CopyPartResult

-- | /See:/ 'createBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
    { _cbcLocationConstraint :: Maybe LocationConstraint
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateBucketConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbcLocationConstraint'
createBucketConfiguration
    :: CreateBucketConfiguration
createBucketConfiguration =
    CreateBucketConfiguration'
    { _cbcLocationConstraint = Nothing
    }

-- | Specifies the region where the bucket will be created. If you don\'t
-- specify a region, the bucket will be created in US Standard.
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe LocationConstraint)
cbcLocationConstraint = lens _cbcLocationConstraint (\ s a -> s{_cbcLocationConstraint = a});

instance Hashable CreateBucketConfiguration

instance NFData CreateBucketConfiguration

instance ToXML CreateBucketConfiguration where
        toXML CreateBucketConfiguration'{..}
          = mconcat
              ["LocationConstraint" @= _cbcLocationConstraint]

-- | /See:/ 'delete'' smart constructor.
data Delete = Delete'
    { _dQuiet   :: !(Maybe Bool)
    , _dObjects :: ![ObjectIdentifier]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Delete' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dQuiet'
--
-- * 'dObjects'
delete'
    :: Delete
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

instance Hashable Delete

instance NFData Delete

instance ToXML Delete where
        toXML Delete'{..}
          = mconcat
              ["Quiet" @= _dQuiet, toXMLList "Object" _dObjects]

-- | /See:/ 'deleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
    { _dmeVersionId    :: !(Maybe ObjectVersionId)
    , _dmeIsLatest     :: !(Maybe Bool)
    , _dmeOwner        :: !(Maybe Owner)
    , _dmeKey          :: !(Maybe ObjectKey)
    , _dmeLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMarkerEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
deleteMarkerEntry
    :: DeleteMarkerEntry
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

instance Hashable DeleteMarkerEntry

instance NFData DeleteMarkerEntry

-- | /See:/ 'deletedObject' smart constructor.
data DeletedObject = DeletedObject'
    { _dVersionId             :: !(Maybe ObjectVersionId)
    , _dDeleteMarker          :: !(Maybe Bool)
    , _dDeleteMarkerVersionId :: !(Maybe Text)
    , _dKey                   :: !(Maybe ObjectKey)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletedObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVersionId'
--
-- * 'dDeleteMarker'
--
-- * 'dDeleteMarkerVersionId'
--
-- * 'dKey'
deletedObject
    :: DeletedObject
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

instance Hashable DeletedObject

instance NFData DeletedObject

-- | /See:/ 'destination' smart constructor.
data Destination = Destination'
    { _dStorageClass :: !(Maybe StorageClass)
    , _dBucket       :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStorageClass'
--
-- * 'dBucket'
destination
    :: BucketName -- ^ 'dBucket'
    -> Destination
destination pBucket_ =
    Destination'
    { _dStorageClass = Nothing
    , _dBucket = pBucket_
    }

-- | The class of storage used to store the object.
dStorageClass :: Lens' Destination (Maybe StorageClass)
dStorageClass = lens _dStorageClass (\ s a -> s{_dStorageClass = a});

-- | Amazon resource name (ARN) of the bucket where you want Amazon S3 to
-- store replicas of the object identified by the rule.
dBucket :: Lens' Destination BucketName
dBucket = lens _dBucket (\ s a -> s{_dBucket = a});

instance FromXML Destination where
        parseXML x
          = Destination' <$>
              (x .@? "StorageClass") <*> (x .@ "Bucket")

instance Hashable Destination

instance NFData Destination

instance ToXML Destination where
        toXML Destination'{..}
          = mconcat
              ["StorageClass" @= _dStorageClass,
               "Bucket" @= _dBucket]

-- | /See:/ 'errorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument'
    { _edKey :: ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ErrorDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edKey'
errorDocument
    :: ObjectKey -- ^ 'edKey'
    -> ErrorDocument
errorDocument pKey_ =
    ErrorDocument'
    { _edKey = pKey_
    }

-- | The object key name to use when a 4XX class error occurs.
edKey :: Lens' ErrorDocument ObjectKey
edKey = lens _edKey (\ s a -> s{_edKey = a});

instance FromXML ErrorDocument where
        parseXML x = ErrorDocument' <$> (x .@ "Key")

instance Hashable ErrorDocument

instance NFData ErrorDocument

instance ToXML ErrorDocument where
        toXML ErrorDocument'{..} = mconcat ["Key" @= _edKey]

-- | Container for key value pair that defines the criteria for the filter
-- rule.
--
-- /See:/ 'filterRule' smart constructor.
data FilterRule = FilterRule'
    { _frValue :: !(Maybe Text)
    , _frName  :: !(Maybe FilterRuleName)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FilterRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frValue'
--
-- * 'frName'
filterRule
    :: FilterRule
filterRule =
    FilterRule'
    { _frValue = Nothing
    , _frName = Nothing
    }

-- | Undocumented member.
frValue :: Lens' FilterRule (Maybe Text)
frValue = lens _frValue (\ s a -> s{_frValue = a});

-- | Object key name prefix or suffix identifying one or more objects to
-- which the filtering rule applies. Maximum prefix length can be up to
-- 1,024 characters. Overlapping prefixes and suffixes are not supported.
-- For more information, go to
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the Amazon Simple Storage Service Developer Guide.
frName :: Lens' FilterRule (Maybe FilterRuleName)
frName = lens _frName (\ s a -> s{_frName = a});

instance FromXML FilterRule where
        parseXML x
          = FilterRule' <$> (x .@? "Value") <*> (x .@? "Name")

instance Hashable FilterRule

instance NFData FilterRule

instance ToXML FilterRule where
        toXML FilterRule'{..}
          = mconcat ["Value" @= _frValue, "Name" @= _frName]

-- | /See:/ 'grant' smart constructor.
data Grant = Grant'
    { _gPermission :: !(Maybe Permission)
    , _gGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPermission'
--
-- * 'gGrantee'
grant
    :: Grant
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

instance Hashable Grant

instance NFData Grant

instance ToXML Grant where
        toXML Grant'{..}
          = mconcat
              ["Permission" @= _gPermission,
               "Grantee" @= _gGrantee]

-- | /See:/ 'grantee' smart constructor.
data Grantee = Grantee'
    { _gURI          :: !(Maybe Text)
    , _gEmailAddress :: !(Maybe Text)
    , _gDisplayName  :: !(Maybe Text)
    , _gId           :: !(Maybe Text)
    , _gType         :: !Type
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Grantee' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gURI'
--
-- * 'gEmailAddress'
--
-- * 'gDisplayName'
--
-- * 'gId'
--
-- * 'gType'
grantee
    :: Type -- ^ 'gType'
    -> Grantee
grantee pType_ =
    Grantee'
    { _gURI = Nothing
    , _gEmailAddress = Nothing
    , _gDisplayName = Nothing
    , _gId = Nothing
    , _gType = pType_
    }

-- | URI of the grantee group.
gURI :: Lens' Grantee (Maybe Text)
gURI = lens _gURI (\ s a -> s{_gURI = a});

-- | Email address of the grantee.
gEmailAddress :: Lens' Grantee (Maybe Text)
gEmailAddress = lens _gEmailAddress (\ s a -> s{_gEmailAddress = a});

-- | Screen name of the grantee.
gDisplayName :: Lens' Grantee (Maybe Text)
gDisplayName = lens _gDisplayName (\ s a -> s{_gDisplayName = a});

-- | The canonical user ID of the grantee.
gId :: Lens' Grantee (Maybe Text)
gId = lens _gId (\ s a -> s{_gId = a});

-- | Type of grantee
gType :: Lens' Grantee Type
gType = lens _gType (\ s a -> s{_gType = a});

instance FromXML Grantee where
        parseXML x
          = Grantee' <$>
              (x .@? "URI") <*> (x .@? "EmailAddress") <*>
                (x .@? "DisplayName")
                <*> (x .@? "ID")
                <*> (x .@ "xsi:type")

instance Hashable Grantee

instance NFData Grantee

instance ToXML Grantee where
        toXML Grantee'{..}
          = mconcat
              ["URI" @= _gURI, "EmailAddress" @= _gEmailAddress,
               "DisplayName" @= _gDisplayName, "ID" @= _gId,
               "xsi:type" @= _gType]

-- | /See:/ 'indexDocument' smart constructor.
newtype IndexDocument = IndexDocument'
    { _idSuffix :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSuffix'
indexDocument
    :: Text -- ^ 'idSuffix'
    -> IndexDocument
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

instance Hashable IndexDocument

instance NFData IndexDocument

instance ToXML IndexDocument where
        toXML IndexDocument'{..}
          = mconcat ["Suffix" @= _idSuffix]

-- | /See:/ 'initiator' smart constructor.
data Initiator = Initiator'
    { _iDisplayName :: !(Maybe Text)
    , _iId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Initiator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iDisplayName'
--
-- * 'iId'
initiator
    :: Initiator
initiator =
    Initiator'
    { _iDisplayName = Nothing
    , _iId = Nothing
    }

-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\ s a -> s{_iDisplayName = a});

-- | If the principal is an AWS account, it provides the Canonical User ID.
-- If the principal is an IAM User, it provides a user ARN value.
iId :: Lens' Initiator (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a});

instance FromXML Initiator where
        parseXML x
          = Initiator' <$>
              (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Initiator

instance NFData Initiator

-- | Container for specifying the AWS Lambda notification configuration.
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
    { _lfcId                :: !(Maybe Text)
    , _lfcFilter            :: !(Maybe NotificationConfigurationFilter)
    , _lfcLambdaFunctionARN :: !Text
    , _lfcEvents            :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LambdaFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfcId'
--
-- * 'lfcFilter'
--
-- * 'lfcLambdaFunctionARN'
--
-- * 'lfcEvents'
lambdaFunctionConfiguration
    :: Text -- ^ 'lfcLambdaFunctionARN'
    -> LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN_ =
    LambdaFunctionConfiguration'
    { _lfcId = Nothing
    , _lfcFilter = Nothing
    , _lfcLambdaFunctionARN = pLambdaFunctionARN_
    , _lfcEvents = mempty
    }

-- | Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\ s a -> s{_lfcId = a});

-- | Undocumented member.
lfcFilter :: Lens' LambdaFunctionConfiguration (Maybe NotificationConfigurationFilter)
lfcFilter = lens _lfcFilter (\ s a -> s{_lfcFilter = a});

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
              (x .@? "Id") <*> (x .@? "Filter") <*>
                (x .@ "CloudFunction")
                <*> (parseXMLList "Event" x)

instance Hashable LambdaFunctionConfiguration

instance NFData LambdaFunctionConfiguration

instance ToXML LambdaFunctionConfiguration where
        toXML LambdaFunctionConfiguration'{..}
          = mconcat
              ["Id" @= _lfcId, "Filter" @= _lfcFilter,
               "CloudFunction" @= _lfcLambdaFunctionARN,
               toXMLList "Event" _lfcEvents]

-- | /See:/ 'lifecycleExpiration' smart constructor.
data LifecycleExpiration = LifecycleExpiration'
    { _leDays                      :: !(Maybe Int)
    , _leDate                      :: !(Maybe RFC822)
    , _leExpiredObjectDeleteMarker :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LifecycleExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leDays'
--
-- * 'leDate'
--
-- * 'leExpiredObjectDeleteMarker'
lifecycleExpiration
    :: LifecycleExpiration
lifecycleExpiration =
    LifecycleExpiration'
    { _leDays = Nothing
    , _leDate = Nothing
    , _leExpiredObjectDeleteMarker = Nothing
    }

-- | Indicates the lifetime, in days, of the objects that are subject to the
-- rule. The value must be a non-zero positive integer.
leDays :: Lens' LifecycleExpiration (Maybe Int)
leDays = lens _leDays (\ s a -> s{_leDays = a});

-- | Indicates at what date the object is to be moved or deleted. Should be
-- in GMT ISO 8601 Format.
leDate :: Lens' LifecycleExpiration (Maybe UTCTime)
leDate = lens _leDate (\ s a -> s{_leDate = a}) . mapping _Time;

-- | Indicates whether Amazon S3 will remove a delete marker with no
-- noncurrent versions. If set to true, the delete marker will be expired;
-- if set to false the policy takes no action. This cannot be specified
-- with Days or Date in a Lifecycle Expiration Policy.
leExpiredObjectDeleteMarker :: Lens' LifecycleExpiration (Maybe Bool)
leExpiredObjectDeleteMarker = lens _leExpiredObjectDeleteMarker (\ s a -> s{_leExpiredObjectDeleteMarker = a});

instance FromXML LifecycleExpiration where
        parseXML x
          = LifecycleExpiration' <$>
              (x .@? "Days") <*> (x .@? "Date") <*>
                (x .@? "ExpiredObjectDeleteMarker")

instance Hashable LifecycleExpiration

instance NFData LifecycleExpiration

instance ToXML LifecycleExpiration where
        toXML LifecycleExpiration'{..}
          = mconcat
              ["Days" @= _leDays, "Date" @= _leDate,
               "ExpiredObjectDeleteMarker" @=
                 _leExpiredObjectDeleteMarker]

-- | /See:/ 'lifecycleRule' smart constructor.
data LifecycleRule = LifecycleRule'
    { _lrTransitions                    :: !(Maybe [Transition])
    , _lrNoncurrentVersionExpiration    :: !(Maybe NoncurrentVersionExpiration)
    , _lrNoncurrentVersionTransitions   :: !(Maybe [NoncurrentVersionTransition])
    , _lrExpiration                     :: !(Maybe LifecycleExpiration)
    , _lrId                             :: !(Maybe Text)
    , _lrAbortIncompleteMultipartUpload :: !(Maybe AbortIncompleteMultipartUpload)
    , _lrPrefix                         :: !Text
    , _lrStatus                         :: !ExpirationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LifecycleRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrTransitions'
--
-- * 'lrNoncurrentVersionExpiration'
--
-- * 'lrNoncurrentVersionTransitions'
--
-- * 'lrExpiration'
--
-- * 'lrId'
--
-- * 'lrAbortIncompleteMultipartUpload'
--
-- * 'lrPrefix'
--
-- * 'lrStatus'
lifecycleRule
    :: Text -- ^ 'lrPrefix'
    -> ExpirationStatus -- ^ 'lrStatus'
    -> LifecycleRule
lifecycleRule pPrefix_ pStatus_ =
    LifecycleRule'
    { _lrTransitions = Nothing
    , _lrNoncurrentVersionExpiration = Nothing
    , _lrNoncurrentVersionTransitions = Nothing
    , _lrExpiration = Nothing
    , _lrId = Nothing
    , _lrAbortIncompleteMultipartUpload = Nothing
    , _lrPrefix = pPrefix_
    , _lrStatus = pStatus_
    }

-- | Undocumented member.
lrTransitions :: Lens' LifecycleRule [Transition]
lrTransitions = lens _lrTransitions (\ s a -> s{_lrTransitions = a}) . _Default . _Coerce;

-- | Undocumented member.
lrNoncurrentVersionExpiration :: Lens' LifecycleRule (Maybe NoncurrentVersionExpiration)
lrNoncurrentVersionExpiration = lens _lrNoncurrentVersionExpiration (\ s a -> s{_lrNoncurrentVersionExpiration = a});

-- | Undocumented member.
lrNoncurrentVersionTransitions :: Lens' LifecycleRule [NoncurrentVersionTransition]
lrNoncurrentVersionTransitions = lens _lrNoncurrentVersionTransitions (\ s a -> s{_lrNoncurrentVersionTransitions = a}) . _Default . _Coerce;

-- | Undocumented member.
lrExpiration :: Lens' LifecycleRule (Maybe LifecycleExpiration)
lrExpiration = lens _lrExpiration (\ s a -> s{_lrExpiration = a});

-- | Unique identifier for the rule. The value cannot be longer than 255
-- characters.
lrId :: Lens' LifecycleRule (Maybe Text)
lrId = lens _lrId (\ s a -> s{_lrId = a});

-- | Undocumented member.
lrAbortIncompleteMultipartUpload :: Lens' LifecycleRule (Maybe AbortIncompleteMultipartUpload)
lrAbortIncompleteMultipartUpload = lens _lrAbortIncompleteMultipartUpload (\ s a -> s{_lrAbortIncompleteMultipartUpload = a});

-- | Prefix identifying one or more objects to which the rule applies.
lrPrefix :: Lens' LifecycleRule Text
lrPrefix = lens _lrPrefix (\ s a -> s{_lrPrefix = a});

-- | If \'Enabled\', the rule is currently being applied. If \'Disabled\',
-- the rule is not currently being applied.
lrStatus :: Lens' LifecycleRule ExpirationStatus
lrStatus = lens _lrStatus (\ s a -> s{_lrStatus = a});

instance FromXML LifecycleRule where
        parseXML x
          = LifecycleRule' <$>
              (may (parseXMLList "Transition") x) <*>
                (x .@? "NoncurrentVersionExpiration")
                <*>
                (may (parseXMLList "NoncurrentVersionTransition") x)
                <*> (x .@? "Expiration")
                <*> (x .@? "ID")
                <*> (x .@? "AbortIncompleteMultipartUpload")
                <*> (x .@ "Prefix")
                <*> (x .@ "Status")

instance Hashable LifecycleRule

instance NFData LifecycleRule

instance ToXML LifecycleRule where
        toXML LifecycleRule'{..}
          = mconcat
              [toXML (toXMLList "Transition" <$> _lrTransitions),
               "NoncurrentVersionExpiration" @=
                 _lrNoncurrentVersionExpiration,
               toXML
                 (toXMLList "NoncurrentVersionTransition" <$>
                    _lrNoncurrentVersionTransitions),
               "Expiration" @= _lrExpiration, "ID" @= _lrId,
               "AbortIncompleteMultipartUpload" @=
                 _lrAbortIncompleteMultipartUpload,
               "Prefix" @= _lrPrefix, "Status" @= _lrStatus]

-- | /See:/ 'loggingEnabled' smart constructor.
data LoggingEnabled = LoggingEnabled'
    { _leTargetBucket :: !(Maybe Text)
    , _leTargetGrants :: !(Maybe [TargetGrant])
    , _leTargetPrefix :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoggingEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leTargetBucket'
--
-- * 'leTargetGrants'
--
-- * 'leTargetPrefix'
loggingEnabled
    :: LoggingEnabled
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

instance Hashable LoggingEnabled

instance NFData LoggingEnabled

instance ToXML LoggingEnabled where
        toXML LoggingEnabled'{..}
          = mconcat
              ["TargetBucket" @= _leTargetBucket,
               "TargetGrants" @=
                 toXML (toXMLList "Grant" <$> _leTargetGrants),
               "TargetPrefix" @= _leTargetPrefix]

-- | /See:/ 'multipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
    { _muInitiated    :: !(Maybe RFC822)
    , _muInitiator    :: !(Maybe Initiator)
    , _muOwner        :: !(Maybe Owner)
    , _muKey          :: !(Maybe ObjectKey)
    , _muStorageClass :: !(Maybe StorageClass)
    , _muUploadId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
multipartUpload
    :: MultipartUpload
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

instance Hashable MultipartUpload

instance NFData MultipartUpload

-- | Specifies when noncurrent object versions expire. Upon expiration,
-- Amazon S3 permanently deletes the noncurrent object versions. You set
-- this lifecycle configuration action on a bucket that has versioning
-- enabled (or suspended) to request that Amazon S3 delete noncurrent
-- object versions at a specific period in the object\'s lifetime.
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
    { _nveNoncurrentDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NoncurrentVersionExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nveNoncurrentDays'
noncurrentVersionExpiration
    :: Int -- ^ 'nveNoncurrentDays'
    -> NoncurrentVersionExpiration
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

instance Hashable NoncurrentVersionExpiration

instance NFData NoncurrentVersionExpiration

instance ToXML NoncurrentVersionExpiration where
        toXML NoncurrentVersionExpiration'{..}
          = mconcat ["NoncurrentDays" @= _nveNoncurrentDays]

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the STANDARD_IA or GLACIER storage class. If your bucket
-- is versioning-enabled (or versioning is suspended), you can set this
-- action to request that Amazon S3 transition noncurrent object versions
-- to the STANDARD_IA or GLACIER storage class at a specific period in the
-- object\'s lifetime.
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
    { _nvtNoncurrentDays :: !Int
    , _nvtStorageClass   :: !TransitionStorageClass
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NoncurrentVersionTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nvtNoncurrentDays'
--
-- * 'nvtStorageClass'
noncurrentVersionTransition
    :: Int -- ^ 'nvtNoncurrentDays'
    -> TransitionStorageClass -- ^ 'nvtStorageClass'
    -> NoncurrentVersionTransition
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

instance Hashable NoncurrentVersionTransition

instance NFData NoncurrentVersionTransition

instance ToXML NoncurrentVersionTransition where
        toXML NoncurrentVersionTransition'{..}
          = mconcat
              ["NoncurrentDays" @= _nvtNoncurrentDays,
               "StorageClass" @= _nvtStorageClass]

-- | Container for specifying the notification configuration of the bucket.
-- If this element is empty, notifications are turned off on the bucket.
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
    { _ncQueueConfigurations          :: !(Maybe [QueueConfiguration])
    , _ncTopicConfigurations          :: !(Maybe [TopicConfiguration])
    , _ncLambdaFunctionConfigurations :: !(Maybe [LambdaFunctionConfiguration])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncQueueConfigurations'
--
-- * 'ncTopicConfigurations'
--
-- * 'ncLambdaFunctionConfigurations'
notificationConfiguration
    :: NotificationConfiguration
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

instance Hashable NotificationConfiguration

instance NFData NotificationConfiguration

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

-- | Container for object key name filtering rules. For information about key
-- name filtering, go to
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications>
-- in the Amazon Simple Storage Service Developer Guide.
--
-- /See:/ 'notificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
    { _ncfKey :: Maybe S3KeyFilter
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NotificationConfigurationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncfKey'
notificationConfigurationFilter
    :: NotificationConfigurationFilter
notificationConfigurationFilter =
    NotificationConfigurationFilter'
    { _ncfKey = Nothing
    }

-- | Undocumented member.
ncfKey :: Lens' NotificationConfigurationFilter (Maybe S3KeyFilter)
ncfKey = lens _ncfKey (\ s a -> s{_ncfKey = a});

instance FromXML NotificationConfigurationFilter
         where
        parseXML x
          = NotificationConfigurationFilter' <$>
              (x .@? "S3Key")

instance Hashable NotificationConfigurationFilter

instance NFData NotificationConfigurationFilter

instance ToXML NotificationConfigurationFilter where
        toXML NotificationConfigurationFilter'{..}
          = mconcat ["S3Key" @= _ncfKey]

-- | /See:/ 'object'' smart constructor.
data Object = Object'
    { _oOwner        :: !(Maybe Owner)
    , _oETag         :: !ETag
    , _oSize         :: !Int
    , _oKey          :: !ObjectKey
    , _oStorageClass :: !ObjectStorageClass
    , _oLastModified :: !RFC822
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Object' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
object'
    :: ETag -- ^ 'oETag'
    -> Int -- ^ 'oSize'
    -> ObjectKey -- ^ 'oKey'
    -> ObjectStorageClass -- ^ 'oStorageClass'
    -> UTCTime -- ^ 'oLastModified'
    -> Object
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

instance Hashable Object

instance NFData Object

-- | /See:/ 'objectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
    { _oiVersionId :: !(Maybe ObjectVersionId)
    , _oiKey       :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ObjectIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oiVersionId'
--
-- * 'oiKey'
objectIdentifier
    :: ObjectKey -- ^ 'oiKey'
    -> ObjectIdentifier
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

instance Hashable ObjectIdentifier

instance NFData ObjectIdentifier

instance ToXML ObjectIdentifier where
        toXML ObjectIdentifier'{..}
          = mconcat
              ["VersionId" @= _oiVersionId, "Key" @= _oiKey]

-- | /See:/ 'objectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
    { _ovETag         :: !(Maybe ETag)
    , _ovVersionId    :: !(Maybe ObjectVersionId)
    , _ovSize         :: !(Maybe Int)
    , _ovIsLatest     :: !(Maybe Bool)
    , _ovOwner        :: !(Maybe Owner)
    , _ovKey          :: !(Maybe ObjectKey)
    , _ovStorageClass :: !(Maybe ObjectVersionStorageClass)
    , _ovLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ObjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovETag'
--
-- * 'ovVersionId'
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
objectVersion
    :: ObjectVersion
objectVersion =
    ObjectVersion'
    { _ovETag = Nothing
    , _ovVersionId = Nothing
    , _ovSize = Nothing
    , _ovIsLatest = Nothing
    , _ovOwner = Nothing
    , _ovKey = Nothing
    , _ovStorageClass = Nothing
    , _ovLastModified = Nothing
    }

-- | Undocumented member.
ovETag :: Lens' ObjectVersion (Maybe ETag)
ovETag = lens _ovETag (\ s a -> s{_ovETag = a});

-- | Version ID of an object.
ovVersionId :: Lens' ObjectVersion (Maybe ObjectVersionId)
ovVersionId = lens _ovVersionId (\ s a -> s{_ovVersionId = a});

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
              (x .@? "ETag") <*> (x .@? "VersionId") <*>
                (x .@? "Size")
                <*> (x .@? "IsLatest")
                <*> (x .@? "Owner")
                <*> (x .@? "Key")
                <*> (x .@? "StorageClass")
                <*> (x .@? "LastModified")

instance Hashable ObjectVersion

instance NFData ObjectVersion

-- | /See:/ 'owner' smart constructor.
data Owner = Owner'
    { _oDisplayName :: !(Maybe Text)
    , _oId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Owner' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oDisplayName'
--
-- * 'oId'
owner
    :: Owner
owner =
    Owner'
    { _oDisplayName = Nothing
    , _oId = Nothing
    }

-- | Undocumented member.
oDisplayName :: Lens' Owner (Maybe Text)
oDisplayName = lens _oDisplayName (\ s a -> s{_oDisplayName = a});

-- | Undocumented member.
oId :: Lens' Owner (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a});

instance FromXML Owner where
        parseXML x
          = Owner' <$> (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Owner

instance NFData Owner

instance ToXML Owner where
        toXML Owner'{..}
          = mconcat
              ["DisplayName" @= _oDisplayName, "ID" @= _oId]

-- | /See:/ 'part' smart constructor.
data Part = Part'
    { _pETag         :: !(Maybe ETag)
    , _pSize         :: !(Maybe Int)
    , _pPartNumber   :: !(Maybe Int)
    , _pLastModified :: !(Maybe RFC822)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Part' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pETag'
--
-- * 'pSize'
--
-- * 'pPartNumber'
--
-- * 'pLastModified'
part
    :: Part
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

instance Hashable Part

instance NFData Part

-- | Container for specifying an configuration when you want Amazon S3 to
-- publish events to an Amazon Simple Queue Service (Amazon SQS) queue.
--
-- /See:/ 'queueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
    { _qcId       :: !(Maybe Text)
    , _qcFilter   :: !(Maybe NotificationConfigurationFilter)
    , _qcQueueARN :: !Text
    , _qcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'QueueConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qcId'
--
-- * 'qcFilter'
--
-- * 'qcQueueARN'
--
-- * 'qcEvents'
queueConfiguration
    :: Text -- ^ 'qcQueueARN'
    -> QueueConfiguration
queueConfiguration pQueueARN_ =
    QueueConfiguration'
    { _qcId = Nothing
    , _qcFilter = Nothing
    , _qcQueueARN = pQueueARN_
    , _qcEvents = mempty
    }

-- | Undocumented member.
qcId :: Lens' QueueConfiguration (Maybe Text)
qcId = lens _qcId (\ s a -> s{_qcId = a});

-- | Undocumented member.
qcFilter :: Lens' QueueConfiguration (Maybe NotificationConfigurationFilter)
qcFilter = lens _qcFilter (\ s a -> s{_qcFilter = a});

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
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Queue")
                <*> (parseXMLList "Event" x)

instance Hashable QueueConfiguration

instance NFData QueueConfiguration

instance ToXML QueueConfiguration where
        toXML QueueConfiguration'{..}
          = mconcat
              ["Id" @= _qcId, "Filter" @= _qcFilter,
               "Queue" @= _qcQueueARN, toXMLList "Event" _qcEvents]

-- | /See:/ 'redirect' smart constructor.
data Redirect = Redirect'
    { _rHostName             :: !(Maybe Text)
    , _rProtocol             :: !(Maybe Protocol)
    , _rHTTPRedirectCode     :: !(Maybe Text)
    , _rReplaceKeyWith       :: !(Maybe Text)
    , _rReplaceKeyPrefixWith :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Redirect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
redirect
    :: Redirect
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

instance Hashable Redirect

instance NFData Redirect

instance ToXML Redirect where
        toXML Redirect'{..}
          = mconcat
              ["HostName" @= _rHostName, "Protocol" @= _rProtocol,
               "HttpRedirectCode" @= _rHTTPRedirectCode,
               "ReplaceKeyWith" @= _rReplaceKeyWith,
               "ReplaceKeyPrefixWith" @= _rReplaceKeyPrefixWith]

-- | /See:/ 'redirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
    { _rartProtocol :: !(Maybe Protocol)
    , _rartHostName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RedirectAllRequestsTo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rartProtocol'
--
-- * 'rartHostName'
redirectAllRequestsTo
    :: Text -- ^ 'rartHostName'
    -> RedirectAllRequestsTo
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

instance Hashable RedirectAllRequestsTo

instance NFData RedirectAllRequestsTo

instance ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo'{..}
          = mconcat
              ["Protocol" @= _rartProtocol,
               "HostName" @= _rartHostName]

-- | Container for replication rules. You can add as many as 1,000 rules.
-- Total replication configuration size can be up to 2 MB.
--
-- /See:/ 'replicationConfiguration' smart constructor.
data ReplicationConfiguration = ReplicationConfiguration'
    { _rcRole  :: !Text
    , _rcRules :: ![ReplicationRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRole'
--
-- * 'rcRules'
replicationConfiguration
    :: Text -- ^ 'rcRole'
    -> ReplicationConfiguration
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

instance Hashable ReplicationConfiguration

instance NFData ReplicationConfiguration

instance ToXML ReplicationConfiguration where
        toXML ReplicationConfiguration'{..}
          = mconcat
              ["Role" @= _rcRole, toXMLList "Rule" _rcRules]

-- | /See:/ 'replicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
    { _rrId          :: !(Maybe Text)
    , _rrPrefix      :: !Text
    , _rrStatus      :: !ReplicationRuleStatus
    , _rrDestination :: !Destination
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplicationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrId'
--
-- * 'rrPrefix'
--
-- * 'rrStatus'
--
-- * 'rrDestination'
replicationRule
    :: Text -- ^ 'rrPrefix'
    -> ReplicationRuleStatus -- ^ 'rrStatus'
    -> Destination -- ^ 'rrDestination'
    -> ReplicationRule
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

instance Hashable ReplicationRule

instance NFData ReplicationRule

instance ToXML ReplicationRule where
        toXML ReplicationRule'{..}
          = mconcat
              ["ID" @= _rrId, "Prefix" @= _rrPrefix,
               "Status" @= _rrStatus,
               "Destination" @= _rrDestination]

-- | /See:/ 'requestPaymentConfiguration' smart constructor.
newtype RequestPaymentConfiguration = RequestPaymentConfiguration'
    { _rpcPayer :: Payer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RequestPaymentConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcPayer'
requestPaymentConfiguration
    :: Payer -- ^ 'rpcPayer'
    -> RequestPaymentConfiguration
requestPaymentConfiguration pPayer_ =
    RequestPaymentConfiguration'
    { _rpcPayer = pPayer_
    }

-- | Specifies who pays for the download and request fees.
rpcPayer :: Lens' RequestPaymentConfiguration Payer
rpcPayer = lens _rpcPayer (\ s a -> s{_rpcPayer = a});

instance Hashable RequestPaymentConfiguration

instance NFData RequestPaymentConfiguration

instance ToXML RequestPaymentConfiguration where
        toXML RequestPaymentConfiguration'{..}
          = mconcat ["Payer" @= _rpcPayer]

-- | /See:/ 'restoreRequest' smart constructor.
newtype RestoreRequest = RestoreRequest'
    { _rrDays :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestoreRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDays'
restoreRequest
    :: Int -- ^ 'rrDays'
    -> RestoreRequest
restoreRequest pDays_ =
    RestoreRequest'
    { _rrDays = pDays_
    }

-- | Lifetime of the active copy in days
rrDays :: Lens' RestoreRequest Int
rrDays = lens _rrDays (\ s a -> s{_rrDays = a});

instance Hashable RestoreRequest

instance NFData RestoreRequest

instance ToXML RestoreRequest where
        toXML RestoreRequest'{..}
          = mconcat ["Days" @= _rrDays]

-- | /See:/ 'routingRule' smart constructor.
data RoutingRule = RoutingRule'
    { _rrCondition :: !(Maybe Condition)
    , _rrRedirect  :: !Redirect
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RoutingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrCondition'
--
-- * 'rrRedirect'
routingRule
    :: Redirect -- ^ 'rrRedirect'
    -> RoutingRule
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

instance Hashable RoutingRule

instance NFData RoutingRule

instance ToXML RoutingRule where
        toXML RoutingRule'{..}
          = mconcat
              ["Condition" @= _rrCondition,
               "Redirect" @= _rrRedirect]

-- | Container for object key name prefix and suffix filtering rules.
--
-- /See:/ 's3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
    { _skfFilterRules :: Maybe [FilterRule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3KeyFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skfFilterRules'
s3KeyFilter
    :: S3KeyFilter
s3KeyFilter =
    S3KeyFilter'
    { _skfFilterRules = Nothing
    }

-- | Undocumented member.
skfFilterRules :: Lens' S3KeyFilter [FilterRule]
skfFilterRules = lens _skfFilterRules (\ s a -> s{_skfFilterRules = a}) . _Default . _Coerce;

instance FromXML S3KeyFilter where
        parseXML x
          = S3KeyFilter' <$>
              (may (parseXMLList "FilterRule") x)

instance Hashable S3KeyFilter

instance NFData S3KeyFilter

instance ToXML S3KeyFilter where
        toXML S3KeyFilter'{..}
          = mconcat
              [toXML (toXMLList "FilterRule" <$> _skfFilterRules)]

-- | /See:/ 's3ServiceError' smart constructor.
data S3ServiceError = S3ServiceError'
    { _sseVersionId :: !(Maybe ObjectVersionId)
    , _sseKey       :: !(Maybe ObjectKey)
    , _sseCode      :: !(Maybe Text)
    , _sseMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3ServiceError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseVersionId'
--
-- * 'sseKey'
--
-- * 'sseCode'
--
-- * 'sseMessage'
s3ServiceError
    :: S3ServiceError
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

instance Hashable S3ServiceError

instance NFData S3ServiceError

-- | /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey   :: !ObjectKey
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey'
--
-- * 'tagValue'
tag
    :: ObjectKey -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
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

instance Hashable Tag

instance NFData Tag

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Key" @= _tagKey, "Value" @= _tagValue]

-- | /See:/ 'tagging' smart constructor.
newtype Tagging = Tagging'
    { _tTagSet :: [Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTagSet'
tagging
    :: Tagging
tagging =
    Tagging'
    { _tTagSet = mempty
    }

-- | Undocumented member.
tTagSet :: Lens' Tagging [Tag]
tTagSet = lens _tTagSet (\ s a -> s{_tTagSet = a}) . _Coerce;

instance Hashable Tagging

instance NFData Tagging

instance ToXML Tagging where
        toXML Tagging'{..}
          = mconcat ["TagSet" @= toXMLList "Tag" _tTagSet]

-- | /See:/ 'targetGrant' smart constructor.
data TargetGrant = TargetGrant'
    { _tgPermission :: !(Maybe BucketLogsPermission)
    , _tgGrantee    :: !(Maybe Grantee)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TargetGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgPermission'
--
-- * 'tgGrantee'
targetGrant
    :: TargetGrant
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

instance Hashable TargetGrant

instance NFData TargetGrant

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
data TopicConfiguration = TopicConfiguration'
    { _tcId       :: !(Maybe Text)
    , _tcFilter   :: !(Maybe NotificationConfigurationFilter)
    , _tcTopicARN :: !Text
    , _tcEvents   :: ![Event]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TopicConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcId'
--
-- * 'tcFilter'
--
-- * 'tcTopicARN'
--
-- * 'tcEvents'
topicConfiguration
    :: Text -- ^ 'tcTopicARN'
    -> TopicConfiguration
topicConfiguration pTopicARN_ =
    TopicConfiguration'
    { _tcId = Nothing
    , _tcFilter = Nothing
    , _tcTopicARN = pTopicARN_
    , _tcEvents = mempty
    }

-- | Undocumented member.
tcId :: Lens' TopicConfiguration (Maybe Text)
tcId = lens _tcId (\ s a -> s{_tcId = a});

-- | Undocumented member.
tcFilter :: Lens' TopicConfiguration (Maybe NotificationConfigurationFilter)
tcFilter = lens _tcFilter (\ s a -> s{_tcFilter = a});

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
              (x .@? "Id") <*> (x .@? "Filter") <*> (x .@ "Topic")
                <*> (parseXMLList "Event" x)

instance Hashable TopicConfiguration

instance NFData TopicConfiguration

instance ToXML TopicConfiguration where
        toXML TopicConfiguration'{..}
          = mconcat
              ["Id" @= _tcId, "Filter" @= _tcFilter,
               "Topic" @= _tcTopicARN, toXMLList "Event" _tcEvents]

-- | /See:/ 'transition' smart constructor.
data Transition = Transition'
    { _tDays         :: !(Maybe Int)
    , _tDate         :: !(Maybe RFC822)
    , _tStorageClass :: !(Maybe TransitionStorageClass)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Transition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDays'
--
-- * 'tDate'
--
-- * 'tStorageClass'
transition
    :: Transition
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

instance Hashable Transition

instance NFData Transition

instance ToXML Transition where
        toXML Transition'{..}
          = mconcat
              ["Days" @= _tDays, "Date" @= _tDate,
               "StorageClass" @= _tStorageClass]

-- | /See:/ 'versioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
    { _vcStatus    :: !(Maybe BucketVersioningStatus)
    , _vcMFADelete :: !(Maybe MFADelete)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VersioningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcStatus'
--
-- * 'vcMFADelete'
versioningConfiguration
    :: VersioningConfiguration
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

instance Hashable VersioningConfiguration

instance NFData VersioningConfiguration

instance ToXML VersioningConfiguration where
        toXML VersioningConfiguration'{..}
          = mconcat
              ["Status" @= _vcStatus, "MfaDelete" @= _vcMFADelete]

-- | /See:/ 'websiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
    { _wcRedirectAllRequestsTo :: !(Maybe RedirectAllRequestsTo)
    , _wcErrorDocument         :: !(Maybe ErrorDocument)
    , _wcIndexDocument         :: !(Maybe IndexDocument)
    , _wcRoutingRules          :: !(Maybe [RoutingRule])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WebsiteConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcRedirectAllRequestsTo'
--
-- * 'wcErrorDocument'
--
-- * 'wcIndexDocument'
--
-- * 'wcRoutingRules'
websiteConfiguration
    :: WebsiteConfiguration
websiteConfiguration =
    WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing
    , _wcErrorDocument = Nothing
    , _wcIndexDocument = Nothing
    , _wcRoutingRules = Nothing
    }

-- | Undocumented member.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\ s a -> s{_wcRedirectAllRequestsTo = a});

-- | Undocumented member.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\ s a -> s{_wcErrorDocument = a});

-- | Undocumented member.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\ s a -> s{_wcIndexDocument = a});

-- | Undocumented member.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\ s a -> s{_wcRoutingRules = a}) . _Default . _Coerce;

instance Hashable WebsiteConfiguration

instance NFData WebsiteConfiguration

instance ToXML WebsiteConfiguration where
        toXML WebsiteConfiguration'{..}
          = mconcat
              ["RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
               "ErrorDocument" @= _wcErrorDocument,
               "IndexDocument" @= _wcIndexDocument,
               "RoutingRules" @=
                 toXML (toXMLList "RoutingRule" <$> _wcRoutingRules)]
