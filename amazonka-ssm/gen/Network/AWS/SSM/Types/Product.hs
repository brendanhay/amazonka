{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SSM.Types.Sum

-- | Describes an association of a configuration document and an instance.
--
-- /See:/ 'association' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aInstanceId'
--
-- * 'aName'
data Association = Association'
    { _aInstanceId :: !(Maybe Text)
    , _aName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Association' smart constructor.
association :: Association
association =
    Association'
    { _aInstanceId = Nothing
    , _aName = Nothing
    }

-- | The ID of the instance.
aInstanceId :: Lens' Association (Maybe Text)
aInstanceId = lens _aInstanceId (\ s a -> s{_aInstanceId = a});

-- | The name of the configuration document.
aName :: Lens' Association (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

instance FromJSON Association where
        parseJSON
          = withObject "Association"
              (\ x ->
                 Association' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name"))

-- | Describes an association.
--
-- /See:/ 'associationDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adInstanceId'
--
-- * 'adStatus'
--
-- * 'adDate'
--
-- * 'adName'
data AssociationDescription = AssociationDescription'
    { _adInstanceId :: !(Maybe Text)
    , _adStatus     :: !(Maybe AssociationStatus)
    , _adDate       :: !(Maybe POSIX)
    , _adName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociationDescription' smart constructor.
associationDescription :: AssociationDescription
associationDescription =
    AssociationDescription'
    { _adInstanceId = Nothing
    , _adStatus = Nothing
    , _adDate = Nothing
    , _adName = Nothing
    }

-- | The ID of the instance.
adInstanceId :: Lens' AssociationDescription (Maybe Text)
adInstanceId = lens _adInstanceId (\ s a -> s{_adInstanceId = a});

-- | The association status.
adStatus :: Lens' AssociationDescription (Maybe AssociationStatus)
adStatus = lens _adStatus (\ s a -> s{_adStatus = a});

-- | The date when the association was made.
adDate :: Lens' AssociationDescription (Maybe UTCTime)
adDate = lens _adDate (\ s a -> s{_adDate = a}) . mapping _Time;

-- | The name of the configuration document.
adName :: Lens' AssociationDescription (Maybe Text)
adName = lens _adName (\ s a -> s{_adName = a});

instance FromJSON AssociationDescription where
        parseJSON
          = withObject "AssociationDescription"
              (\ x ->
                 AssociationDescription' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Date")
                     <*> (x .:? "Name"))

-- | Describes a filter.
--
-- /See:/ 'associationFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'afKey'
--
-- * 'afValue'
data AssociationFilter = AssociationFilter'
    { _afKey   :: !AssociationFilterKey
    , _afValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociationFilter' smart constructor.
associationFilter :: AssociationFilterKey -> Text -> AssociationFilter
associationFilter pKey pValue =
    AssociationFilter'
    { _afKey = pKey
    , _afValue = pValue
    }

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\ s a -> s{_afKey = a});

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\ s a -> s{_afValue = a});

instance ToJSON AssociationFilter where
        toJSON AssociationFilter'{..}
          = object ["key" .= _afKey, "value" .= _afValue]

-- | Describes an association status.
--
-- /See:/ 'associationStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asAdditionalInfo'
--
-- * 'asDate'
--
-- * 'asName'
--
-- * 'asMessage'
data AssociationStatus = AssociationStatus'
    { _asAdditionalInfo :: !(Maybe Text)
    , _asDate           :: !POSIX
    , _asName           :: !AssociationStatusName
    , _asMessage        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociationStatus' smart constructor.
associationStatus :: UTCTime -> AssociationStatusName -> Text -> AssociationStatus
associationStatus pDate pName pMessage =
    AssociationStatus'
    { _asAdditionalInfo = Nothing
    , _asDate = _Time # pDate
    , _asName = pName
    , _asMessage = pMessage
    }

-- | A user-defined string.
asAdditionalInfo :: Lens' AssociationStatus (Maybe Text)
asAdditionalInfo = lens _asAdditionalInfo (\ s a -> s{_asAdditionalInfo = a});

-- | The date when the status changed.
asDate :: Lens' AssociationStatus UTCTime
asDate = lens _asDate (\ s a -> s{_asDate = a}) . _Time;

-- | The status.
asName :: Lens' AssociationStatus AssociationStatusName
asName = lens _asName (\ s a -> s{_asName = a});

-- | The reason for the status.
asMessage :: Lens' AssociationStatus Text
asMessage = lens _asMessage (\ s a -> s{_asMessage = a});

instance FromJSON AssociationStatus where
        parseJSON
          = withObject "AssociationStatus"
              (\ x ->
                 AssociationStatus' <$>
                   (x .:? "AdditionalInfo") <*> (x .: "Date") <*>
                     (x .: "Name")
                     <*> (x .: "Message"))

instance ToJSON AssociationStatus where
        toJSON AssociationStatus'{..}
          = object
              ["AdditionalInfo" .= _asAdditionalInfo,
               "Date" .= _asDate, "Name" .= _asName,
               "Message" .= _asMessage]

-- | Describes the association of a configuration document and an instance.
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cabreInstanceId'
--
-- * 'cabreName'
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
    { _cabreInstanceId :: !(Maybe Text)
    , _cabreName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAssociationBatchRequestEntry' smart constructor.
createAssociationBatchRequestEntry :: CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry =
    CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing
    , _cabreName = Nothing
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\ s a -> s{_cabreInstanceId = a});

-- | The name of the configuration document.
cabreName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreName = lens _cabreName (\ s a -> s{_cabreName = a});

instance FromJSON CreateAssociationBatchRequestEntry
         where
        parseJSON
          = withObject "CreateAssociationBatchRequestEntry"
              (\ x ->
                 CreateAssociationBatchRequestEntry' <$>
                   (x .:? "InstanceId") <*> (x .:? "Name"))

instance ToJSON CreateAssociationBatchRequestEntry
         where
        toJSON CreateAssociationBatchRequestEntry'{..}
          = object
              ["InstanceId" .= _cabreInstanceId,
               "Name" .= _cabreName]

-- | Describes a configuration document.
--
-- /See:/ 'documentDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddStatus'
--
-- * 'ddSha1'
--
-- * 'ddCreatedDate'
--
-- * 'ddName'
data DocumentDescription = DocumentDescription'
    { _ddStatus      :: !(Maybe DocumentStatus)
    , _ddSha1        :: !(Maybe Text)
    , _ddCreatedDate :: !(Maybe POSIX)
    , _ddName        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DocumentDescription' smart constructor.
documentDescription :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _ddStatus = Nothing
    , _ddSha1 = Nothing
    , _ddCreatedDate = Nothing
    , _ddName = Nothing
    }

-- | The status of the configuration document.
ddStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
ddStatus = lens _ddStatus (\ s a -> s{_ddStatus = a});

-- | The SHA1 hash of the document, which you can use for verification
-- purposes.
ddSha1 :: Lens' DocumentDescription (Maybe Text)
ddSha1 = lens _ddSha1 (\ s a -> s{_ddSha1 = a});

-- | The date when the configuration document was created.
ddCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
ddCreatedDate = lens _ddCreatedDate (\ s a -> s{_ddCreatedDate = a}) . mapping _Time;

-- | The name of the configuration document.
ddName :: Lens' DocumentDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a});

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "Sha1") <*>
                     (x .:? "CreatedDate")
                     <*> (x .:? "Name"))

-- | Describes a filter.
--
-- /See:/ 'documentFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dfKey'
--
-- * 'dfValue'
data DocumentFilter = DocumentFilter'
    { _dfKey   :: !DocumentFilterKey
    , _dfValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DocumentFilter' smart constructor.
documentFilter :: DocumentFilterKey -> Text -> DocumentFilter
documentFilter pKey pValue =
    DocumentFilter'
    { _dfKey = pKey
    , _dfValue = pValue
    }

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\ s a -> s{_dfKey = a});

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object ["key" .= _dfKey, "value" .= _dfValue]

-- | Describes the name of a configuration document.
--
-- /See:/ 'documentIdentifier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diName'
newtype DocumentIdentifier = DocumentIdentifier'
    { _diName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DocumentIdentifier' smart constructor.
documentIdentifier :: DocumentIdentifier
documentIdentifier =
    DocumentIdentifier'
    { _diName = Nothing
    }

-- | The name of the configuration document.
diName :: Lens' DocumentIdentifier (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a});

instance FromJSON DocumentIdentifier where
        parseJSON
          = withObject "DocumentIdentifier"
              (\ x -> DocumentIdentifier' <$> (x .:? "Name"))

-- | Describes a failed association.
--
-- /See:/ 'failedCreateAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcaEntry'
--
-- * 'fcaFault'
--
-- * 'fcaMessage'
data FailedCreateAssociation = FailedCreateAssociation'
    { _fcaEntry   :: !(Maybe CreateAssociationBatchRequestEntry)
    , _fcaFault   :: !(Maybe Fault)
    , _fcaMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'FailedCreateAssociation' smart constructor.
failedCreateAssociation :: FailedCreateAssociation
failedCreateAssociation =
    FailedCreateAssociation'
    { _fcaEntry = Nothing
    , _fcaFault = Nothing
    , _fcaMessage = Nothing
    }

-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\ s a -> s{_fcaEntry = a});

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\ s a -> s{_fcaFault = a});

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\ s a -> s{_fcaMessage = a});

instance FromJSON FailedCreateAssociation where
        parseJSON
          = withObject "FailedCreateAssociation"
              (\ x ->
                 FailedCreateAssociation' <$>
                   (x .:? "Entry") <*> (x .:? "Fault") <*>
                     (x .:? "Message"))
