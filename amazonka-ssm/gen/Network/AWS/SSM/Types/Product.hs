{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SSM.Types.Sum

-- | Describes an association of a configuration document and an instance.
--
-- /See:/ 'association' smart constructor.
data Association = Association'
    { _aInstanceId :: !(Maybe Text)
    , _aName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aInstanceId'
--
-- * 'aName'
association
    :: Association
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
data AssociationDescription = AssociationDescription'
    { _adInstanceId :: !(Maybe Text)
    , _adStatus     :: !(Maybe AssociationStatus)
    , _adDate       :: !(Maybe POSIX)
    , _adName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adInstanceId'
--
-- * 'adStatus'
--
-- * 'adDate'
--
-- * 'adName'
associationDescription
    :: AssociationDescription
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
data AssociationFilter = AssociationFilter'
    { _afKey   :: !AssociationFilterKey
    , _afValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afKey'
--
-- * 'afValue'
associationFilter
    :: AssociationFilterKey -- ^ 'afKey'
    -> Text -- ^ 'afValue'
    -> AssociationFilter
associationFilter pKey_ pValue_ =
    AssociationFilter'
    { _afKey = pKey_
    , _afValue = pValue_
    }

-- | The name of the filter.
afKey :: Lens' AssociationFilter AssociationFilterKey
afKey = lens _afKey (\ s a -> s{_afKey = a});

-- | The filter value.
afValue :: Lens' AssociationFilter Text
afValue = lens _afValue (\ s a -> s{_afValue = a});

instance ToJSON AssociationFilter where
        toJSON AssociationFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _afKey), Just ("value" .= _afValue)])

-- | Describes an association status.
--
-- /See:/ 'associationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
    { _asAdditionalInfo :: !(Maybe Text)
    , _asDate           :: !POSIX
    , _asName           :: !AssociationStatusName
    , _asMessage        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAdditionalInfo'
--
-- * 'asDate'
--
-- * 'asName'
--
-- * 'asMessage'
associationStatus
    :: UTCTime -- ^ 'asDate'
    -> AssociationStatusName -- ^ 'asName'
    -> Text -- ^ 'asMessage'
    -> AssociationStatus
associationStatus pDate_ pName_ pMessage_ =
    AssociationStatus'
    { _asAdditionalInfo = Nothing
    , _asDate = _Time # pDate_
    , _asName = pName_
    , _asMessage = pMessage_
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
              (catMaybes
                 [("AdditionalInfo" .=) <$> _asAdditionalInfo,
                  Just ("Date" .= _asDate), Just ("Name" .= _asName),
                  Just ("Message" .= _asMessage)])

-- | Describes the association of a configuration document and an instance.
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
    { _cabreInstanceId :: !(Maybe Text)
    , _cabreName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabreInstanceId'
--
-- * 'cabreName'
createAssociationBatchRequestEntry
    :: CreateAssociationBatchRequestEntry
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
              (catMaybes
                 [("InstanceId" .=) <$> _cabreInstanceId,
                  ("Name" .=) <$> _cabreName])

-- | Describes a configuration document.
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'
    { _dStatus      :: !(Maybe DocumentStatus)
    , _dSha1        :: !(Maybe Text)
    , _dCreatedDate :: !(Maybe POSIX)
    , _dName        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus'
--
-- * 'dSha1'
--
-- * 'dCreatedDate'
--
-- * 'dName'
documentDescription
    :: DocumentDescription
documentDescription =
    DocumentDescription'
    { _dStatus = Nothing
    , _dSha1 = Nothing
    , _dCreatedDate = Nothing
    , _dName = Nothing
    }

-- | The status of the configuration document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The SHA1 hash of the document, which you can use for verification
-- purposes.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a});

-- | The date when the configuration document was created.
dCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time;

-- | The name of the configuration document.
dName :: Lens' DocumentDescription (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

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
data DocumentFilter = DocumentFilter'
    { _dfKey   :: !DocumentFilterKey
    , _dfValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfKey'
--
-- * 'dfValue'
documentFilter
    :: DocumentFilterKey -- ^ 'dfKey'
    -> Text -- ^ 'dfValue'
    -> DocumentFilter
documentFilter pKey_ pValue_ =
    DocumentFilter'
    { _dfKey = pKey_
    , _dfValue = pValue_
    }

-- | The name of the filter.
dfKey :: Lens' DocumentFilter DocumentFilterKey
dfKey = lens _dfKey (\ s a -> s{_dfKey = a});

-- | The value of the filter.
dfValue :: Lens' DocumentFilter Text
dfValue = lens _dfValue (\ s a -> s{_dfValue = a});

instance ToJSON DocumentFilter where
        toJSON DocumentFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _dfKey), Just ("value" .= _dfValue)])

-- | Describes the name of a configuration document.
--
-- /See:/ 'documentIdentifier' smart constructor.
newtype DocumentIdentifier = DocumentIdentifier'
    { _diName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DocumentIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diName'
documentIdentifier
    :: DocumentIdentifier
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
data FailedCreateAssociation = FailedCreateAssociation'
    { _fcaEntry   :: !(Maybe CreateAssociationBatchRequestEntry)
    , _fcaFault   :: !(Maybe Fault)
    , _fcaMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailedCreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcaEntry'
--
-- * 'fcaFault'
--
-- * 'fcaMessage'
failedCreateAssociation
    :: FailedCreateAssociation
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
