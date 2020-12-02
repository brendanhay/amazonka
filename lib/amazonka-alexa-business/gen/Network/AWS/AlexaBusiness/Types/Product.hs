{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Product where

import Network.AWS.AlexaBusiness.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An address book with attributes.
--
--
--
-- /See:/ 'addressBook' smart constructor.
data AddressBook = AddressBook'
  { _abAddressBookARN :: !(Maybe Text)
  , _abName           :: !(Maybe Text)
  , _abDescription    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAddressBookARN' - The ARN of the address book.
--
-- * 'abName' - The name of the address book.
--
-- * 'abDescription' - The description of the address book.
addressBook
    :: AddressBook
addressBook =
  AddressBook'
    {_abAddressBookARN = Nothing, _abName = Nothing, _abDescription = Nothing}


-- | The ARN of the address book.
abAddressBookARN :: Lens' AddressBook (Maybe Text)
abAddressBookARN = lens _abAddressBookARN (\ s a -> s{_abAddressBookARN = a})

-- | The name of the address book.
abName :: Lens' AddressBook (Maybe Text)
abName = lens _abName (\ s a -> s{_abName = a})

-- | The description of the address book.
abDescription :: Lens' AddressBook (Maybe Text)
abDescription = lens _abDescription (\ s a -> s{_abDescription = a})

instance FromJSON AddressBook where
        parseJSON
          = withObject "AddressBook"
              (\ x ->
                 AddressBook' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBook where

instance NFData AddressBook where

-- | Information related to an address book.
--
--
--
-- /See:/ 'addressBookData' smart constructor.
data AddressBookData = AddressBookData'
  { _abdAddressBookARN :: !(Maybe Text)
  , _abdName           :: !(Maybe Text)
  , _abdDescription    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressBookData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abdAddressBookARN' - The ARN of the address book.
--
-- * 'abdName' - The name of the address book.
--
-- * 'abdDescription' - The description of the address book.
addressBookData
    :: AddressBookData
addressBookData =
  AddressBookData'
    { _abdAddressBookARN = Nothing
    , _abdName = Nothing
    , _abdDescription = Nothing
    }


-- | The ARN of the address book.
abdAddressBookARN :: Lens' AddressBookData (Maybe Text)
abdAddressBookARN = lens _abdAddressBookARN (\ s a -> s{_abdAddressBookARN = a})

-- | The name of the address book.
abdName :: Lens' AddressBookData (Maybe Text)
abdName = lens _abdName (\ s a -> s{_abdName = a})

-- | The description of the address book.
abdDescription :: Lens' AddressBookData (Maybe Text)
abdDescription = lens _abdDescription (\ s a -> s{_abdDescription = a})

instance FromJSON AddressBookData where
        parseJSON
          = withObject "AddressBookData"
              (\ x ->
                 AddressBookData' <$>
                   (x .:? "AddressBookArn") <*> (x .:? "Name") <*>
                     (x .:? "Description"))

instance Hashable AddressBookData where

instance NFData AddressBookData where

-- | A contact with attributes.
--
--
--
-- /See:/ 'contact' smart constructor.
data Contact = Contact'
  { _cLastName    :: !(Maybe Text)
  , _cContactARN  :: !(Maybe Text)
  , _cPhoneNumber :: !(Maybe Text)
  , _cFirstName   :: !(Maybe Text)
  , _cDisplayName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Contact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cContactARN' - The ARN of the contact.
--
-- * 'cPhoneNumber' - The phone number of the contact.
--
-- * 'cFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cDisplayName' - The name of the contact to display on the console.
contact
    :: Contact
contact =
  Contact'
    { _cLastName = Nothing
    , _cContactARN = Nothing
    , _cPhoneNumber = Nothing
    , _cFirstName = Nothing
    , _cDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cLastName :: Lens' Contact (Maybe Text)
cLastName = lens _cLastName (\ s a -> s{_cLastName = a})

-- | The ARN of the contact.
cContactARN :: Lens' Contact (Maybe Text)
cContactARN = lens _cContactARN (\ s a -> s{_cContactARN = a})

-- | The phone number of the contact.
cPhoneNumber :: Lens' Contact (Maybe Text)
cPhoneNumber = lens _cPhoneNumber (\ s a -> s{_cPhoneNumber = a})

-- | The first name of the contact, used to call the contact on the device.
cFirstName :: Lens' Contact (Maybe Text)
cFirstName = lens _cFirstName (\ s a -> s{_cFirstName = a})

-- | The name of the contact to display on the console.
cDisplayName :: Lens' Contact (Maybe Text)
cDisplayName = lens _cDisplayName (\ s a -> s{_cDisplayName = a})

instance FromJSON Contact where
        parseJSON
          = withObject "Contact"
              (\ x ->
                 Contact' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable Contact where

instance NFData Contact where

-- | Information related to a contact.
--
--
--
-- /See:/ 'contactData' smart constructor.
data ContactData = ContactData'
  { _cdLastName    :: !(Maybe Text)
  , _cdContactARN  :: !(Maybe Text)
  , _cdPhoneNumber :: !(Maybe Text)
  , _cdFirstName   :: !(Maybe Text)
  , _cdDisplayName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContactData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cdContactARN' - The ARN of the contact.
--
-- * 'cdPhoneNumber' - The phone number of the contact.
--
-- * 'cdFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cdDisplayName' - The name of the contact to display on the console.
contactData
    :: ContactData
contactData =
  ContactData'
    { _cdLastName = Nothing
    , _cdContactARN = Nothing
    , _cdPhoneNumber = Nothing
    , _cdFirstName = Nothing
    , _cdDisplayName = Nothing
    }


-- | The last name of the contact, used to call the contact on the device.
cdLastName :: Lens' ContactData (Maybe Text)
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a})

-- | The ARN of the contact.
cdContactARN :: Lens' ContactData (Maybe Text)
cdContactARN = lens _cdContactARN (\ s a -> s{_cdContactARN = a})

-- | The phone number of the contact.
cdPhoneNumber :: Lens' ContactData (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\ s a -> s{_cdPhoneNumber = a})

-- | The first name of the contact, used to call the contact on the device.
cdFirstName :: Lens' ContactData (Maybe Text)
cdFirstName = lens _cdFirstName (\ s a -> s{_cdFirstName = a})

-- | The name of the contact to display on the console.
cdDisplayName :: Lens' ContactData (Maybe Text)
cdDisplayName = lens _cdDisplayName (\ s a -> s{_cdDisplayName = a})

instance FromJSON ContactData where
        parseJSON
          = withObject "ContactData"
              (\ x ->
                 ContactData' <$>
                   (x .:? "LastName") <*> (x .:? "ContactArn") <*>
                     (x .:? "PhoneNumber")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "DisplayName"))

instance Hashable ContactData where

instance NFData ContactData where

-- | A device with attributes.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dDeviceStatus       :: !(Maybe DeviceStatus)
  , _dDeviceStatusInfo   :: !(Maybe DeviceStatusInfo)
  , _dDeviceARN          :: !(Maybe Text)
  , _dMACAddress         :: !(Maybe Text)
  , _dDeviceName         :: !(Maybe Text)
  , _dRoomARN            :: !(Maybe Text)
  , _dSoftwareVersion    :: !(Maybe Text)
  , _dDeviceType         :: !(Maybe Text)
  , _dDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeviceStatus' - The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
--
-- * 'dDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'dDeviceARN' - The ARN of a device.
--
-- * 'dMACAddress' - The MAC address of a device.
--
-- * 'dDeviceName' - The name of a device.
--
-- * 'dRoomARN' - The room ARN of a device.
--
-- * 'dSoftwareVersion' - The software version of a device.
--
-- * 'dDeviceType' - The type of a device.
--
-- * 'dDeviceSerialNumber' - The serial number of a device.
device
    :: Device
device =
  Device'
    { _dDeviceStatus = Nothing
    , _dDeviceStatusInfo = Nothing
    , _dDeviceARN = Nothing
    , _dMACAddress = Nothing
    , _dDeviceName = Nothing
    , _dRoomARN = Nothing
    , _dSoftwareVersion = Nothing
    , _dDeviceType = Nothing
    , _dDeviceSerialNumber = Nothing
    }


-- | The status of a device. If the status is not READY, check the DeviceStatusInfo value for details.
dDeviceStatus :: Lens' Device (Maybe DeviceStatus)
dDeviceStatus = lens _dDeviceStatus (\ s a -> s{_dDeviceStatus = a})

-- | Detailed information about a device's status.
dDeviceStatusInfo :: Lens' Device (Maybe DeviceStatusInfo)
dDeviceStatusInfo = lens _dDeviceStatusInfo (\ s a -> s{_dDeviceStatusInfo = a})

-- | The ARN of a device.
dDeviceARN :: Lens' Device (Maybe Text)
dDeviceARN = lens _dDeviceARN (\ s a -> s{_dDeviceARN = a})

-- | The MAC address of a device.
dMACAddress :: Lens' Device (Maybe Text)
dMACAddress = lens _dMACAddress (\ s a -> s{_dMACAddress = a})

-- | The name of a device.
dDeviceName :: Lens' Device (Maybe Text)
dDeviceName = lens _dDeviceName (\ s a -> s{_dDeviceName = a})

-- | The room ARN of a device.
dRoomARN :: Lens' Device (Maybe Text)
dRoomARN = lens _dRoomARN (\ s a -> s{_dRoomARN = a})

-- | The software version of a device.
dSoftwareVersion :: Lens' Device (Maybe Text)
dSoftwareVersion = lens _dSoftwareVersion (\ s a -> s{_dSoftwareVersion = a})

-- | The type of a device.
dDeviceType :: Lens' Device (Maybe Text)
dDeviceType = lens _dDeviceType (\ s a -> s{_dDeviceType = a})

-- | The serial number of a device.
dDeviceSerialNumber :: Lens' Device (Maybe Text)
dDeviceSerialNumber = lens _dDeviceSerialNumber (\ s a -> s{_dDeviceSerialNumber = a})

instance FromJSON Device where
        parseJSON
          = withObject "Device"
              (\ x ->
                 Device' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable Device where

instance NFData Device where

-- | Device attributes.
--
--
--
-- /See:/ 'deviceData' smart constructor.
data DeviceData = DeviceData'
  { _ddDeviceStatus       :: !(Maybe DeviceStatus)
  , _ddDeviceStatusInfo   :: !(Maybe DeviceStatusInfo)
  , _ddDeviceARN          :: !(Maybe Text)
  , _ddMACAddress         :: !(Maybe Text)
  , _ddDeviceName         :: !(Maybe Text)
  , _ddRoomARN            :: !(Maybe Text)
  , _ddSoftwareVersion    :: !(Maybe Text)
  , _ddDeviceType         :: !(Maybe Text)
  , _ddRoomName           :: !(Maybe Text)
  , _ddDeviceSerialNumber :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDeviceStatus' - The status of a device.
--
-- * 'ddDeviceStatusInfo' - Detailed information about a device's status.
--
-- * 'ddDeviceARN' - The ARN of a device.
--
-- * 'ddMACAddress' - The MAC address of a device.
--
-- * 'ddDeviceName' - The name of a device.
--
-- * 'ddRoomARN' - The room ARN associated with a device.
--
-- * 'ddSoftwareVersion' - The software version of a device.
--
-- * 'ddDeviceType' - The type of a device.
--
-- * 'ddRoomName' - The name of the room associated with a device.
--
-- * 'ddDeviceSerialNumber' - The serial number of a device.
deviceData
    :: DeviceData
deviceData =
  DeviceData'
    { _ddDeviceStatus = Nothing
    , _ddDeviceStatusInfo = Nothing
    , _ddDeviceARN = Nothing
    , _ddMACAddress = Nothing
    , _ddDeviceName = Nothing
    , _ddRoomARN = Nothing
    , _ddSoftwareVersion = Nothing
    , _ddDeviceType = Nothing
    , _ddRoomName = Nothing
    , _ddDeviceSerialNumber = Nothing
    }


-- | The status of a device.
ddDeviceStatus :: Lens' DeviceData (Maybe DeviceStatus)
ddDeviceStatus = lens _ddDeviceStatus (\ s a -> s{_ddDeviceStatus = a})

-- | Detailed information about a device's status.
ddDeviceStatusInfo :: Lens' DeviceData (Maybe DeviceStatusInfo)
ddDeviceStatusInfo = lens _ddDeviceStatusInfo (\ s a -> s{_ddDeviceStatusInfo = a})

-- | The ARN of a device.
ddDeviceARN :: Lens' DeviceData (Maybe Text)
ddDeviceARN = lens _ddDeviceARN (\ s a -> s{_ddDeviceARN = a})

-- | The MAC address of a device.
ddMACAddress :: Lens' DeviceData (Maybe Text)
ddMACAddress = lens _ddMACAddress (\ s a -> s{_ddMACAddress = a})

-- | The name of a device.
ddDeviceName :: Lens' DeviceData (Maybe Text)
ddDeviceName = lens _ddDeviceName (\ s a -> s{_ddDeviceName = a})

-- | The room ARN associated with a device.
ddRoomARN :: Lens' DeviceData (Maybe Text)
ddRoomARN = lens _ddRoomARN (\ s a -> s{_ddRoomARN = a})

-- | The software version of a device.
ddSoftwareVersion :: Lens' DeviceData (Maybe Text)
ddSoftwareVersion = lens _ddSoftwareVersion (\ s a -> s{_ddSoftwareVersion = a})

-- | The type of a device.
ddDeviceType :: Lens' DeviceData (Maybe Text)
ddDeviceType = lens _ddDeviceType (\ s a -> s{_ddDeviceType = a})

-- | The name of the room associated with a device.
ddRoomName :: Lens' DeviceData (Maybe Text)
ddRoomName = lens _ddRoomName (\ s a -> s{_ddRoomName = a})

-- | The serial number of a device.
ddDeviceSerialNumber :: Lens' DeviceData (Maybe Text)
ddDeviceSerialNumber = lens _ddDeviceSerialNumber (\ s a -> s{_ddDeviceSerialNumber = a})

instance FromJSON DeviceData where
        parseJSON
          = withObject "DeviceData"
              (\ x ->
                 DeviceData' <$>
                   (x .:? "DeviceStatus") <*> (x .:? "DeviceStatusInfo")
                     <*> (x .:? "DeviceArn")
                     <*> (x .:? "MacAddress")
                     <*> (x .:? "DeviceName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "SoftwareVersion")
                     <*> (x .:? "DeviceType")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "DeviceSerialNumber"))

instance Hashable DeviceData where

instance NFData DeviceData where

-- | The list of device events.
--
--
--
-- /See:/ 'deviceEvent' smart constructor.
data DeviceEvent = DeviceEvent'
  { _deValue     :: !(Maybe Text)
  , _deType      :: !(Maybe DeviceEventType)
  , _deTimestamp :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deValue' - The value of the event.
--
-- * 'deType' - The type of device event.
--
-- * 'deTimestamp' - The time (in epoch) when the event occurred.
deviceEvent
    :: DeviceEvent
deviceEvent =
  DeviceEvent' {_deValue = Nothing, _deType = Nothing, _deTimestamp = Nothing}


-- | The value of the event.
deValue :: Lens' DeviceEvent (Maybe Text)
deValue = lens _deValue (\ s a -> s{_deValue = a})

-- | The type of device event.
deType :: Lens' DeviceEvent (Maybe DeviceEventType)
deType = lens _deType (\ s a -> s{_deType = a})

-- | The time (in epoch) when the event occurred.
deTimestamp :: Lens' DeviceEvent (Maybe UTCTime)
deTimestamp = lens _deTimestamp (\ s a -> s{_deTimestamp = a}) . mapping _Time

instance FromJSON DeviceEvent where
        parseJSON
          = withObject "DeviceEvent"
              (\ x ->
                 DeviceEvent' <$>
                   (x .:? "Value") <*> (x .:? "Type") <*>
                     (x .:? "Timestamp"))

instance Hashable DeviceEvent where

instance NFData DeviceEvent where

-- | Details of a device’s status.
--
--
--
-- /See:/ 'deviceStatusDetail' smart constructor.
newtype DeviceStatusDetail = DeviceStatusDetail'
  { _dsdCode :: Maybe DeviceStatusDetailCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceStatusDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdCode' - The device status detail code.
deviceStatusDetail
    :: DeviceStatusDetail
deviceStatusDetail = DeviceStatusDetail' {_dsdCode = Nothing}


-- | The device status detail code.
dsdCode :: Lens' DeviceStatusDetail (Maybe DeviceStatusDetailCode)
dsdCode = lens _dsdCode (\ s a -> s{_dsdCode = a})

instance FromJSON DeviceStatusDetail where
        parseJSON
          = withObject "DeviceStatusDetail"
              (\ x -> DeviceStatusDetail' <$> (x .:? "Code"))

instance Hashable DeviceStatusDetail where

instance NFData DeviceStatusDetail where

-- | Detailed information about a device's status.
--
--
--
-- /See:/ 'deviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { _dsiDeviceStatusDetails :: !(Maybe [DeviceStatusDetail])
  , _dsiConnectionStatus    :: !(Maybe ConnectionStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiDeviceStatusDetails' - One or more device status detail descriptions.
--
-- * 'dsiConnectionStatus' - The latest available information about the connection status of a device.
deviceStatusInfo
    :: DeviceStatusInfo
deviceStatusInfo =
  DeviceStatusInfo'
    {_dsiDeviceStatusDetails = Nothing, _dsiConnectionStatus = Nothing}


-- | One or more device status detail descriptions.
dsiDeviceStatusDetails :: Lens' DeviceStatusInfo [DeviceStatusDetail]
dsiDeviceStatusDetails = lens _dsiDeviceStatusDetails (\ s a -> s{_dsiDeviceStatusDetails = a}) . _Default . _Coerce

-- | The latest available information about the connection status of a device.
dsiConnectionStatus :: Lens' DeviceStatusInfo (Maybe ConnectionStatus)
dsiConnectionStatus = lens _dsiConnectionStatus (\ s a -> s{_dsiConnectionStatus = a})

instance FromJSON DeviceStatusInfo where
        parseJSON
          = withObject "DeviceStatusInfo"
              (\ x ->
                 DeviceStatusInfo' <$>
                   (x .:? "DeviceStatusDetails" .!= mempty) <*>
                     (x .:? "ConnectionStatus"))

instance Hashable DeviceStatusInfo where

instance NFData DeviceStatusInfo where

-- | A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fKey    :: !Text
  , _fValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKey' - The key of a filter.
--
-- * 'fValues' - The values of a filter.
filter'
    :: Text -- ^ 'fKey'
    -> Filter
filter' pKey_ = Filter' {_fKey = pKey_, _fValues = mempty}


-- | The key of a filter.
fKey :: Lens' Filter Text
fKey = lens _fKey (\ s a -> s{_fKey = a})

-- | The values of a filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _fKey), Just ("Values" .= _fValues)])

-- | A room profile with attributes.
--
--
--
-- /See:/ 'profile' smart constructor.
data Profile = Profile'
  { _pSetupModeDisabled :: !(Maybe Bool)
  , _pPSTNEnabled       :: !(Maybe Bool)
  , _pDistanceUnit      :: !(Maybe DistanceUnit)
  , _pAddress           :: !(Maybe Text)
  , _pProfileARN        :: !(Maybe Text)
  , _pWakeWord          :: !(Maybe WakeWord)
  , _pProfileName       :: !(Maybe Text)
  , _pTemperatureUnit   :: !(Maybe TemperatureUnit)
  , _pTimezone          :: !(Maybe Text)
  , _pMaxVolumeLimit    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Profile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSetupModeDisabled' - The setup mode of a room profile.
--
-- * 'pPSTNEnabled' - The PSTN setting of a room profile.
--
-- * 'pDistanceUnit' - The distance unit of a room profile.
--
-- * 'pAddress' - The address of a room profile.
--
-- * 'pProfileARN' - The ARN of a room profile.
--
-- * 'pWakeWord' - The wake word of a room profile.
--
-- * 'pProfileName' - The name of a room profile.
--
-- * 'pTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pTimezone' - The time zone of a room profile.
--
-- * 'pMaxVolumeLimit' - The max volume limit of a room profile.
profile
    :: Profile
profile =
  Profile'
    { _pSetupModeDisabled = Nothing
    , _pPSTNEnabled = Nothing
    , _pDistanceUnit = Nothing
    , _pAddress = Nothing
    , _pProfileARN = Nothing
    , _pWakeWord = Nothing
    , _pProfileName = Nothing
    , _pTemperatureUnit = Nothing
    , _pTimezone = Nothing
    , _pMaxVolumeLimit = Nothing
    }


-- | The setup mode of a room profile.
pSetupModeDisabled :: Lens' Profile (Maybe Bool)
pSetupModeDisabled = lens _pSetupModeDisabled (\ s a -> s{_pSetupModeDisabled = a})

-- | The PSTN setting of a room profile.
pPSTNEnabled :: Lens' Profile (Maybe Bool)
pPSTNEnabled = lens _pPSTNEnabled (\ s a -> s{_pPSTNEnabled = a})

-- | The distance unit of a room profile.
pDistanceUnit :: Lens' Profile (Maybe DistanceUnit)
pDistanceUnit = lens _pDistanceUnit (\ s a -> s{_pDistanceUnit = a})

-- | The address of a room profile.
pAddress :: Lens' Profile (Maybe Text)
pAddress = lens _pAddress (\ s a -> s{_pAddress = a})

-- | The ARN of a room profile.
pProfileARN :: Lens' Profile (Maybe Text)
pProfileARN = lens _pProfileARN (\ s a -> s{_pProfileARN = a})

-- | The wake word of a room profile.
pWakeWord :: Lens' Profile (Maybe WakeWord)
pWakeWord = lens _pWakeWord (\ s a -> s{_pWakeWord = a})

-- | The name of a room profile.
pProfileName :: Lens' Profile (Maybe Text)
pProfileName = lens _pProfileName (\ s a -> s{_pProfileName = a})

-- | The temperature unit of a room profile.
pTemperatureUnit :: Lens' Profile (Maybe TemperatureUnit)
pTemperatureUnit = lens _pTemperatureUnit (\ s a -> s{_pTemperatureUnit = a})

-- | The time zone of a room profile.
pTimezone :: Lens' Profile (Maybe Text)
pTimezone = lens _pTimezone (\ s a -> s{_pTimezone = a})

-- | The max volume limit of a room profile.
pMaxVolumeLimit :: Lens' Profile (Maybe Int)
pMaxVolumeLimit = lens _pMaxVolumeLimit (\ s a -> s{_pMaxVolumeLimit = a})

instance FromJSON Profile where
        parseJSON
          = withObject "Profile"
              (\ x ->
                 Profile' <$>
                   (x .:? "SetupModeDisabled") <*> (x .:? "PSTNEnabled")
                     <*> (x .:? "DistanceUnit")
                     <*> (x .:? "Address")
                     <*> (x .:? "ProfileArn")
                     <*> (x .:? "WakeWord")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "TemperatureUnit")
                     <*> (x .:? "Timezone")
                     <*> (x .:? "MaxVolumeLimit"))

instance Hashable Profile where

instance NFData Profile where

-- | The data of a room profile.
--
--
--
-- /See:/ 'profileData' smart constructor.
data ProfileData = ProfileData'
  { _pdDistanceUnit    :: !(Maybe DistanceUnit)
  , _pdAddress         :: !(Maybe Text)
  , _pdProfileARN      :: !(Maybe Text)
  , _pdWakeWord        :: !(Maybe WakeWord)
  , _pdProfileName     :: !(Maybe Text)
  , _pdTemperatureUnit :: !(Maybe TemperatureUnit)
  , _pdTimezone        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProfileData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDistanceUnit' - The distance unit of a room profile.
--
-- * 'pdAddress' - The address of a room profile.
--
-- * 'pdProfileARN' - The ARN of a room profile.
--
-- * 'pdWakeWord' - The wake word of a room profile.
--
-- * 'pdProfileName' - The name of a room profile.
--
-- * 'pdTemperatureUnit' - The temperature unit of a room profile.
--
-- * 'pdTimezone' - The timezone of a room profile.
profileData
    :: ProfileData
profileData =
  ProfileData'
    { _pdDistanceUnit = Nothing
    , _pdAddress = Nothing
    , _pdProfileARN = Nothing
    , _pdWakeWord = Nothing
    , _pdProfileName = Nothing
    , _pdTemperatureUnit = Nothing
    , _pdTimezone = Nothing
    }


-- | The distance unit of a room profile.
pdDistanceUnit :: Lens' ProfileData (Maybe DistanceUnit)
pdDistanceUnit = lens _pdDistanceUnit (\ s a -> s{_pdDistanceUnit = a})

-- | The address of a room profile.
pdAddress :: Lens' ProfileData (Maybe Text)
pdAddress = lens _pdAddress (\ s a -> s{_pdAddress = a})

-- | The ARN of a room profile.
pdProfileARN :: Lens' ProfileData (Maybe Text)
pdProfileARN = lens _pdProfileARN (\ s a -> s{_pdProfileARN = a})

-- | The wake word of a room profile.
pdWakeWord :: Lens' ProfileData (Maybe WakeWord)
pdWakeWord = lens _pdWakeWord (\ s a -> s{_pdWakeWord = a})

-- | The name of a room profile.
pdProfileName :: Lens' ProfileData (Maybe Text)
pdProfileName = lens _pdProfileName (\ s a -> s{_pdProfileName = a})

-- | The temperature unit of a room profile.
pdTemperatureUnit :: Lens' ProfileData (Maybe TemperatureUnit)
pdTemperatureUnit = lens _pdTemperatureUnit (\ s a -> s{_pdTemperatureUnit = a})

-- | The timezone of a room profile.
pdTimezone :: Lens' ProfileData (Maybe Text)
pdTimezone = lens _pdTimezone (\ s a -> s{_pdTimezone = a})

instance FromJSON ProfileData where
        parseJSON
          = withObject "ProfileData"
              (\ x ->
                 ProfileData' <$>
                   (x .:? "DistanceUnit") <*> (x .:? "Address") <*>
                     (x .:? "ProfileArn")
                     <*> (x .:? "WakeWord")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "TemperatureUnit")
                     <*> (x .:? "Timezone"))

instance Hashable ProfileData where

instance NFData ProfileData where

-- | A room with attributes.
--
--
--
-- /See:/ 'room' smart constructor.
data Room = Room'
  { _rProfileARN         :: !(Maybe Text)
  , _rProviderCalendarId :: !(Maybe Text)
  , _rRoomARN            :: !(Maybe Text)
  , _rRoomName           :: !(Maybe Text)
  , _rDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Room' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rProfileARN' - The profile ARN of a room.
--
-- * 'rProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rRoomARN' - The ARN of a room.
--
-- * 'rRoomName' - The name of a room.
--
-- * 'rDescription' - The description of a room.
room
    :: Room
room =
  Room'
    { _rProfileARN = Nothing
    , _rProviderCalendarId = Nothing
    , _rRoomARN = Nothing
    , _rRoomName = Nothing
    , _rDescription = Nothing
    }


-- | The profile ARN of a room.
rProfileARN :: Lens' Room (Maybe Text)
rProfileARN = lens _rProfileARN (\ s a -> s{_rProfileARN = a})

-- | The provider calendar ARN of a room.
rProviderCalendarId :: Lens' Room (Maybe Text)
rProviderCalendarId = lens _rProviderCalendarId (\ s a -> s{_rProviderCalendarId = a})

-- | The ARN of a room.
rRoomARN :: Lens' Room (Maybe Text)
rRoomARN = lens _rRoomARN (\ s a -> s{_rRoomARN = a})

-- | The name of a room.
rRoomName :: Lens' Room (Maybe Text)
rRoomName = lens _rRoomName (\ s a -> s{_rRoomName = a})

-- | The description of a room.
rDescription :: Lens' Room (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

instance FromJSON Room where
        parseJSON
          = withObject "Room"
              (\ x ->
                 Room' <$>
                   (x .:? "ProfileArn") <*> (x .:? "ProviderCalendarId")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "Description"))

instance Hashable Room where

instance NFData Room where

-- | The data of a room.
--
--
--
-- /See:/ 'roomData' smart constructor.
data RoomData = RoomData'
  { _rdProfileARN         :: !(Maybe Text)
  , _rdProviderCalendarId :: !(Maybe Text)
  , _rdProfileName        :: !(Maybe Text)
  , _rdRoomARN            :: !(Maybe Text)
  , _rdRoomName           :: !(Maybe Text)
  , _rdDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoomData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdProfileARN' - The profile ARN of a room.
--
-- * 'rdProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rdProfileName' - The profile name of a room.
--
-- * 'rdRoomARN' - The ARN of a room.
--
-- * 'rdRoomName' - The name of a room.
--
-- * 'rdDescription' - The description of a room.
roomData
    :: RoomData
roomData =
  RoomData'
    { _rdProfileARN = Nothing
    , _rdProviderCalendarId = Nothing
    , _rdProfileName = Nothing
    , _rdRoomARN = Nothing
    , _rdRoomName = Nothing
    , _rdDescription = Nothing
    }


-- | The profile ARN of a room.
rdProfileARN :: Lens' RoomData (Maybe Text)
rdProfileARN = lens _rdProfileARN (\ s a -> s{_rdProfileARN = a})

-- | The provider calendar ARN of a room.
rdProviderCalendarId :: Lens' RoomData (Maybe Text)
rdProviderCalendarId = lens _rdProviderCalendarId (\ s a -> s{_rdProviderCalendarId = a})

-- | The profile name of a room.
rdProfileName :: Lens' RoomData (Maybe Text)
rdProfileName = lens _rdProfileName (\ s a -> s{_rdProfileName = a})

-- | The ARN of a room.
rdRoomARN :: Lens' RoomData (Maybe Text)
rdRoomARN = lens _rdRoomARN (\ s a -> s{_rdRoomARN = a})

-- | The name of a room.
rdRoomName :: Lens' RoomData (Maybe Text)
rdRoomName = lens _rdRoomName (\ s a -> s{_rdRoomName = a})

-- | The description of a room.
rdDescription :: Lens' RoomData (Maybe Text)
rdDescription = lens _rdDescription (\ s a -> s{_rdDescription = a})

instance FromJSON RoomData where
        parseJSON
          = withObject "RoomData"
              (\ x ->
                 RoomData' <$>
                   (x .:? "ProfileArn") <*> (x .:? "ProviderCalendarId")
                     <*> (x .:? "ProfileName")
                     <*> (x .:? "RoomArn")
                     <*> (x .:? "RoomName")
                     <*> (x .:? "Description"))

instance Hashable RoomData where

instance NFData RoomData where

-- | A skill parameter associated with a room.
--
--
--
-- /See:/ 'roomSkillParameter' smart constructor.
data RoomSkillParameter = RoomSkillParameter'
  { _rspParameterKey   :: !Text
  , _rspParameterValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RoomSkillParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rspParameterKey' - The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
--
-- * 'rspParameterValue' - The parameter value of a room skill parameter.
roomSkillParameter
    :: Text -- ^ 'rspParameterKey'
    -> Text -- ^ 'rspParameterValue'
    -> RoomSkillParameter
roomSkillParameter pParameterKey_ pParameterValue_ =
  RoomSkillParameter'
    {_rspParameterKey = pParameterKey_, _rspParameterValue = pParameterValue_}


-- | The parameter key of a room skill parameter. ParameterKey is an enumerated type that only takes “DEFAULT” or “SCOPE” as valid values.
rspParameterKey :: Lens' RoomSkillParameter Text
rspParameterKey = lens _rspParameterKey (\ s a -> s{_rspParameterKey = a})

-- | The parameter value of a room skill parameter.
rspParameterValue :: Lens' RoomSkillParameter Text
rspParameterValue = lens _rspParameterValue (\ s a -> s{_rspParameterValue = a})

instance FromJSON RoomSkillParameter where
        parseJSON
          = withObject "RoomSkillParameter"
              (\ x ->
                 RoomSkillParameter' <$>
                   (x .: "ParameterKey") <*> (x .: "ParameterValue"))

instance Hashable RoomSkillParameter where

instance NFData RoomSkillParameter where

instance ToJSON RoomSkillParameter where
        toJSON RoomSkillParameter'{..}
          = object
              (catMaybes
                 [Just ("ParameterKey" .= _rspParameterKey),
                  Just ("ParameterValue" .= _rspParameterValue)])

-- | A skill group with attributes.
--
--
--
-- /See:/ 'skillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { _sgSkillGroupARN  :: !(Maybe Text)
  , _sgDescription    :: !(Maybe Text)
  , _sgSkillGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgSkillGroupARN' - The ARN of a skill group.
--
-- * 'sgDescription' - The description of a skill group.
--
-- * 'sgSkillGroupName' - The name of a skill group.
skillGroup
    :: SkillGroup
skillGroup =
  SkillGroup'
    { _sgSkillGroupARN = Nothing
    , _sgDescription = Nothing
    , _sgSkillGroupName = Nothing
    }


-- | The ARN of a skill group.
sgSkillGroupARN :: Lens' SkillGroup (Maybe Text)
sgSkillGroupARN = lens _sgSkillGroupARN (\ s a -> s{_sgSkillGroupARN = a})

-- | The description of a skill group.
sgDescription :: Lens' SkillGroup (Maybe Text)
sgDescription = lens _sgDescription (\ s a -> s{_sgDescription = a})

-- | The name of a skill group.
sgSkillGroupName :: Lens' SkillGroup (Maybe Text)
sgSkillGroupName = lens _sgSkillGroupName (\ s a -> s{_sgSkillGroupName = a})

instance FromJSON SkillGroup where
        parseJSON
          = withObject "SkillGroup"
              (\ x ->
                 SkillGroup' <$>
                   (x .:? "SkillGroupArn") <*> (x .:? "Description") <*>
                     (x .:? "SkillGroupName"))

instance Hashable SkillGroup where

instance NFData SkillGroup where

-- | The attributes of a skill group.
--
--
--
-- /See:/ 'skillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { _sgdSkillGroupARN  :: !(Maybe Text)
  , _sgdDescription    :: !(Maybe Text)
  , _sgdSkillGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillGroupData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgdSkillGroupARN' - The skill group ARN of a skill group.
--
-- * 'sgdDescription' - The description of a skill group.
--
-- * 'sgdSkillGroupName' - The skill group name of a skill group.
skillGroupData
    :: SkillGroupData
skillGroupData =
  SkillGroupData'
    { _sgdSkillGroupARN = Nothing
    , _sgdDescription = Nothing
    , _sgdSkillGroupName = Nothing
    }


-- | The skill group ARN of a skill group.
sgdSkillGroupARN :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupARN = lens _sgdSkillGroupARN (\ s a -> s{_sgdSkillGroupARN = a})

-- | The description of a skill group.
sgdDescription :: Lens' SkillGroupData (Maybe Text)
sgdDescription = lens _sgdDescription (\ s a -> s{_sgdDescription = a})

-- | The skill group name of a skill group.
sgdSkillGroupName :: Lens' SkillGroupData (Maybe Text)
sgdSkillGroupName = lens _sgdSkillGroupName (\ s a -> s{_sgdSkillGroupName = a})

instance FromJSON SkillGroupData where
        parseJSON
          = withObject "SkillGroupData"
              (\ x ->
                 SkillGroupData' <$>
                   (x .:? "SkillGroupArn") <*> (x .:? "Description") <*>
                     (x .:? "SkillGroupName"))

instance Hashable SkillGroupData where

instance NFData SkillGroupData where

-- | The summary of skills.
--
--
--
-- /See:/ 'skillSummary' smart constructor.
data SkillSummary = SkillSummary'
  { _ssSkillId         :: !(Maybe Text)
  , _ssSupportsLinking :: !(Maybe Bool)
  , _ssSkillName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkillSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSkillId' - The ARN of the skill summary.
--
-- * 'ssSupportsLinking' - Linking support for a skill.
--
-- * 'ssSkillName' - The name of the skill.
skillSummary
    :: SkillSummary
skillSummary =
  SkillSummary'
    {_ssSkillId = Nothing, _ssSupportsLinking = Nothing, _ssSkillName = Nothing}


-- | The ARN of the skill summary.
ssSkillId :: Lens' SkillSummary (Maybe Text)
ssSkillId = lens _ssSkillId (\ s a -> s{_ssSkillId = a})

-- | Linking support for a skill.
ssSupportsLinking :: Lens' SkillSummary (Maybe Bool)
ssSupportsLinking = lens _ssSupportsLinking (\ s a -> s{_ssSupportsLinking = a})

-- | The name of the skill.
ssSkillName :: Lens' SkillSummary (Maybe Text)
ssSkillName = lens _ssSkillName (\ s a -> s{_ssSkillName = a})

instance FromJSON SkillSummary where
        parseJSON
          = withObject "SkillSummary"
              (\ x ->
                 SkillSummary' <$>
                   (x .:? "SkillId") <*> (x .:? "SupportsLinking") <*>
                     (x .:? "SkillName"))

instance Hashable SkillSummary where

instance NFData SkillSummary where

-- | An object representing a sort criteria.
--
--
--
-- /See:/ 'sort' smart constructor.
data Sort = Sort'
  { _sKey   :: !Text
  , _sValue :: !SortValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Sort' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sKey' - The sort key of a sort object.
--
-- * 'sValue' - The sort value of a sort object.
sort
    :: Text -- ^ 'sKey'
    -> SortValue -- ^ 'sValue'
    -> Sort
sort pKey_ pValue_ = Sort' {_sKey = pKey_, _sValue = pValue_}


-- | The sort key of a sort object.
sKey :: Lens' Sort Text
sKey = lens _sKey (\ s a -> s{_sKey = a})

-- | The sort value of a sort object.
sValue :: Lens' Sort SortValue
sValue = lens _sValue (\ s a -> s{_sValue = a})

instance Hashable Sort where

instance NFData Sort where

instance ToJSON Sort where
        toJSON Sort'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _sKey), Just ("Value" .= _sValue)])

-- | A key-value pair that can be associated with a resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of a tag. Tag values are case-sensitive and can be null.
--
-- * 'tagKey' - The key of a tag. Tag keys are case-sensitive.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value of a tag. Tag values are case-sensitive and can be null.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of a tag. Tag keys are case-sensitive.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- | Information related to a user.
--
--
--
-- /See:/ 'userData' smart constructor.
data UserData = UserData'
  { _udEmail            :: !(Maybe Text)
  , _udLastName         :: !(Maybe Text)
  , _udEnrollmentId     :: !(Maybe Text)
  , _udUserARN          :: !(Maybe Text)
  , _udFirstName        :: !(Maybe Text)
  , _udEnrollmentStatus :: !(Maybe EnrollmentStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udEmail' - The email of a user.
--
-- * 'udLastName' - The last name of a user.
--
-- * 'udEnrollmentId' - The enrollment ARN of a user.
--
-- * 'udUserARN' - The ARN of a user.
--
-- * 'udFirstName' - The first name of a user.
--
-- * 'udEnrollmentStatus' - The enrollment status of a user.
userData
    :: UserData
userData =
  UserData'
    { _udEmail = Nothing
    , _udLastName = Nothing
    , _udEnrollmentId = Nothing
    , _udUserARN = Nothing
    , _udFirstName = Nothing
    , _udEnrollmentStatus = Nothing
    }


-- | The email of a user.
udEmail :: Lens' UserData (Maybe Text)
udEmail = lens _udEmail (\ s a -> s{_udEmail = a})

-- | The last name of a user.
udLastName :: Lens' UserData (Maybe Text)
udLastName = lens _udLastName (\ s a -> s{_udLastName = a})

-- | The enrollment ARN of a user.
udEnrollmentId :: Lens' UserData (Maybe Text)
udEnrollmentId = lens _udEnrollmentId (\ s a -> s{_udEnrollmentId = a})

-- | The ARN of a user.
udUserARN :: Lens' UserData (Maybe Text)
udUserARN = lens _udUserARN (\ s a -> s{_udUserARN = a})

-- | The first name of a user.
udFirstName :: Lens' UserData (Maybe Text)
udFirstName = lens _udFirstName (\ s a -> s{_udFirstName = a})

-- | The enrollment status of a user.
udEnrollmentStatus :: Lens' UserData (Maybe EnrollmentStatus)
udEnrollmentStatus = lens _udEnrollmentStatus (\ s a -> s{_udEnrollmentStatus = a})

instance FromJSON UserData where
        parseJSON
          = withObject "UserData"
              (\ x ->
                 UserData' <$>
                   (x .:? "Email") <*> (x .:? "LastName") <*>
                     (x .:? "EnrollmentId")
                     <*> (x .:? "UserArn")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "EnrollmentStatus"))

instance Hashable UserData where

instance NFData UserData where
