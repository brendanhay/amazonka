{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Product where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.Sum
import Network.AWS.Prelude

-- | Amazon Device Messaging channel definition.
--
-- /See:/ 'aDMChannelRequest' smart constructor.
data ADMChannelRequest = ADMChannelRequest'
  { _admcrClientId     :: !(Maybe Text)
  , _admcrClientSecret :: !(Maybe Text)
  , _admcrEnabled      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ADMChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admcrClientId' - Client ID as gotten from Amazon
--
-- * 'admcrClientSecret' - Client secret as gotten from Amazon
--
-- * 'admcrEnabled' - If the channel is enabled for sending messages.
aDMChannelRequest
    :: ADMChannelRequest
aDMChannelRequest =
  ADMChannelRequest'
    { _admcrClientId = Nothing
    , _admcrClientSecret = Nothing
    , _admcrEnabled = Nothing
    }


-- | Client ID as gotten from Amazon
admcrClientId :: Lens' ADMChannelRequest (Maybe Text)
admcrClientId = lens _admcrClientId (\ s a -> s{_admcrClientId = a})

-- | Client secret as gotten from Amazon
admcrClientSecret :: Lens' ADMChannelRequest (Maybe Text)
admcrClientSecret = lens _admcrClientSecret (\ s a -> s{_admcrClientSecret = a})

-- | If the channel is enabled for sending messages.
admcrEnabled :: Lens' ADMChannelRequest (Maybe Bool)
admcrEnabled = lens _admcrEnabled (\ s a -> s{_admcrEnabled = a})

instance Hashable ADMChannelRequest where

instance NFData ADMChannelRequest where

instance ToJSON ADMChannelRequest where
        toJSON ADMChannelRequest'{..}
          = object
              (catMaybes
                 [("ClientId" .=) <$> _admcrClientId,
                  ("ClientSecret" .=) <$> _admcrClientSecret,
                  ("Enabled" .=) <$> _admcrEnabled])

-- | Amazon Device Messaging channel definition.
--
-- /See:/ 'aDMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
  { _admcPlatform         :: !(Maybe Text)
  , _admcLastModifiedDate :: !(Maybe Text)
  , _admcEnabled          :: !(Maybe Bool)
  , _admcIsArchived       :: !(Maybe Bool)
  , _admcApplicationId    :: !(Maybe Text)
  , _admcVersion          :: !(Maybe Int)
  , _admcId               :: !(Maybe Text)
  , _admcCreationDate     :: !(Maybe Text)
  , _admcLastModifiedBy   :: !(Maybe Text)
  , _admcHasCredential    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ADMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admcPlatform' - Platform type. Will be "ADM"
--
-- * 'admcLastModifiedDate' - Last date this was updated
--
-- * 'admcEnabled' - If the channel is enabled for sending messages.
--
-- * 'admcIsArchived' - Is this channel archived
--
-- * 'admcApplicationId' - The ID of the application to which the channel applies.
--
-- * 'admcVersion' - Version of channel
--
-- * 'admcId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'admcCreationDate' - When was this segment created
--
-- * 'admcLastModifiedBy' - Who last updated this entry
--
-- * 'admcHasCredential' - Indicates whether the channel is configured with ADM credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with ADM. Provide your credentials by setting the ClientId and ClientSecret attributes.
aDMChannelResponse
    :: ADMChannelResponse
aDMChannelResponse =
  ADMChannelResponse'
    { _admcPlatform = Nothing
    , _admcLastModifiedDate = Nothing
    , _admcEnabled = Nothing
    , _admcIsArchived = Nothing
    , _admcApplicationId = Nothing
    , _admcVersion = Nothing
    , _admcId = Nothing
    , _admcCreationDate = Nothing
    , _admcLastModifiedBy = Nothing
    , _admcHasCredential = Nothing
    }


-- | Platform type. Will be "ADM"
admcPlatform :: Lens' ADMChannelResponse (Maybe Text)
admcPlatform = lens _admcPlatform (\ s a -> s{_admcPlatform = a})

-- | Last date this was updated
admcLastModifiedDate :: Lens' ADMChannelResponse (Maybe Text)
admcLastModifiedDate = lens _admcLastModifiedDate (\ s a -> s{_admcLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
admcEnabled :: Lens' ADMChannelResponse (Maybe Bool)
admcEnabled = lens _admcEnabled (\ s a -> s{_admcEnabled = a})

-- | Is this channel archived
admcIsArchived :: Lens' ADMChannelResponse (Maybe Bool)
admcIsArchived = lens _admcIsArchived (\ s a -> s{_admcIsArchived = a})

-- | The ID of the application to which the channel applies.
admcApplicationId :: Lens' ADMChannelResponse (Maybe Text)
admcApplicationId = lens _admcApplicationId (\ s a -> s{_admcApplicationId = a})

-- | Version of channel
admcVersion :: Lens' ADMChannelResponse (Maybe Int)
admcVersion = lens _admcVersion (\ s a -> s{_admcVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
admcId :: Lens' ADMChannelResponse (Maybe Text)
admcId = lens _admcId (\ s a -> s{_admcId = a})

-- | When was this segment created
admcCreationDate :: Lens' ADMChannelResponse (Maybe Text)
admcCreationDate = lens _admcCreationDate (\ s a -> s{_admcCreationDate = a})

-- | Who last updated this entry
admcLastModifiedBy :: Lens' ADMChannelResponse (Maybe Text)
admcLastModifiedBy = lens _admcLastModifiedBy (\ s a -> s{_admcLastModifiedBy = a})

-- | Indicates whether the channel is configured with ADM credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with ADM. Provide your credentials by setting the ClientId and ClientSecret attributes.
admcHasCredential :: Lens' ADMChannelResponse (Maybe Bool)
admcHasCredential = lens _admcHasCredential (\ s a -> s{_admcHasCredential = a})

instance FromJSON ADMChannelResponse where
        parseJSON
          = withObject "ADMChannelResponse"
              (\ x ->
                 ADMChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable ADMChannelResponse where

instance NFData ADMChannelResponse where

-- | ADM Message.
--
-- /See:/ 'aDMMessage' smart constructor.
data ADMMessage = ADMMessage'
  { _admmSubstitutions     :: !(Maybe (Map Text [Text]))
  , _admmExpiresAfter      :: !(Maybe Text)
  , _admmMD5               :: !(Maybe Text)
  , _admmSilentPush        :: !(Maybe Bool)
  , _admmImageIconURL      :: !(Maybe Text)
  , _admmRawContent        :: !(Maybe Text)
  , _admmData              :: !(Maybe (Map Text Text))
  , _admmSmallImageIconURL :: !(Maybe Text)
  , _admmBody              :: !(Maybe Text)
  , _admmURL               :: !(Maybe Text)
  , _admmSound             :: !(Maybe Text)
  , _admmAction            :: !(Maybe Action)
  , _admmImageURL          :: !(Maybe Text)
  , _admmConsolidationKey  :: !(Maybe Text)
  , _admmTitle             :: !(Maybe Text)
  , _admmIconReference     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ADMMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'admmExpiresAfter' - Optional. Number of seconds ADM should retain the message if the device is offline
--
-- * 'admmMD5' - Optional. Base-64-encoded MD5 checksum of the data parameter. Used to verify data integrity
--
-- * 'admmSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'admmImageIconURL' - The URL that points to an image used as the large icon to the notification content view.
--
-- * 'admmRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'admmData' - The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
--
-- * 'admmSmallImageIconURL' - The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
--
-- * 'admmBody' - The message body of the notification, the email body or the text message.
--
-- * 'admmURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'admmSound' - Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
--
-- * 'admmAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
--
-- * 'admmImageURL' - The URL that points to an image used in the push notification.
--
-- * 'admmConsolidationKey' - Optional. Arbitrary string used to indicate multiple messages are logically the same and that ADM is allowed to drop previously enqueued messages in favor of this one.
--
-- * 'admmTitle' - The message title that displays above the message on the user's device.
--
-- * 'admmIconReference' - The icon image name of the asset saved in your application.
aDMMessage
    :: ADMMessage
aDMMessage =
  ADMMessage'
    { _admmSubstitutions = Nothing
    , _admmExpiresAfter = Nothing
    , _admmMD5 = Nothing
    , _admmSilentPush = Nothing
    , _admmImageIconURL = Nothing
    , _admmRawContent = Nothing
    , _admmData = Nothing
    , _admmSmallImageIconURL = Nothing
    , _admmBody = Nothing
    , _admmURL = Nothing
    , _admmSound = Nothing
    , _admmAction = Nothing
    , _admmImageURL = Nothing
    , _admmConsolidationKey = Nothing
    , _admmTitle = Nothing
    , _admmIconReference = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
admmSubstitutions :: Lens' ADMMessage (HashMap Text [Text])
admmSubstitutions = lens _admmSubstitutions (\ s a -> s{_admmSubstitutions = a}) . _Default . _Map

-- | Optional. Number of seconds ADM should retain the message if the device is offline
admmExpiresAfter :: Lens' ADMMessage (Maybe Text)
admmExpiresAfter = lens _admmExpiresAfter (\ s a -> s{_admmExpiresAfter = a})

-- | Optional. Base-64-encoded MD5 checksum of the data parameter. Used to verify data integrity
admmMD5 :: Lens' ADMMessage (Maybe Text)
admmMD5 = lens _admmMD5 (\ s a -> s{_admmMD5 = a})

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
admmSilentPush :: Lens' ADMMessage (Maybe Bool)
admmSilentPush = lens _admmSilentPush (\ s a -> s{_admmSilentPush = a})

-- | The URL that points to an image used as the large icon to the notification content view.
admmImageIconURL :: Lens' ADMMessage (Maybe Text)
admmImageIconURL = lens _admmImageIconURL (\ s a -> s{_admmImageIconURL = a})

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
admmRawContent :: Lens' ADMMessage (Maybe Text)
admmRawContent = lens _admmRawContent (\ s a -> s{_admmRawContent = a})

-- | The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
admmData :: Lens' ADMMessage (HashMap Text Text)
admmData = lens _admmData (\ s a -> s{_admmData = a}) . _Default . _Map

-- | The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
admmSmallImageIconURL :: Lens' ADMMessage (Maybe Text)
admmSmallImageIconURL = lens _admmSmallImageIconURL (\ s a -> s{_admmSmallImageIconURL = a})

-- | The message body of the notification, the email body or the text message.
admmBody :: Lens' ADMMessage (Maybe Text)
admmBody = lens _admmBody (\ s a -> s{_admmBody = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
admmURL :: Lens' ADMMessage (Maybe Text)
admmURL = lens _admmURL (\ s a -> s{_admmURL = a})

-- | Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
admmSound :: Lens' ADMMessage (Maybe Text)
admmSound = lens _admmSound (\ s a -> s{_admmSound = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
admmAction :: Lens' ADMMessage (Maybe Action)
admmAction = lens _admmAction (\ s a -> s{_admmAction = a})

-- | The URL that points to an image used in the push notification.
admmImageURL :: Lens' ADMMessage (Maybe Text)
admmImageURL = lens _admmImageURL (\ s a -> s{_admmImageURL = a})

-- | Optional. Arbitrary string used to indicate multiple messages are logically the same and that ADM is allowed to drop previously enqueued messages in favor of this one.
admmConsolidationKey :: Lens' ADMMessage (Maybe Text)
admmConsolidationKey = lens _admmConsolidationKey (\ s a -> s{_admmConsolidationKey = a})

-- | The message title that displays above the message on the user's device.
admmTitle :: Lens' ADMMessage (Maybe Text)
admmTitle = lens _admmTitle (\ s a -> s{_admmTitle = a})

-- | The icon image name of the asset saved in your application.
admmIconReference :: Lens' ADMMessage (Maybe Text)
admmIconReference = lens _admmIconReference (\ s a -> s{_admmIconReference = a})

instance Hashable ADMMessage where

instance NFData ADMMessage where

instance ToJSON ADMMessage where
        toJSON ADMMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _admmSubstitutions,
                  ("ExpiresAfter" .=) <$> _admmExpiresAfter,
                  ("MD5" .=) <$> _admmMD5,
                  ("SilentPush" .=) <$> _admmSilentPush,
                  ("ImageIconUrl" .=) <$> _admmImageIconURL,
                  ("RawContent" .=) <$> _admmRawContent,
                  ("Data" .=) <$> _admmData,
                  ("SmallImageIconUrl" .=) <$> _admmSmallImageIconURL,
                  ("Body" .=) <$> _admmBody, ("Url" .=) <$> _admmURL,
                  ("Sound" .=) <$> _admmSound,
                  ("Action" .=) <$> _admmAction,
                  ("ImageUrl" .=) <$> _admmImageURL,
                  ("ConsolidationKey" .=) <$> _admmConsolidationKey,
                  ("Title" .=) <$> _admmTitle,
                  ("IconReference" .=) <$> _admmIconReference])

-- | Apple Push Notification Service channel definition.
--
-- /See:/ 'apnsChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
  { _acrTokenKey                    :: !(Maybe Text)
  , _acrPrivateKey                  :: !(Maybe Text)
  , _acrEnabled                     :: !(Maybe Bool)
  , _acrTeamId                      :: !(Maybe Text)
  , _acrBundleId                    :: !(Maybe Text)
  , _acrDefaultAuthenticationMethod :: !(Maybe Text)
  , _acrCertificate                 :: !(Maybe Text)
  , _acrTokenKeyId                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrTokenKey' - The token key used for APNs Tokens.
--
-- * 'acrPrivateKey' - The certificate private key.
--
-- * 'acrEnabled' - If the channel is enabled for sending messages.
--
-- * 'acrTeamId' - The team id used for APNs Tokens.
--
-- * 'acrBundleId' - The bundle id used for APNs Tokens.
--
-- * 'acrDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'acrCertificate' - The distribution certificate from Apple.
--
-- * 'acrTokenKeyId' - The token key used for APNs Tokens.
apnsChannelRequest
    :: APNSChannelRequest
apnsChannelRequest =
  APNSChannelRequest'
    { _acrTokenKey = Nothing
    , _acrPrivateKey = Nothing
    , _acrEnabled = Nothing
    , _acrTeamId = Nothing
    , _acrBundleId = Nothing
    , _acrDefaultAuthenticationMethod = Nothing
    , _acrCertificate = Nothing
    , _acrTokenKeyId = Nothing
    }


-- | The token key used for APNs Tokens.
acrTokenKey :: Lens' APNSChannelRequest (Maybe Text)
acrTokenKey = lens _acrTokenKey (\ s a -> s{_acrTokenKey = a})

-- | The certificate private key.
acrPrivateKey :: Lens' APNSChannelRequest (Maybe Text)
acrPrivateKey = lens _acrPrivateKey (\ s a -> s{_acrPrivateKey = a})

-- | If the channel is enabled for sending messages.
acrEnabled :: Lens' APNSChannelRequest (Maybe Bool)
acrEnabled = lens _acrEnabled (\ s a -> s{_acrEnabled = a})

-- | The team id used for APNs Tokens.
acrTeamId :: Lens' APNSChannelRequest (Maybe Text)
acrTeamId = lens _acrTeamId (\ s a -> s{_acrTeamId = a})

-- | The bundle id used for APNs Tokens.
acrBundleId :: Lens' APNSChannelRequest (Maybe Text)
acrBundleId = lens _acrBundleId (\ s a -> s{_acrBundleId = a})

-- | The default authentication method used for APNs.
acrDefaultAuthenticationMethod :: Lens' APNSChannelRequest (Maybe Text)
acrDefaultAuthenticationMethod = lens _acrDefaultAuthenticationMethod (\ s a -> s{_acrDefaultAuthenticationMethod = a})

-- | The distribution certificate from Apple.
acrCertificate :: Lens' APNSChannelRequest (Maybe Text)
acrCertificate = lens _acrCertificate (\ s a -> s{_acrCertificate = a})

-- | The token key used for APNs Tokens.
acrTokenKeyId :: Lens' APNSChannelRequest (Maybe Text)
acrTokenKeyId = lens _acrTokenKeyId (\ s a -> s{_acrTokenKeyId = a})

instance Hashable APNSChannelRequest where

instance NFData APNSChannelRequest where

instance ToJSON APNSChannelRequest where
        toJSON APNSChannelRequest'{..}
          = object
              (catMaybes
                 [("TokenKey" .=) <$> _acrTokenKey,
                  ("PrivateKey" .=) <$> _acrPrivateKey,
                  ("Enabled" .=) <$> _acrEnabled,
                  ("TeamId" .=) <$> _acrTeamId,
                  ("BundleId" .=) <$> _acrBundleId,
                  ("DefaultAuthenticationMethod" .=) <$>
                    _acrDefaultAuthenticationMethod,
                  ("Certificate" .=) <$> _acrCertificate,
                  ("TokenKeyId" .=) <$> _acrTokenKeyId])

-- | Apple Distribution Push Notification Service channel definition.
--
-- /See:/ 'apnsChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { _acPlatform                    :: !(Maybe Text)
  , _acLastModifiedDate            :: !(Maybe Text)
  , _acEnabled                     :: !(Maybe Bool)
  , _acHasTokenKey                 :: !(Maybe Bool)
  , _acDefaultAuthenticationMethod :: !(Maybe Text)
  , _acIsArchived                  :: !(Maybe Bool)
  , _acApplicationId               :: !(Maybe Text)
  , _acVersion                     :: !(Maybe Int)
  , _acId                          :: !(Maybe Text)
  , _acCreationDate                :: !(Maybe Text)
  , _acLastModifiedBy              :: !(Maybe Text)
  , _acHasCredential               :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acPlatform' - The platform type. Will be APNS.
--
-- * 'acLastModifiedDate' - Last date this was updated
--
-- * 'acEnabled' - If the channel is enabled for sending messages.
--
-- * 'acHasTokenKey' - Indicates whether the channel is configured with a key for APNs token authentication. Provide a token key by setting the TokenKey attribute.
--
-- * 'acDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'acIsArchived' - Is this channel archived
--
-- * 'acApplicationId' - The ID of the application to which the channel applies.
--
-- * 'acVersion' - Version of channel
--
-- * 'acId' - Channel ID. Not used. Present only for backwards compatibility.
--
-- * 'acCreationDate' - When was this segment created
--
-- * 'acLastModifiedBy' - Who last updated this entry
--
-- * 'acHasCredential' - Indicates whether the channel is configured with APNs credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with APNs. To use APNs token authentication, set the BundleId, TeamId, TokenKey, and TokenKeyId attributes. To use certificate authentication, set the Certificate and PrivateKey attributes.
apnsChannelResponse
    :: APNSChannelResponse
apnsChannelResponse =
  APNSChannelResponse'
    { _acPlatform = Nothing
    , _acLastModifiedDate = Nothing
    , _acEnabled = Nothing
    , _acHasTokenKey = Nothing
    , _acDefaultAuthenticationMethod = Nothing
    , _acIsArchived = Nothing
    , _acApplicationId = Nothing
    , _acVersion = Nothing
    , _acId = Nothing
    , _acCreationDate = Nothing
    , _acLastModifiedBy = Nothing
    , _acHasCredential = Nothing
    }


-- | The platform type. Will be APNS.
acPlatform :: Lens' APNSChannelResponse (Maybe Text)
acPlatform = lens _acPlatform (\ s a -> s{_acPlatform = a})

-- | Last date this was updated
acLastModifiedDate :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedDate = lens _acLastModifiedDate (\ s a -> s{_acLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
acEnabled :: Lens' APNSChannelResponse (Maybe Bool)
acEnabled = lens _acEnabled (\ s a -> s{_acEnabled = a})

-- | Indicates whether the channel is configured with a key for APNs token authentication. Provide a token key by setting the TokenKey attribute.
acHasTokenKey :: Lens' APNSChannelResponse (Maybe Bool)
acHasTokenKey = lens _acHasTokenKey (\ s a -> s{_acHasTokenKey = a})

-- | The default authentication method used for APNs.
acDefaultAuthenticationMethod :: Lens' APNSChannelResponse (Maybe Text)
acDefaultAuthenticationMethod = lens _acDefaultAuthenticationMethod (\ s a -> s{_acDefaultAuthenticationMethod = a})

-- | Is this channel archived
acIsArchived :: Lens' APNSChannelResponse (Maybe Bool)
acIsArchived = lens _acIsArchived (\ s a -> s{_acIsArchived = a})

-- | The ID of the application to which the channel applies.
acApplicationId :: Lens' APNSChannelResponse (Maybe Text)
acApplicationId = lens _acApplicationId (\ s a -> s{_acApplicationId = a})

-- | Version of channel
acVersion :: Lens' APNSChannelResponse (Maybe Int)
acVersion = lens _acVersion (\ s a -> s{_acVersion = a})

-- | Channel ID. Not used. Present only for backwards compatibility.
acId :: Lens' APNSChannelResponse (Maybe Text)
acId = lens _acId (\ s a -> s{_acId = a})

-- | When was this segment created
acCreationDate :: Lens' APNSChannelResponse (Maybe Text)
acCreationDate = lens _acCreationDate (\ s a -> s{_acCreationDate = a})

-- | Who last updated this entry
acLastModifiedBy :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedBy = lens _acLastModifiedBy (\ s a -> s{_acLastModifiedBy = a})

-- | Indicates whether the channel is configured with APNs credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with APNs. To use APNs token authentication, set the BundleId, TeamId, TokenKey, and TokenKeyId attributes. To use certificate authentication, set the Certificate and PrivateKey attributes.
acHasCredential :: Lens' APNSChannelResponse (Maybe Bool)
acHasCredential = lens _acHasCredential (\ s a -> s{_acHasCredential = a})

instance FromJSON APNSChannelResponse where
        parseJSON
          = withObject "APNSChannelResponse"
              (\ x ->
                 APNSChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "HasTokenKey")
                     <*> (x .:? "DefaultAuthenticationMethod")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable APNSChannelResponse where

instance NFData APNSChannelResponse where

-- | APNS Message.
--
-- /See:/ 'apnsMessage' smart constructor.
data APNSMessage = APNSMessage'
  { _amSubstitutions                 :: !(Maybe (Map Text [Text]))
  , _amSilentPush                    :: !(Maybe Bool)
  , _amPriority                      :: !(Maybe Text)
  , _amRawContent                    :: !(Maybe Text)
  , _amData                          :: !(Maybe (Map Text Text))
  , _amBody                          :: !(Maybe Text)
  , _amCategory                      :: !(Maybe Text)
  , _amTimeToLive                    :: !(Maybe Int)
  , _amURL                           :: !(Maybe Text)
  , _amSound                         :: !(Maybe Text)
  , _amAction                        :: !(Maybe Action)
  , _amMediaURL                      :: !(Maybe Text)
  , _amPreferredAuthenticationMethod :: !(Maybe Text)
  , _amBadge                         :: !(Maybe Int)
  , _amTitle                         :: !(Maybe Text)
  , _amThreadId                      :: !(Maybe Text)
  , _amCollapseId                    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'amSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'amPriority' - The message priority. Amazon Pinpoint uses this value to set the apns-priority request header when it sends the message to APNs. Accepts the following values: "5" - Low priority. Messages might be delayed, delivered in groups, and throttled. "10" - High priority. Messages are sent immediately. High priority messages must cause an alert, sound, or badge on the receiving device. The default value is "10". The equivalent values for FCM or GCM messages are "normal" and "high". Amazon Pinpoint accepts these values for APNs messages and converts them. For more information about the apns-priority parameter, see Communicating with APNs in the APNs Local and Remote Notification Programming Guide.
--
-- * 'amRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'amData' - The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
--
-- * 'amBody' - The message body of the notification, the email body or the text message.
--
-- * 'amCategory' - Provide this key with a string value that represents the notification's type. This value corresponds to the value in the identifier property of one of your app's registered categories.
--
-- * 'amTimeToLive' - The length of time (in seconds) that APNs stores and attempts to deliver the message. If the value is 0, APNs does not store the message or attempt to deliver it more than once. Amazon Pinpoint uses this value to set the apns-expiration request header when it sends the message to APNs.
--
-- * 'amURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'amSound' - Include this key when you want the system to play a sound. The value of this key is the name of a sound file in your app's main bundle or in the Library/Sounds folder of your app's data container. If the sound file cannot be found, or if you specify defaultfor the value, the system plays the default alert sound.
--
-- * 'amAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
--
-- * 'amMediaURL' - The URL that points to a video used in the push notification.
--
-- * 'amPreferredAuthenticationMethod' - The preferred authentication method, either "CERTIFICATE" or "TOKEN"
--
-- * 'amBadge' - Include this key when you want the system to modify the badge of your app icon. If this key is not included in the dictionary, the badge is not changed. To remove the badge, set the value of this key to 0.
--
-- * 'amTitle' - The message title that displays above the message on the user's device.
--
-- * 'amThreadId' - Provide this key with a string value that represents the app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
--
-- * 'amCollapseId' - An ID that, if assigned to multiple messages, causes APNs to coalesce the messages into a single push notification instead of delivering each message individually. The value must not exceed 64 bytes. Amazon Pinpoint uses this value to set the apns-collapse-id request header when it sends the message to APNs.
apnsMessage
    :: APNSMessage
apnsMessage =
  APNSMessage'
    { _amSubstitutions = Nothing
    , _amSilentPush = Nothing
    , _amPriority = Nothing
    , _amRawContent = Nothing
    , _amData = Nothing
    , _amBody = Nothing
    , _amCategory = Nothing
    , _amTimeToLive = Nothing
    , _amURL = Nothing
    , _amSound = Nothing
    , _amAction = Nothing
    , _amMediaURL = Nothing
    , _amPreferredAuthenticationMethod = Nothing
    , _amBadge = Nothing
    , _amTitle = Nothing
    , _amThreadId = Nothing
    , _amCollapseId = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
amSubstitutions :: Lens' APNSMessage (HashMap Text [Text])
amSubstitutions = lens _amSubstitutions (\ s a -> s{_amSubstitutions = a}) . _Default . _Map

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
amSilentPush :: Lens' APNSMessage (Maybe Bool)
amSilentPush = lens _amSilentPush (\ s a -> s{_amSilentPush = a})

-- | The message priority. Amazon Pinpoint uses this value to set the apns-priority request header when it sends the message to APNs. Accepts the following values: "5" - Low priority. Messages might be delayed, delivered in groups, and throttled. "10" - High priority. Messages are sent immediately. High priority messages must cause an alert, sound, or badge on the receiving device. The default value is "10". The equivalent values for FCM or GCM messages are "normal" and "high". Amazon Pinpoint accepts these values for APNs messages and converts them. For more information about the apns-priority parameter, see Communicating with APNs in the APNs Local and Remote Notification Programming Guide.
amPriority :: Lens' APNSMessage (Maybe Text)
amPriority = lens _amPriority (\ s a -> s{_amPriority = a})

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
amRawContent :: Lens' APNSMessage (Maybe Text)
amRawContent = lens _amRawContent (\ s a -> s{_amRawContent = a})

-- | The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
amData :: Lens' APNSMessage (HashMap Text Text)
amData = lens _amData (\ s a -> s{_amData = a}) . _Default . _Map

-- | The message body of the notification, the email body or the text message.
amBody :: Lens' APNSMessage (Maybe Text)
amBody = lens _amBody (\ s a -> s{_amBody = a})

-- | Provide this key with a string value that represents the notification's type. This value corresponds to the value in the identifier property of one of your app's registered categories.
amCategory :: Lens' APNSMessage (Maybe Text)
amCategory = lens _amCategory (\ s a -> s{_amCategory = a})

-- | The length of time (in seconds) that APNs stores and attempts to deliver the message. If the value is 0, APNs does not store the message or attempt to deliver it more than once. Amazon Pinpoint uses this value to set the apns-expiration request header when it sends the message to APNs.
amTimeToLive :: Lens' APNSMessage (Maybe Int)
amTimeToLive = lens _amTimeToLive (\ s a -> s{_amTimeToLive = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
amURL :: Lens' APNSMessage (Maybe Text)
amURL = lens _amURL (\ s a -> s{_amURL = a})

-- | Include this key when you want the system to play a sound. The value of this key is the name of a sound file in your app's main bundle or in the Library/Sounds folder of your app's data container. If the sound file cannot be found, or if you specify defaultfor the value, the system plays the default alert sound.
amSound :: Lens' APNSMessage (Maybe Text)
amSound = lens _amSound (\ s a -> s{_amSound = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
amAction :: Lens' APNSMessage (Maybe Action)
amAction = lens _amAction (\ s a -> s{_amAction = a})

-- | The URL that points to a video used in the push notification.
amMediaURL :: Lens' APNSMessage (Maybe Text)
amMediaURL = lens _amMediaURL (\ s a -> s{_amMediaURL = a})

-- | The preferred authentication method, either "CERTIFICATE" or "TOKEN"
amPreferredAuthenticationMethod :: Lens' APNSMessage (Maybe Text)
amPreferredAuthenticationMethod = lens _amPreferredAuthenticationMethod (\ s a -> s{_amPreferredAuthenticationMethod = a})

-- | Include this key when you want the system to modify the badge of your app icon. If this key is not included in the dictionary, the badge is not changed. To remove the badge, set the value of this key to 0.
amBadge :: Lens' APNSMessage (Maybe Int)
amBadge = lens _amBadge (\ s a -> s{_amBadge = a})

-- | The message title that displays above the message on the user's device.
amTitle :: Lens' APNSMessage (Maybe Text)
amTitle = lens _amTitle (\ s a -> s{_amTitle = a})

-- | Provide this key with a string value that represents the app-specific identifier for grouping notifications. If you provide a Notification Content app extension, you can use this value to group your notifications together.
amThreadId :: Lens' APNSMessage (Maybe Text)
amThreadId = lens _amThreadId (\ s a -> s{_amThreadId = a})

-- | An ID that, if assigned to multiple messages, causes APNs to coalesce the messages into a single push notification instead of delivering each message individually. The value must not exceed 64 bytes. Amazon Pinpoint uses this value to set the apns-collapse-id request header when it sends the message to APNs.
amCollapseId :: Lens' APNSMessage (Maybe Text)
amCollapseId = lens _amCollapseId (\ s a -> s{_amCollapseId = a})

instance Hashable APNSMessage where

instance NFData APNSMessage where

instance ToJSON APNSMessage where
        toJSON APNSMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _amSubstitutions,
                  ("SilentPush" .=) <$> _amSilentPush,
                  ("Priority" .=) <$> _amPriority,
                  ("RawContent" .=) <$> _amRawContent,
                  ("Data" .=) <$> _amData, ("Body" .=) <$> _amBody,
                  ("Category" .=) <$> _amCategory,
                  ("TimeToLive" .=) <$> _amTimeToLive,
                  ("Url" .=) <$> _amURL, ("Sound" .=) <$> _amSound,
                  ("Action" .=) <$> _amAction,
                  ("MediaUrl" .=) <$> _amMediaURL,
                  ("PreferredAuthenticationMethod" .=) <$>
                    _amPreferredAuthenticationMethod,
                  ("Badge" .=) <$> _amBadge, ("Title" .=) <$> _amTitle,
                  ("ThreadId" .=) <$> _amThreadId,
                  ("CollapseId" .=) <$> _amCollapseId])

-- | Apple Development Push Notification Service channel definition.
--
-- /See:/ 'apnsSandboxChannelRequest' smart constructor.
data APNSSandboxChannelRequest = APNSSandboxChannelRequest'
  { _ascrTokenKey                    :: !(Maybe Text)
  , _ascrPrivateKey                  :: !(Maybe Text)
  , _ascrEnabled                     :: !(Maybe Bool)
  , _ascrTeamId                      :: !(Maybe Text)
  , _ascrBundleId                    :: !(Maybe Text)
  , _ascrDefaultAuthenticationMethod :: !(Maybe Text)
  , _ascrCertificate                 :: !(Maybe Text)
  , _ascrTokenKeyId                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSSandboxChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascrTokenKey' - The token key used for APNs Tokens.
--
-- * 'ascrPrivateKey' - The certificate private key.
--
-- * 'ascrEnabled' - If the channel is enabled for sending messages.
--
-- * 'ascrTeamId' - The team id used for APNs Tokens.
--
-- * 'ascrBundleId' - The bundle id used for APNs Tokens.
--
-- * 'ascrDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'ascrCertificate' - The distribution certificate from Apple.
--
-- * 'ascrTokenKeyId' - The token key used for APNs Tokens.
apnsSandboxChannelRequest
    :: APNSSandboxChannelRequest
apnsSandboxChannelRequest =
  APNSSandboxChannelRequest'
    { _ascrTokenKey = Nothing
    , _ascrPrivateKey = Nothing
    , _ascrEnabled = Nothing
    , _ascrTeamId = Nothing
    , _ascrBundleId = Nothing
    , _ascrDefaultAuthenticationMethod = Nothing
    , _ascrCertificate = Nothing
    , _ascrTokenKeyId = Nothing
    }


-- | The token key used for APNs Tokens.
ascrTokenKey :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTokenKey = lens _ascrTokenKey (\ s a -> s{_ascrTokenKey = a})

-- | The certificate private key.
ascrPrivateKey :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrPrivateKey = lens _ascrPrivateKey (\ s a -> s{_ascrPrivateKey = a})

-- | If the channel is enabled for sending messages.
ascrEnabled :: Lens' APNSSandboxChannelRequest (Maybe Bool)
ascrEnabled = lens _ascrEnabled (\ s a -> s{_ascrEnabled = a})

-- | The team id used for APNs Tokens.
ascrTeamId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTeamId = lens _ascrTeamId (\ s a -> s{_ascrTeamId = a})

-- | The bundle id used for APNs Tokens.
ascrBundleId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrBundleId = lens _ascrBundleId (\ s a -> s{_ascrBundleId = a})

-- | The default authentication method used for APNs.
ascrDefaultAuthenticationMethod :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrDefaultAuthenticationMethod = lens _ascrDefaultAuthenticationMethod (\ s a -> s{_ascrDefaultAuthenticationMethod = a})

-- | The distribution certificate from Apple.
ascrCertificate :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrCertificate = lens _ascrCertificate (\ s a -> s{_ascrCertificate = a})

-- | The token key used for APNs Tokens.
ascrTokenKeyId :: Lens' APNSSandboxChannelRequest (Maybe Text)
ascrTokenKeyId = lens _ascrTokenKeyId (\ s a -> s{_ascrTokenKeyId = a})

instance Hashable APNSSandboxChannelRequest where

instance NFData APNSSandboxChannelRequest where

instance ToJSON APNSSandboxChannelRequest where
        toJSON APNSSandboxChannelRequest'{..}
          = object
              (catMaybes
                 [("TokenKey" .=) <$> _ascrTokenKey,
                  ("PrivateKey" .=) <$> _ascrPrivateKey,
                  ("Enabled" .=) <$> _ascrEnabled,
                  ("TeamId" .=) <$> _ascrTeamId,
                  ("BundleId" .=) <$> _ascrBundleId,
                  ("DefaultAuthenticationMethod" .=) <$>
                    _ascrDefaultAuthenticationMethod,
                  ("Certificate" .=) <$> _ascrCertificate,
                  ("TokenKeyId" .=) <$> _ascrTokenKeyId])

-- | Apple Development Push Notification Service channel definition.
--
-- /See:/ 'apnsSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { _ascPlatform                    :: !(Maybe Text)
  , _ascLastModifiedDate            :: !(Maybe Text)
  , _ascEnabled                     :: !(Maybe Bool)
  , _ascHasTokenKey                 :: !(Maybe Bool)
  , _ascDefaultAuthenticationMethod :: !(Maybe Text)
  , _ascIsArchived                  :: !(Maybe Bool)
  , _ascApplicationId               :: !(Maybe Text)
  , _ascVersion                     :: !(Maybe Int)
  , _ascId                          :: !(Maybe Text)
  , _ascCreationDate                :: !(Maybe Text)
  , _ascLastModifiedBy              :: !(Maybe Text)
  , _ascHasCredential               :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascPlatform' - The platform type. Will be APNS_SANDBOX.
--
-- * 'ascLastModifiedDate' - Last date this was updated
--
-- * 'ascEnabled' - If the channel is enabled for sending messages.
--
-- * 'ascHasTokenKey' - Indicates whether the channel is configured with a key for APNs token authentication. Provide a token key by setting the TokenKey attribute.
--
-- * 'ascDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'ascIsArchived' - Is this channel archived
--
-- * 'ascApplicationId' - The ID of the application to which the channel applies.
--
-- * 'ascVersion' - Version of channel
--
-- * 'ascId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'ascCreationDate' - When was this segment created
--
-- * 'ascLastModifiedBy' - Who last updated this entry
--
-- * 'ascHasCredential' - Indicates whether the channel is configured with APNs credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with APNs. To use APNs token authentication, set the BundleId, TeamId, TokenKey, and TokenKeyId attributes. To use certificate authentication, set the Certificate and PrivateKey attributes.
apnsSandboxChannelResponse
    :: APNSSandboxChannelResponse
apnsSandboxChannelResponse =
  APNSSandboxChannelResponse'
    { _ascPlatform = Nothing
    , _ascLastModifiedDate = Nothing
    , _ascEnabled = Nothing
    , _ascHasTokenKey = Nothing
    , _ascDefaultAuthenticationMethod = Nothing
    , _ascIsArchived = Nothing
    , _ascApplicationId = Nothing
    , _ascVersion = Nothing
    , _ascId = Nothing
    , _ascCreationDate = Nothing
    , _ascLastModifiedBy = Nothing
    , _ascHasCredential = Nothing
    }


-- | The platform type. Will be APNS_SANDBOX.
ascPlatform :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascPlatform = lens _ascPlatform (\ s a -> s{_ascPlatform = a})

-- | Last date this was updated
ascLastModifiedDate :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascLastModifiedDate = lens _ascLastModifiedDate (\ s a -> s{_ascLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
ascEnabled :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascEnabled = lens _ascEnabled (\ s a -> s{_ascEnabled = a})

-- | Indicates whether the channel is configured with a key for APNs token authentication. Provide a token key by setting the TokenKey attribute.
ascHasTokenKey :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascHasTokenKey = lens _ascHasTokenKey (\ s a -> s{_ascHasTokenKey = a})

-- | The default authentication method used for APNs.
ascDefaultAuthenticationMethod :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascDefaultAuthenticationMethod = lens _ascDefaultAuthenticationMethod (\ s a -> s{_ascDefaultAuthenticationMethod = a})

-- | Is this channel archived
ascIsArchived :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascIsArchived = lens _ascIsArchived (\ s a -> s{_ascIsArchived = a})

-- | The ID of the application to which the channel applies.
ascApplicationId :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascApplicationId = lens _ascApplicationId (\ s a -> s{_ascApplicationId = a})

-- | Version of channel
ascVersion :: Lens' APNSSandboxChannelResponse (Maybe Int)
ascVersion = lens _ascVersion (\ s a -> s{_ascVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
ascId :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascId = lens _ascId (\ s a -> s{_ascId = a})

-- | When was this segment created
ascCreationDate :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascCreationDate = lens _ascCreationDate (\ s a -> s{_ascCreationDate = a})

-- | Who last updated this entry
ascLastModifiedBy :: Lens' APNSSandboxChannelResponse (Maybe Text)
ascLastModifiedBy = lens _ascLastModifiedBy (\ s a -> s{_ascLastModifiedBy = a})

-- | Indicates whether the channel is configured with APNs credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with APNs. To use APNs token authentication, set the BundleId, TeamId, TokenKey, and TokenKeyId attributes. To use certificate authentication, set the Certificate and PrivateKey attributes.
ascHasCredential :: Lens' APNSSandboxChannelResponse (Maybe Bool)
ascHasCredential = lens _ascHasCredential (\ s a -> s{_ascHasCredential = a})

instance FromJSON APNSSandboxChannelResponse where
        parseJSON
          = withObject "APNSSandboxChannelResponse"
              (\ x ->
                 APNSSandboxChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "HasTokenKey")
                     <*> (x .:? "DefaultAuthenticationMethod")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable APNSSandboxChannelResponse where

instance NFData APNSSandboxChannelResponse where

-- | Apple VoIP Push Notification Service channel definition.
--
-- /See:/ 'apnsVoipChannelRequest' smart constructor.
data APNSVoipChannelRequest = APNSVoipChannelRequest'
  { _avcrTokenKey                    :: !(Maybe Text)
  , _avcrPrivateKey                  :: !(Maybe Text)
  , _avcrEnabled                     :: !(Maybe Bool)
  , _avcrTeamId                      :: !(Maybe Text)
  , _avcrBundleId                    :: !(Maybe Text)
  , _avcrDefaultAuthenticationMethod :: !(Maybe Text)
  , _avcrCertificate                 :: !(Maybe Text)
  , _avcrTokenKeyId                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSVoipChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcrTokenKey' - The token key used for APNs Tokens.
--
-- * 'avcrPrivateKey' - The certificate private key.
--
-- * 'avcrEnabled' - If the channel is enabled for sending messages.
--
-- * 'avcrTeamId' - The team id used for APNs Tokens.
--
-- * 'avcrBundleId' - The bundle id used for APNs Tokens.
--
-- * 'avcrDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'avcrCertificate' - The distribution certificate from Apple.
--
-- * 'avcrTokenKeyId' - The token key used for APNs Tokens.
apnsVoipChannelRequest
    :: APNSVoipChannelRequest
apnsVoipChannelRequest =
  APNSVoipChannelRequest'
    { _avcrTokenKey = Nothing
    , _avcrPrivateKey = Nothing
    , _avcrEnabled = Nothing
    , _avcrTeamId = Nothing
    , _avcrBundleId = Nothing
    , _avcrDefaultAuthenticationMethod = Nothing
    , _avcrCertificate = Nothing
    , _avcrTokenKeyId = Nothing
    }


-- | The token key used for APNs Tokens.
avcrTokenKey :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrTokenKey = lens _avcrTokenKey (\ s a -> s{_avcrTokenKey = a})

-- | The certificate private key.
avcrPrivateKey :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrPrivateKey = lens _avcrPrivateKey (\ s a -> s{_avcrPrivateKey = a})

-- | If the channel is enabled for sending messages.
avcrEnabled :: Lens' APNSVoipChannelRequest (Maybe Bool)
avcrEnabled = lens _avcrEnabled (\ s a -> s{_avcrEnabled = a})

-- | The team id used for APNs Tokens.
avcrTeamId :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrTeamId = lens _avcrTeamId (\ s a -> s{_avcrTeamId = a})

-- | The bundle id used for APNs Tokens.
avcrBundleId :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrBundleId = lens _avcrBundleId (\ s a -> s{_avcrBundleId = a})

-- | The default authentication method used for APNs.
avcrDefaultAuthenticationMethod :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrDefaultAuthenticationMethod = lens _avcrDefaultAuthenticationMethod (\ s a -> s{_avcrDefaultAuthenticationMethod = a})

-- | The distribution certificate from Apple.
avcrCertificate :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrCertificate = lens _avcrCertificate (\ s a -> s{_avcrCertificate = a})

-- | The token key used for APNs Tokens.
avcrTokenKeyId :: Lens' APNSVoipChannelRequest (Maybe Text)
avcrTokenKeyId = lens _avcrTokenKeyId (\ s a -> s{_avcrTokenKeyId = a})

instance Hashable APNSVoipChannelRequest where

instance NFData APNSVoipChannelRequest where

instance ToJSON APNSVoipChannelRequest where
        toJSON APNSVoipChannelRequest'{..}
          = object
              (catMaybes
                 [("TokenKey" .=) <$> _avcrTokenKey,
                  ("PrivateKey" .=) <$> _avcrPrivateKey,
                  ("Enabled" .=) <$> _avcrEnabled,
                  ("TeamId" .=) <$> _avcrTeamId,
                  ("BundleId" .=) <$> _avcrBundleId,
                  ("DefaultAuthenticationMethod" .=) <$>
                    _avcrDefaultAuthenticationMethod,
                  ("Certificate" .=) <$> _avcrCertificate,
                  ("TokenKeyId" .=) <$> _avcrTokenKeyId])

-- | Apple VoIP Push Notification Service channel definition.
--
-- /See:/ 'apnsVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { _avcPlatform                    :: !(Maybe Text)
  , _avcLastModifiedDate            :: !(Maybe Text)
  , _avcEnabled                     :: !(Maybe Bool)
  , _avcHasTokenKey                 :: !(Maybe Bool)
  , _avcDefaultAuthenticationMethod :: !(Maybe Text)
  , _avcIsArchived                  :: !(Maybe Bool)
  , _avcApplicationId               :: !(Maybe Text)
  , _avcVersion                     :: !(Maybe Int)
  , _avcId                          :: !(Maybe Text)
  , _avcCreationDate                :: !(Maybe Text)
  , _avcLastModifiedBy              :: !(Maybe Text)
  , _avcHasCredential               :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avcPlatform' - The platform type. Will be APNS.
--
-- * 'avcLastModifiedDate' - Last date this was updated
--
-- * 'avcEnabled' - If the channel is enabled for sending messages.
--
-- * 'avcHasTokenKey' - If the channel is registered with a token key for authentication.
--
-- * 'avcDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'avcIsArchived' - Is this channel archived
--
-- * 'avcApplicationId' - Application id
--
-- * 'avcVersion' - Version of channel
--
-- * 'avcId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'avcCreationDate' - When was this segment created
--
-- * 'avcLastModifiedBy' - Who made the last change
--
-- * 'avcHasCredential' - If the channel is registered with a credential for authentication.
apnsVoipChannelResponse
    :: APNSVoipChannelResponse
apnsVoipChannelResponse =
  APNSVoipChannelResponse'
    { _avcPlatform = Nothing
    , _avcLastModifiedDate = Nothing
    , _avcEnabled = Nothing
    , _avcHasTokenKey = Nothing
    , _avcDefaultAuthenticationMethod = Nothing
    , _avcIsArchived = Nothing
    , _avcApplicationId = Nothing
    , _avcVersion = Nothing
    , _avcId = Nothing
    , _avcCreationDate = Nothing
    , _avcLastModifiedBy = Nothing
    , _avcHasCredential = Nothing
    }


-- | The platform type. Will be APNS.
avcPlatform :: Lens' APNSVoipChannelResponse (Maybe Text)
avcPlatform = lens _avcPlatform (\ s a -> s{_avcPlatform = a})

-- | Last date this was updated
avcLastModifiedDate :: Lens' APNSVoipChannelResponse (Maybe Text)
avcLastModifiedDate = lens _avcLastModifiedDate (\ s a -> s{_avcLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
avcEnabled :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcEnabled = lens _avcEnabled (\ s a -> s{_avcEnabled = a})

-- | If the channel is registered with a token key for authentication.
avcHasTokenKey :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcHasTokenKey = lens _avcHasTokenKey (\ s a -> s{_avcHasTokenKey = a})

-- | The default authentication method used for APNs.
avcDefaultAuthenticationMethod :: Lens' APNSVoipChannelResponse (Maybe Text)
avcDefaultAuthenticationMethod = lens _avcDefaultAuthenticationMethod (\ s a -> s{_avcDefaultAuthenticationMethod = a})

-- | Is this channel archived
avcIsArchived :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcIsArchived = lens _avcIsArchived (\ s a -> s{_avcIsArchived = a})

-- | Application id
avcApplicationId :: Lens' APNSVoipChannelResponse (Maybe Text)
avcApplicationId = lens _avcApplicationId (\ s a -> s{_avcApplicationId = a})

-- | Version of channel
avcVersion :: Lens' APNSVoipChannelResponse (Maybe Int)
avcVersion = lens _avcVersion (\ s a -> s{_avcVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
avcId :: Lens' APNSVoipChannelResponse (Maybe Text)
avcId = lens _avcId (\ s a -> s{_avcId = a})

-- | When was this segment created
avcCreationDate :: Lens' APNSVoipChannelResponse (Maybe Text)
avcCreationDate = lens _avcCreationDate (\ s a -> s{_avcCreationDate = a})

-- | Who made the last change
avcLastModifiedBy :: Lens' APNSVoipChannelResponse (Maybe Text)
avcLastModifiedBy = lens _avcLastModifiedBy (\ s a -> s{_avcLastModifiedBy = a})

-- | If the channel is registered with a credential for authentication.
avcHasCredential :: Lens' APNSVoipChannelResponse (Maybe Bool)
avcHasCredential = lens _avcHasCredential (\ s a -> s{_avcHasCredential = a})

instance FromJSON APNSVoipChannelResponse where
        parseJSON
          = withObject "APNSVoipChannelResponse"
              (\ x ->
                 APNSVoipChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "HasTokenKey")
                     <*> (x .:? "DefaultAuthenticationMethod")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable APNSVoipChannelResponse where

instance NFData APNSVoipChannelResponse where

-- | Apple VoIP Developer Push Notification Service channel definition.
--
-- /See:/ 'apnsVoipSandboxChannelRequest' smart constructor.
data APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest'
  { _avscrTokenKey                    :: !(Maybe Text)
  , _avscrPrivateKey                  :: !(Maybe Text)
  , _avscrEnabled                     :: !(Maybe Bool)
  , _avscrTeamId                      :: !(Maybe Text)
  , _avscrBundleId                    :: !(Maybe Text)
  , _avscrDefaultAuthenticationMethod :: !(Maybe Text)
  , _avscrCertificate                 :: !(Maybe Text)
  , _avscrTokenKeyId                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSVoipSandboxChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avscrTokenKey' - The token key used for APNs Tokens.
--
-- * 'avscrPrivateKey' - The certificate private key.
--
-- * 'avscrEnabled' - If the channel is enabled for sending messages.
--
-- * 'avscrTeamId' - The team id used for APNs Tokens.
--
-- * 'avscrBundleId' - The bundle id used for APNs Tokens.
--
-- * 'avscrDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'avscrCertificate' - The distribution certificate from Apple.
--
-- * 'avscrTokenKeyId' - The token key used for APNs Tokens.
apnsVoipSandboxChannelRequest
    :: APNSVoipSandboxChannelRequest
apnsVoipSandboxChannelRequest =
  APNSVoipSandboxChannelRequest'
    { _avscrTokenKey = Nothing
    , _avscrPrivateKey = Nothing
    , _avscrEnabled = Nothing
    , _avscrTeamId = Nothing
    , _avscrBundleId = Nothing
    , _avscrDefaultAuthenticationMethod = Nothing
    , _avscrCertificate = Nothing
    , _avscrTokenKeyId = Nothing
    }


-- | The token key used for APNs Tokens.
avscrTokenKey :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTokenKey = lens _avscrTokenKey (\ s a -> s{_avscrTokenKey = a})

-- | The certificate private key.
avscrPrivateKey :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrPrivateKey = lens _avscrPrivateKey (\ s a -> s{_avscrPrivateKey = a})

-- | If the channel is enabled for sending messages.
avscrEnabled :: Lens' APNSVoipSandboxChannelRequest (Maybe Bool)
avscrEnabled = lens _avscrEnabled (\ s a -> s{_avscrEnabled = a})

-- | The team id used for APNs Tokens.
avscrTeamId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTeamId = lens _avscrTeamId (\ s a -> s{_avscrTeamId = a})

-- | The bundle id used for APNs Tokens.
avscrBundleId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrBundleId = lens _avscrBundleId (\ s a -> s{_avscrBundleId = a})

-- | The default authentication method used for APNs.
avscrDefaultAuthenticationMethod :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrDefaultAuthenticationMethod = lens _avscrDefaultAuthenticationMethod (\ s a -> s{_avscrDefaultAuthenticationMethod = a})

-- | The distribution certificate from Apple.
avscrCertificate :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrCertificate = lens _avscrCertificate (\ s a -> s{_avscrCertificate = a})

-- | The token key used for APNs Tokens.
avscrTokenKeyId :: Lens' APNSVoipSandboxChannelRequest (Maybe Text)
avscrTokenKeyId = lens _avscrTokenKeyId (\ s a -> s{_avscrTokenKeyId = a})

instance Hashable APNSVoipSandboxChannelRequest where

instance NFData APNSVoipSandboxChannelRequest where

instance ToJSON APNSVoipSandboxChannelRequest where
        toJSON APNSVoipSandboxChannelRequest'{..}
          = object
              (catMaybes
                 [("TokenKey" .=) <$> _avscrTokenKey,
                  ("PrivateKey" .=) <$> _avscrPrivateKey,
                  ("Enabled" .=) <$> _avscrEnabled,
                  ("TeamId" .=) <$> _avscrTeamId,
                  ("BundleId" .=) <$> _avscrBundleId,
                  ("DefaultAuthenticationMethod" .=) <$>
                    _avscrDefaultAuthenticationMethod,
                  ("Certificate" .=) <$> _avscrCertificate,
                  ("TokenKeyId" .=) <$> _avscrTokenKeyId])

-- | Apple VoIP Developer Push Notification Service channel definition.
--
-- /See:/ 'apnsVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { _avscPlatform                    :: !(Maybe Text)
  , _avscLastModifiedDate            :: !(Maybe Text)
  , _avscEnabled                     :: !(Maybe Bool)
  , _avscHasTokenKey                 :: !(Maybe Bool)
  , _avscDefaultAuthenticationMethod :: !(Maybe Text)
  , _avscIsArchived                  :: !(Maybe Bool)
  , _avscApplicationId               :: !(Maybe Text)
  , _avscVersion                     :: !(Maybe Int)
  , _avscId                          :: !(Maybe Text)
  , _avscCreationDate                :: !(Maybe Text)
  , _avscLastModifiedBy              :: !(Maybe Text)
  , _avscHasCredential               :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'APNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avscPlatform' - The platform type. Will be APNS.
--
-- * 'avscLastModifiedDate' - Last date this was updated
--
-- * 'avscEnabled' - If the channel is enabled for sending messages.
--
-- * 'avscHasTokenKey' - If the channel is registered with a token key for authentication.
--
-- * 'avscDefaultAuthenticationMethod' - The default authentication method used for APNs.
--
-- * 'avscIsArchived' - Is this channel archived
--
-- * 'avscApplicationId' - Application id
--
-- * 'avscVersion' - Version of channel
--
-- * 'avscId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'avscCreationDate' - When was this segment created
--
-- * 'avscLastModifiedBy' - Who made the last change
--
-- * 'avscHasCredential' - If the channel is registered with a credential for authentication.
apnsVoipSandboxChannelResponse
    :: APNSVoipSandboxChannelResponse
apnsVoipSandboxChannelResponse =
  APNSVoipSandboxChannelResponse'
    { _avscPlatform = Nothing
    , _avscLastModifiedDate = Nothing
    , _avscEnabled = Nothing
    , _avscHasTokenKey = Nothing
    , _avscDefaultAuthenticationMethod = Nothing
    , _avscIsArchived = Nothing
    , _avscApplicationId = Nothing
    , _avscVersion = Nothing
    , _avscId = Nothing
    , _avscCreationDate = Nothing
    , _avscLastModifiedBy = Nothing
    , _avscHasCredential = Nothing
    }


-- | The platform type. Will be APNS.
avscPlatform :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscPlatform = lens _avscPlatform (\ s a -> s{_avscPlatform = a})

-- | Last date this was updated
avscLastModifiedDate :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscLastModifiedDate = lens _avscLastModifiedDate (\ s a -> s{_avscLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
avscEnabled :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscEnabled = lens _avscEnabled (\ s a -> s{_avscEnabled = a})

-- | If the channel is registered with a token key for authentication.
avscHasTokenKey :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscHasTokenKey = lens _avscHasTokenKey (\ s a -> s{_avscHasTokenKey = a})

-- | The default authentication method used for APNs.
avscDefaultAuthenticationMethod :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscDefaultAuthenticationMethod = lens _avscDefaultAuthenticationMethod (\ s a -> s{_avscDefaultAuthenticationMethod = a})

-- | Is this channel archived
avscIsArchived :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscIsArchived = lens _avscIsArchived (\ s a -> s{_avscIsArchived = a})

-- | Application id
avscApplicationId :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscApplicationId = lens _avscApplicationId (\ s a -> s{_avscApplicationId = a})

-- | Version of channel
avscVersion :: Lens' APNSVoipSandboxChannelResponse (Maybe Int)
avscVersion = lens _avscVersion (\ s a -> s{_avscVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
avscId :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscId = lens _avscId (\ s a -> s{_avscId = a})

-- | When was this segment created
avscCreationDate :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscCreationDate = lens _avscCreationDate (\ s a -> s{_avscCreationDate = a})

-- | Who made the last change
avscLastModifiedBy :: Lens' APNSVoipSandboxChannelResponse (Maybe Text)
avscLastModifiedBy = lens _avscLastModifiedBy (\ s a -> s{_avscLastModifiedBy = a})

-- | If the channel is registered with a credential for authentication.
avscHasCredential :: Lens' APNSVoipSandboxChannelResponse (Maybe Bool)
avscHasCredential = lens _avscHasCredential (\ s a -> s{_avscHasCredential = a})

instance FromJSON APNSVoipSandboxChannelResponse
         where
        parseJSON
          = withObject "APNSVoipSandboxChannelResponse"
              (\ x ->
                 APNSVoipSandboxChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "HasTokenKey")
                     <*> (x .:? "DefaultAuthenticationMethod")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable APNSVoipSandboxChannelResponse
         where

instance NFData APNSVoipSandboxChannelResponse where

-- | Activities for campaign.
--
-- /See:/ 'activitiesResponse' smart constructor.
newtype ActivitiesResponse = ActivitiesResponse'
  { _aItem :: Maybe [ActivityResponse]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aItem' - List of campaign activities
activitiesResponse
    :: ActivitiesResponse
activitiesResponse = ActivitiesResponse' {_aItem = Nothing}


-- | List of campaign activities
aItem :: Lens' ActivitiesResponse [ActivityResponse]
aItem = lens _aItem (\ s a -> s{_aItem = a}) . _Default . _Coerce

instance FromJSON ActivitiesResponse where
        parseJSON
          = withObject "ActivitiesResponse"
              (\ x ->
                 ActivitiesResponse' <$> (x .:? "Item" .!= mempty))

instance Hashable ActivitiesResponse where

instance NFData ActivitiesResponse where

-- | Activity definition
--
-- /See:/ 'activityResponse' smart constructor.
data ActivityResponse = ActivityResponse'
  { _aState                   :: !(Maybe Text)
  , _aStart                   :: !(Maybe Text)
  , _aCampaignId              :: !(Maybe Text)
  , _aTimezonesCompletedCount :: !(Maybe Int)
  , _aTimezonesTotalCount     :: !(Maybe Int)
  , _aResult                  :: !(Maybe Text)
  , _aTreatmentId             :: !(Maybe Text)
  , _aSuccessfulEndpointCount :: !(Maybe Int)
  , _aEnd                     :: !(Maybe Text)
  , _aApplicationId           :: !(Maybe Text)
  , _aTotalEndpointCount      :: !(Maybe Int)
  , _aId                      :: !(Maybe Text)
  , _aScheduledStart          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aState' - The state of the activity. Valid values: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, COMPLETED
--
-- * 'aStart' - The actual start time of the activity in ISO 8601 format.
--
-- * 'aCampaignId' - The ID of the campaign to which the activity applies.
--
-- * 'aTimezonesCompletedCount' - The total number of timezones completed.
--
-- * 'aTimezonesTotalCount' - The total number of unique timezones present in the segment.
--
-- * 'aResult' - Indicates whether the activity succeeded. Valid values: SUCCESS, FAIL
--
-- * 'aTreatmentId' - The ID of a variation of the campaign used for A/B testing.
--
-- * 'aSuccessfulEndpointCount' - The total number of endpoints to which the campaign successfully delivered messages.
--
-- * 'aEnd' - The actual time the activity was marked CANCELLED or COMPLETED. Provided in ISO 8601 format.
--
-- * 'aApplicationId' - The ID of the application to which the campaign applies.
--
-- * 'aTotalEndpointCount' - The total number of endpoints to which the campaign attempts to deliver messages.
--
-- * 'aId' - The unique activity ID.
--
-- * 'aScheduledStart' - The scheduled start time for the activity in ISO 8601 format.
activityResponse
    :: ActivityResponse
activityResponse =
  ActivityResponse'
    { _aState = Nothing
    , _aStart = Nothing
    , _aCampaignId = Nothing
    , _aTimezonesCompletedCount = Nothing
    , _aTimezonesTotalCount = Nothing
    , _aResult = Nothing
    , _aTreatmentId = Nothing
    , _aSuccessfulEndpointCount = Nothing
    , _aEnd = Nothing
    , _aApplicationId = Nothing
    , _aTotalEndpointCount = Nothing
    , _aId = Nothing
    , _aScheduledStart = Nothing
    }


-- | The state of the activity. Valid values: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, COMPLETED
aState :: Lens' ActivityResponse (Maybe Text)
aState = lens _aState (\ s a -> s{_aState = a})

-- | The actual start time of the activity in ISO 8601 format.
aStart :: Lens' ActivityResponse (Maybe Text)
aStart = lens _aStart (\ s a -> s{_aStart = a})

-- | The ID of the campaign to which the activity applies.
aCampaignId :: Lens' ActivityResponse (Maybe Text)
aCampaignId = lens _aCampaignId (\ s a -> s{_aCampaignId = a})

-- | The total number of timezones completed.
aTimezonesCompletedCount :: Lens' ActivityResponse (Maybe Int)
aTimezonesCompletedCount = lens _aTimezonesCompletedCount (\ s a -> s{_aTimezonesCompletedCount = a})

-- | The total number of unique timezones present in the segment.
aTimezonesTotalCount :: Lens' ActivityResponse (Maybe Int)
aTimezonesTotalCount = lens _aTimezonesTotalCount (\ s a -> s{_aTimezonesTotalCount = a})

-- | Indicates whether the activity succeeded. Valid values: SUCCESS, FAIL
aResult :: Lens' ActivityResponse (Maybe Text)
aResult = lens _aResult (\ s a -> s{_aResult = a})

-- | The ID of a variation of the campaign used for A/B testing.
aTreatmentId :: Lens' ActivityResponse (Maybe Text)
aTreatmentId = lens _aTreatmentId (\ s a -> s{_aTreatmentId = a})

-- | The total number of endpoints to which the campaign successfully delivered messages.
aSuccessfulEndpointCount :: Lens' ActivityResponse (Maybe Int)
aSuccessfulEndpointCount = lens _aSuccessfulEndpointCount (\ s a -> s{_aSuccessfulEndpointCount = a})

-- | The actual time the activity was marked CANCELLED or COMPLETED. Provided in ISO 8601 format.
aEnd :: Lens' ActivityResponse (Maybe Text)
aEnd = lens _aEnd (\ s a -> s{_aEnd = a})

-- | The ID of the application to which the campaign applies.
aApplicationId :: Lens' ActivityResponse (Maybe Text)
aApplicationId = lens _aApplicationId (\ s a -> s{_aApplicationId = a})

-- | The total number of endpoints to which the campaign attempts to deliver messages.
aTotalEndpointCount :: Lens' ActivityResponse (Maybe Int)
aTotalEndpointCount = lens _aTotalEndpointCount (\ s a -> s{_aTotalEndpointCount = a})

-- | The unique activity ID.
aId :: Lens' ActivityResponse (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a})

-- | The scheduled start time for the activity in ISO 8601 format.
aScheduledStart :: Lens' ActivityResponse (Maybe Text)
aScheduledStart = lens _aScheduledStart (\ s a -> s{_aScheduledStart = a})

instance FromJSON ActivityResponse where
        parseJSON
          = withObject "ActivityResponse"
              (\ x ->
                 ActivityResponse' <$>
                   (x .:? "State") <*> (x .:? "Start") <*>
                     (x .:? "CampaignId")
                     <*> (x .:? "TimezonesCompletedCount")
                     <*> (x .:? "TimezonesTotalCount")
                     <*> (x .:? "Result")
                     <*> (x .:? "TreatmentId")
                     <*> (x .:? "SuccessfulEndpointCount")
                     <*> (x .:? "End")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "TotalEndpointCount")
                     <*> (x .:? "Id")
                     <*> (x .:? "ScheduledStart"))

instance Hashable ActivityResponse where

instance NFData ActivityResponse where

-- | Address configuration.
--
-- /See:/ 'addressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { _acSubstitutions :: !(Maybe (Map Text [Text]))
  , _acTitleOverride :: !(Maybe Text)
  , _acContext       :: !(Maybe (Map Text Text))
  , _acRawContent    :: !(Maybe Text)
  , _acBodyOverride  :: !(Maybe Text)
  , _acChannelType   :: !(Maybe ChannelType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddressConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSubstitutions' - A map of substitution values for the message to be merged with the DefaultMessage's substitutions. Substitutions on this map take precedence over the all other substitutions.
--
-- * 'acTitleOverride' - Title override. If specified will override default title if applicable.
--
-- * 'acContext' - A map of custom attributes to attributes to be attached to the message for this address. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
--
-- * 'acRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'acBodyOverride' - Body override. If specified will override default body.
--
-- * 'acChannelType' - The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
addressConfiguration
    :: AddressConfiguration
addressConfiguration =
  AddressConfiguration'
    { _acSubstitutions = Nothing
    , _acTitleOverride = Nothing
    , _acContext = Nothing
    , _acRawContent = Nothing
    , _acBodyOverride = Nothing
    , _acChannelType = Nothing
    }


-- | A map of substitution values for the message to be merged with the DefaultMessage's substitutions. Substitutions on this map take precedence over the all other substitutions.
acSubstitutions :: Lens' AddressConfiguration (HashMap Text [Text])
acSubstitutions = lens _acSubstitutions (\ s a -> s{_acSubstitutions = a}) . _Default . _Map

-- | Title override. If specified will override default title if applicable.
acTitleOverride :: Lens' AddressConfiguration (Maybe Text)
acTitleOverride = lens _acTitleOverride (\ s a -> s{_acTitleOverride = a})

-- | A map of custom attributes to attributes to be attached to the message for this address. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
acContext :: Lens' AddressConfiguration (HashMap Text Text)
acContext = lens _acContext (\ s a -> s{_acContext = a}) . _Default . _Map

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
acRawContent :: Lens' AddressConfiguration (Maybe Text)
acRawContent = lens _acRawContent (\ s a -> s{_acRawContent = a})

-- | Body override. If specified will override default body.
acBodyOverride :: Lens' AddressConfiguration (Maybe Text)
acBodyOverride = lens _acBodyOverride (\ s a -> s{_acBodyOverride = a})

-- | The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
acChannelType :: Lens' AddressConfiguration (Maybe ChannelType)
acChannelType = lens _acChannelType (\ s a -> s{_acChannelType = a})

instance Hashable AddressConfiguration where

instance NFData AddressConfiguration where

instance ToJSON AddressConfiguration where
        toJSON AddressConfiguration'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _acSubstitutions,
                  ("TitleOverride" .=) <$> _acTitleOverride,
                  ("Context" .=) <$> _acContext,
                  ("RawContent" .=) <$> _acRawContent,
                  ("BodyOverride" .=) <$> _acBodyOverride,
                  ("ChannelType" .=) <$> _acChannelType])

-- | Application Response.
--
-- /See:/ 'applicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { _appName :: !(Maybe Text)
  , _appId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appName' - The display name of the application.
--
-- * 'appId' - The unique application ID.
applicationResponse
    :: ApplicationResponse
applicationResponse =
  ApplicationResponse' {_appName = Nothing, _appId = Nothing}


-- | The display name of the application.
appName :: Lens' ApplicationResponse (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a})

-- | The unique application ID.
appId :: Lens' ApplicationResponse (Maybe Text)
appId = lens _appId (\ s a -> s{_appId = a})

instance FromJSON ApplicationResponse where
        parseJSON
          = withObject "ApplicationResponse"
              (\ x ->
                 ApplicationResponse' <$>
                   (x .:? "Name") <*> (x .:? "Id"))

instance Hashable ApplicationResponse where

instance NFData ApplicationResponse where

-- | Application settings.
--
-- /See:/ 'applicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
  { _asrLastModifiedDate :: !(Maybe Text)
  , _asrLimits           :: !(Maybe CampaignLimits)
  , _asrQuietTime        :: !(Maybe QuietTime)
  , _asrApplicationId    :: !(Maybe Text)
  , _asrCampaignHook     :: !(Maybe CampaignHook)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationSettingsResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrLastModifiedDate' - The date that the settings were last updated in ISO 8601 format.
--
-- * 'asrLimits' - The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
--
-- * 'asrQuietTime' - The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
--
-- * 'asrApplicationId' - The unique ID for the application.
--
-- * 'asrCampaignHook' - Default campaign hook.
applicationSettingsResource
    :: ApplicationSettingsResource
applicationSettingsResource =
  ApplicationSettingsResource'
    { _asrLastModifiedDate = Nothing
    , _asrLimits = Nothing
    , _asrQuietTime = Nothing
    , _asrApplicationId = Nothing
    , _asrCampaignHook = Nothing
    }


-- | The date that the settings were last updated in ISO 8601 format.
asrLastModifiedDate :: Lens' ApplicationSettingsResource (Maybe Text)
asrLastModifiedDate = lens _asrLastModifiedDate (\ s a -> s{_asrLastModifiedDate = a})

-- | The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
asrLimits :: Lens' ApplicationSettingsResource (Maybe CampaignLimits)
asrLimits = lens _asrLimits (\ s a -> s{_asrLimits = a})

-- | The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
asrQuietTime :: Lens' ApplicationSettingsResource (Maybe QuietTime)
asrQuietTime = lens _asrQuietTime (\ s a -> s{_asrQuietTime = a})

-- | The unique ID for the application.
asrApplicationId :: Lens' ApplicationSettingsResource (Maybe Text)
asrApplicationId = lens _asrApplicationId (\ s a -> s{_asrApplicationId = a})

-- | Default campaign hook.
asrCampaignHook :: Lens' ApplicationSettingsResource (Maybe CampaignHook)
asrCampaignHook = lens _asrCampaignHook (\ s a -> s{_asrCampaignHook = a})

instance FromJSON ApplicationSettingsResource where
        parseJSON
          = withObject "ApplicationSettingsResource"
              (\ x ->
                 ApplicationSettingsResource' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "Limits") <*>
                     (x .:? "QuietTime")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "CampaignHook"))

instance Hashable ApplicationSettingsResource where

instance NFData ApplicationSettingsResource where

-- | Get Applications Result.
--
-- /See:/ 'applicationsResponse' smart constructor.
data ApplicationsResponse = ApplicationsResponse'
  { _appNextToken :: !(Maybe Text)
  , _appItem      :: !(Maybe [ApplicationResponse])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'appItem' - List of applications returned in this page.
applicationsResponse
    :: ApplicationsResponse
applicationsResponse =
  ApplicationsResponse' {_appNextToken = Nothing, _appItem = Nothing}


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
appNextToken :: Lens' ApplicationsResponse (Maybe Text)
appNextToken = lens _appNextToken (\ s a -> s{_appNextToken = a})

-- | List of applications returned in this page.
appItem :: Lens' ApplicationsResponse [ApplicationResponse]
appItem = lens _appItem (\ s a -> s{_appItem = a}) . _Default . _Coerce

instance FromJSON ApplicationsResponse where
        parseJSON
          = withObject "ApplicationsResponse"
              (\ x ->
                 ApplicationsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable ApplicationsResponse where

instance NFData ApplicationsResponse where

-- | Custom attibute dimension
--
-- /See:/ 'attributeDimension' smart constructor.
data AttributeDimension = AttributeDimension'
  { _adValues        :: !(Maybe [Text])
  , _adAttributeType :: !(Maybe AttributeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adValues' - The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
--
-- * 'adAttributeType' - The type of dimension: INCLUSIVE - Endpoints that match the criteria are included in the segment. EXCLUSIVE - Endpoints that match the criteria are excluded from the segment.
attributeDimension
    :: AttributeDimension
attributeDimension =
  AttributeDimension' {_adValues = Nothing, _adAttributeType = Nothing}


-- | The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
adValues :: Lens' AttributeDimension [Text]
adValues = lens _adValues (\ s a -> s{_adValues = a}) . _Default . _Coerce

-- | The type of dimension: INCLUSIVE - Endpoints that match the criteria are included in the segment. EXCLUSIVE - Endpoints that match the criteria are excluded from the segment.
adAttributeType :: Lens' AttributeDimension (Maybe AttributeType)
adAttributeType = lens _adAttributeType (\ s a -> s{_adAttributeType = a})

instance FromJSON AttributeDimension where
        parseJSON
          = withObject "AttributeDimension"
              (\ x ->
                 AttributeDimension' <$>
                   (x .:? "Values" .!= mempty) <*>
                     (x .:? "AttributeType"))

instance Hashable AttributeDimension where

instance NFData AttributeDimension where

instance ToJSON AttributeDimension where
        toJSON AttributeDimension'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _adValues,
                  ("AttributeType" .=) <$> _adAttributeType])

-- | Baidu Cloud Push credentials
--
-- /See:/ 'baiduChannelRequest' smart constructor.
data BaiduChannelRequest = BaiduChannelRequest'
  { _bcrAPIKey    :: !(Maybe Text)
  , _bcrEnabled   :: !(Maybe Bool)
  , _bcrSecretKey :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BaiduChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcrAPIKey' - Platform credential API key from Baidu.
--
-- * 'bcrEnabled' - If the channel is enabled for sending messages.
--
-- * 'bcrSecretKey' - Platform credential Secret key from Baidu.
baiduChannelRequest
    :: BaiduChannelRequest
baiduChannelRequest =
  BaiduChannelRequest'
    {_bcrAPIKey = Nothing, _bcrEnabled = Nothing, _bcrSecretKey = Nothing}


-- | Platform credential API key from Baidu.
bcrAPIKey :: Lens' BaiduChannelRequest (Maybe Text)
bcrAPIKey = lens _bcrAPIKey (\ s a -> s{_bcrAPIKey = a})

-- | If the channel is enabled for sending messages.
bcrEnabled :: Lens' BaiduChannelRequest (Maybe Bool)
bcrEnabled = lens _bcrEnabled (\ s a -> s{_bcrEnabled = a})

-- | Platform credential Secret key from Baidu.
bcrSecretKey :: Lens' BaiduChannelRequest (Maybe Text)
bcrSecretKey = lens _bcrSecretKey (\ s a -> s{_bcrSecretKey = a})

instance Hashable BaiduChannelRequest where

instance NFData BaiduChannelRequest where

instance ToJSON BaiduChannelRequest where
        toJSON BaiduChannelRequest'{..}
          = object
              (catMaybes
                 [("ApiKey" .=) <$> _bcrAPIKey,
                  ("Enabled" .=) <$> _bcrEnabled,
                  ("SecretKey" .=) <$> _bcrSecretKey])

-- | Baidu Cloud Messaging channel definition
--
-- /See:/ 'baiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
  { _bcPlatform         :: !(Maybe Text)
  , _bcLastModifiedDate :: !(Maybe Text)
  , _bcEnabled          :: !(Maybe Bool)
  , _bcCredential       :: !(Maybe Text)
  , _bcIsArchived       :: !(Maybe Bool)
  , _bcApplicationId    :: !(Maybe Text)
  , _bcVersion          :: !(Maybe Int)
  , _bcId               :: !(Maybe Text)
  , _bcCreationDate     :: !(Maybe Text)
  , _bcLastModifiedBy   :: !(Maybe Text)
  , _bcHasCredential    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BaiduChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcPlatform' - The platform type. Will be BAIDU
--
-- * 'bcLastModifiedDate' - Last date this was updated
--
-- * 'bcEnabled' - If the channel is enabled for sending messages.
--
-- * 'bcCredential' - The Baidu API key from Baidu.
--
-- * 'bcIsArchived' - Is this channel archived
--
-- * 'bcApplicationId' - Application id
--
-- * 'bcVersion' - Version of channel
--
-- * 'bcId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'bcCreationDate' - When was this segment created
--
-- * 'bcLastModifiedBy' - Who made the last change
--
-- * 'bcHasCredential' - Indicates whether the channel is configured with Baidu Cloud Push credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with Baidu Cloud Push. Provide your credentials by setting the ApiKey and SecretKey attributes.
baiduChannelResponse
    :: BaiduChannelResponse
baiduChannelResponse =
  BaiduChannelResponse'
    { _bcPlatform = Nothing
    , _bcLastModifiedDate = Nothing
    , _bcEnabled = Nothing
    , _bcCredential = Nothing
    , _bcIsArchived = Nothing
    , _bcApplicationId = Nothing
    , _bcVersion = Nothing
    , _bcId = Nothing
    , _bcCreationDate = Nothing
    , _bcLastModifiedBy = Nothing
    , _bcHasCredential = Nothing
    }


-- | The platform type. Will be BAIDU
bcPlatform :: Lens' BaiduChannelResponse (Maybe Text)
bcPlatform = lens _bcPlatform (\ s a -> s{_bcPlatform = a})

-- | Last date this was updated
bcLastModifiedDate :: Lens' BaiduChannelResponse (Maybe Text)
bcLastModifiedDate = lens _bcLastModifiedDate (\ s a -> s{_bcLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
bcEnabled :: Lens' BaiduChannelResponse (Maybe Bool)
bcEnabled = lens _bcEnabled (\ s a -> s{_bcEnabled = a})

-- | The Baidu API key from Baidu.
bcCredential :: Lens' BaiduChannelResponse (Maybe Text)
bcCredential = lens _bcCredential (\ s a -> s{_bcCredential = a})

-- | Is this channel archived
bcIsArchived :: Lens' BaiduChannelResponse (Maybe Bool)
bcIsArchived = lens _bcIsArchived (\ s a -> s{_bcIsArchived = a})

-- | Application id
bcApplicationId :: Lens' BaiduChannelResponse (Maybe Text)
bcApplicationId = lens _bcApplicationId (\ s a -> s{_bcApplicationId = a})

-- | Version of channel
bcVersion :: Lens' BaiduChannelResponse (Maybe Int)
bcVersion = lens _bcVersion (\ s a -> s{_bcVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
bcId :: Lens' BaiduChannelResponse (Maybe Text)
bcId = lens _bcId (\ s a -> s{_bcId = a})

-- | When was this segment created
bcCreationDate :: Lens' BaiduChannelResponse (Maybe Text)
bcCreationDate = lens _bcCreationDate (\ s a -> s{_bcCreationDate = a})

-- | Who made the last change
bcLastModifiedBy :: Lens' BaiduChannelResponse (Maybe Text)
bcLastModifiedBy = lens _bcLastModifiedBy (\ s a -> s{_bcLastModifiedBy = a})

-- | Indicates whether the channel is configured with Baidu Cloud Push credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with Baidu Cloud Push. Provide your credentials by setting the ApiKey and SecretKey attributes.
bcHasCredential :: Lens' BaiduChannelResponse (Maybe Bool)
bcHasCredential = lens _bcHasCredential (\ s a -> s{_bcHasCredential = a})

instance FromJSON BaiduChannelResponse where
        parseJSON
          = withObject "BaiduChannelResponse"
              (\ x ->
                 BaiduChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "Credential")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable BaiduChannelResponse where

instance NFData BaiduChannelResponse where

-- | Baidu Message.
--
-- /See:/ 'baiduMessage' smart constructor.
data BaiduMessage = BaiduMessage'
  { _bmSubstitutions     :: !(Maybe (Map Text [Text]))
  , _bmSilentPush        :: !(Maybe Bool)
  , _bmImageIconURL      :: !(Maybe Text)
  , _bmRawContent        :: !(Maybe Text)
  , _bmData              :: !(Maybe (Map Text Text))
  , _bmSmallImageIconURL :: !(Maybe Text)
  , _bmBody              :: !(Maybe Text)
  , _bmURL               :: !(Maybe Text)
  , _bmSound             :: !(Maybe Text)
  , _bmAction            :: !(Maybe Action)
  , _bmImageURL          :: !(Maybe Text)
  , _bmTitle             :: !(Maybe Text)
  , _bmIconReference     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BaiduMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'bmSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'bmImageIconURL' - The URL that points to an image used as the large icon to the notification content view.
--
-- * 'bmRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'bmData' - The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
--
-- * 'bmSmallImageIconURL' - The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
--
-- * 'bmBody' - The message body of the notification, the email body or the text message.
--
-- * 'bmURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'bmSound' - Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
--
-- * 'bmAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
--
-- * 'bmImageURL' - The URL that points to an image used in the push notification.
--
-- * 'bmTitle' - The message title that displays above the message on the user's device.
--
-- * 'bmIconReference' - The icon image name of the asset saved in your application.
baiduMessage
    :: BaiduMessage
baiduMessage =
  BaiduMessage'
    { _bmSubstitutions = Nothing
    , _bmSilentPush = Nothing
    , _bmImageIconURL = Nothing
    , _bmRawContent = Nothing
    , _bmData = Nothing
    , _bmSmallImageIconURL = Nothing
    , _bmBody = Nothing
    , _bmURL = Nothing
    , _bmSound = Nothing
    , _bmAction = Nothing
    , _bmImageURL = Nothing
    , _bmTitle = Nothing
    , _bmIconReference = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
bmSubstitutions :: Lens' BaiduMessage (HashMap Text [Text])
bmSubstitutions = lens _bmSubstitutions (\ s a -> s{_bmSubstitutions = a}) . _Default . _Map

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
bmSilentPush :: Lens' BaiduMessage (Maybe Bool)
bmSilentPush = lens _bmSilentPush (\ s a -> s{_bmSilentPush = a})

-- | The URL that points to an image used as the large icon to the notification content view.
bmImageIconURL :: Lens' BaiduMessage (Maybe Text)
bmImageIconURL = lens _bmImageIconURL (\ s a -> s{_bmImageIconURL = a})

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
bmRawContent :: Lens' BaiduMessage (Maybe Text)
bmRawContent = lens _bmRawContent (\ s a -> s{_bmRawContent = a})

-- | The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
bmData :: Lens' BaiduMessage (HashMap Text Text)
bmData = lens _bmData (\ s a -> s{_bmData = a}) . _Default . _Map

-- | The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
bmSmallImageIconURL :: Lens' BaiduMessage (Maybe Text)
bmSmallImageIconURL = lens _bmSmallImageIconURL (\ s a -> s{_bmSmallImageIconURL = a})

-- | The message body of the notification, the email body or the text message.
bmBody :: Lens' BaiduMessage (Maybe Text)
bmBody = lens _bmBody (\ s a -> s{_bmBody = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
bmURL :: Lens' BaiduMessage (Maybe Text)
bmURL = lens _bmURL (\ s a -> s{_bmURL = a})

-- | Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
bmSound :: Lens' BaiduMessage (Maybe Text)
bmSound = lens _bmSound (\ s a -> s{_bmSound = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
bmAction :: Lens' BaiduMessage (Maybe Action)
bmAction = lens _bmAction (\ s a -> s{_bmAction = a})

-- | The URL that points to an image used in the push notification.
bmImageURL :: Lens' BaiduMessage (Maybe Text)
bmImageURL = lens _bmImageURL (\ s a -> s{_bmImageURL = a})

-- | The message title that displays above the message on the user's device.
bmTitle :: Lens' BaiduMessage (Maybe Text)
bmTitle = lens _bmTitle (\ s a -> s{_bmTitle = a})

-- | The icon image name of the asset saved in your application.
bmIconReference :: Lens' BaiduMessage (Maybe Text)
bmIconReference = lens _bmIconReference (\ s a -> s{_bmIconReference = a})

instance Hashable BaiduMessage where

instance NFData BaiduMessage where

instance ToJSON BaiduMessage where
        toJSON BaiduMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _bmSubstitutions,
                  ("SilentPush" .=) <$> _bmSilentPush,
                  ("ImageIconUrl" .=) <$> _bmImageIconURL,
                  ("RawContent" .=) <$> _bmRawContent,
                  ("Data" .=) <$> _bmData,
                  ("SmallImageIconUrl" .=) <$> _bmSmallImageIconURL,
                  ("Body" .=) <$> _bmBody, ("Url" .=) <$> _bmURL,
                  ("Sound" .=) <$> _bmSound,
                  ("Action" .=) <$> _bmAction,
                  ("ImageUrl" .=) <$> _bmImageURL,
                  ("Title" .=) <$> _bmTitle,
                  ("IconReference" .=) <$> _bmIconReference])

-- | The email message configuration.
--
-- /See:/ 'campaignEmailMessage' smart constructor.
data CampaignEmailMessage = CampaignEmailMessage'
  { _cemBody        :: !(Maybe Text)
  , _cemFromAddress :: !(Maybe Text)
  , _cemHTMLBody    :: !(Maybe Text)
  , _cemTitle       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignEmailMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cemBody' - The email text body.
--
-- * 'cemFromAddress' - The email address used to send the email from. Defaults to use FromAddress specified in the Email Channel.
--
-- * 'cemHTMLBody' - The email html body.
--
-- * 'cemTitle' - The email title (Or subject).
campaignEmailMessage
    :: CampaignEmailMessage
campaignEmailMessage =
  CampaignEmailMessage'
    { _cemBody = Nothing
    , _cemFromAddress = Nothing
    , _cemHTMLBody = Nothing
    , _cemTitle = Nothing
    }


-- | The email text body.
cemBody :: Lens' CampaignEmailMessage (Maybe Text)
cemBody = lens _cemBody (\ s a -> s{_cemBody = a})

-- | The email address used to send the email from. Defaults to use FromAddress specified in the Email Channel.
cemFromAddress :: Lens' CampaignEmailMessage (Maybe Text)
cemFromAddress = lens _cemFromAddress (\ s a -> s{_cemFromAddress = a})

-- | The email html body.
cemHTMLBody :: Lens' CampaignEmailMessage (Maybe Text)
cemHTMLBody = lens _cemHTMLBody (\ s a -> s{_cemHTMLBody = a})

-- | The email title (Or subject).
cemTitle :: Lens' CampaignEmailMessage (Maybe Text)
cemTitle = lens _cemTitle (\ s a -> s{_cemTitle = a})

instance FromJSON CampaignEmailMessage where
        parseJSON
          = withObject "CampaignEmailMessage"
              (\ x ->
                 CampaignEmailMessage' <$>
                   (x .:? "Body") <*> (x .:? "FromAddress") <*>
                     (x .:? "HtmlBody")
                     <*> (x .:? "Title"))

instance Hashable CampaignEmailMessage where

instance NFData CampaignEmailMessage where

instance ToJSON CampaignEmailMessage where
        toJSON CampaignEmailMessage'{..}
          = object
              (catMaybes
                 [("Body" .=) <$> _cemBody,
                  ("FromAddress" .=) <$> _cemFromAddress,
                  ("HtmlBody" .=) <$> _cemHTMLBody,
                  ("Title" .=) <$> _cemTitle])

-- | /See:/ 'campaignHook' smart constructor.
data CampaignHook = CampaignHook'
  { _chLambdaFunctionName :: !(Maybe Text)
  , _chMode               :: !(Maybe Mode)
  , _chWebURL             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignHook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chLambdaFunctionName' - Lambda function name or arn to be called for delivery
--
-- * 'chMode' - What mode Lambda should be invoked in.
--
-- * 'chWebURL' - Web URL to call for hook. If the URL has authentication specified it will be added as authentication to the request
campaignHook
    :: CampaignHook
campaignHook =
  CampaignHook'
    {_chLambdaFunctionName = Nothing, _chMode = Nothing, _chWebURL = Nothing}


-- | Lambda function name or arn to be called for delivery
chLambdaFunctionName :: Lens' CampaignHook (Maybe Text)
chLambdaFunctionName = lens _chLambdaFunctionName (\ s a -> s{_chLambdaFunctionName = a})

-- | What mode Lambda should be invoked in.
chMode :: Lens' CampaignHook (Maybe Mode)
chMode = lens _chMode (\ s a -> s{_chMode = a})

-- | Web URL to call for hook. If the URL has authentication specified it will be added as authentication to the request
chWebURL :: Lens' CampaignHook (Maybe Text)
chWebURL = lens _chWebURL (\ s a -> s{_chWebURL = a})

instance FromJSON CampaignHook where
        parseJSON
          = withObject "CampaignHook"
              (\ x ->
                 CampaignHook' <$>
                   (x .:? "LambdaFunctionName") <*> (x .:? "Mode") <*>
                     (x .:? "WebUrl"))

instance Hashable CampaignHook where

instance NFData CampaignHook where

instance ToJSON CampaignHook where
        toJSON CampaignHook'{..}
          = object
              (catMaybes
                 [("LambdaFunctionName" .=) <$> _chLambdaFunctionName,
                  ("Mode" .=) <$> _chMode,
                  ("WebUrl" .=) <$> _chWebURL])

-- | Campaign Limits are used to limit the number of messages that can be sent to a user.
--
-- /See:/ 'campaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { _clMessagesPerSecond :: !(Maybe Int)
  , _clDaily             :: !(Maybe Int)
  , _clTotal             :: !(Maybe Int)
  , _clMaximumDuration   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clMessagesPerSecond' - The number of messages that the campaign can send per second. The minimum value is 50, and the maximum is 20000.
--
-- * 'clDaily' - The maximum number of messages that the campaign can send daily.
--
-- * 'clTotal' - The maximum total number of messages that the campaign can send.
--
-- * 'clMaximumDuration' - The length of time (in seconds) that the campaign can run before it ends and message deliveries stop. This duration begins at the scheduled start time for the campaign. The minimum value is 60.
campaignLimits
    :: CampaignLimits
campaignLimits =
  CampaignLimits'
    { _clMessagesPerSecond = Nothing
    , _clDaily = Nothing
    , _clTotal = Nothing
    , _clMaximumDuration = Nothing
    }


-- | The number of messages that the campaign can send per second. The minimum value is 50, and the maximum is 20000.
clMessagesPerSecond :: Lens' CampaignLimits (Maybe Int)
clMessagesPerSecond = lens _clMessagesPerSecond (\ s a -> s{_clMessagesPerSecond = a})

-- | The maximum number of messages that the campaign can send daily.
clDaily :: Lens' CampaignLimits (Maybe Int)
clDaily = lens _clDaily (\ s a -> s{_clDaily = a})

-- | The maximum total number of messages that the campaign can send.
clTotal :: Lens' CampaignLimits (Maybe Int)
clTotal = lens _clTotal (\ s a -> s{_clTotal = a})

-- | The length of time (in seconds) that the campaign can run before it ends and message deliveries stop. This duration begins at the scheduled start time for the campaign. The minimum value is 60.
clMaximumDuration :: Lens' CampaignLimits (Maybe Int)
clMaximumDuration = lens _clMaximumDuration (\ s a -> s{_clMaximumDuration = a})

instance FromJSON CampaignLimits where
        parseJSON
          = withObject "CampaignLimits"
              (\ x ->
                 CampaignLimits' <$>
                   (x .:? "MessagesPerSecond") <*> (x .:? "Daily") <*>
                     (x .:? "Total")
                     <*> (x .:? "MaximumDuration"))

instance Hashable CampaignLimits where

instance NFData CampaignLimits where

instance ToJSON CampaignLimits where
        toJSON CampaignLimits'{..}
          = object
              (catMaybes
                 [("MessagesPerSecond" .=) <$> _clMessagesPerSecond,
                  ("Daily" .=) <$> _clDaily, ("Total" .=) <$> _clTotal,
                  ("MaximumDuration" .=) <$> _clMaximumDuration])

-- | Campaign definition
--
-- /See:/ 'campaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { _cState                :: !(Maybe CampaignState)
  , _cLastModifiedDate     :: !(Maybe Text)
  , _cSchedule             :: !(Maybe Schedule)
  , _cHook                 :: !(Maybe CampaignHook)
  , _cTreatmentName        :: !(Maybe Text)
  , _cLimits               :: !(Maybe CampaignLimits)
  , _cIsPaused             :: !(Maybe Bool)
  , _cDefaultState         :: !(Maybe CampaignState)
  , _cApplicationId        :: !(Maybe Text)
  , _cName                 :: !(Maybe Text)
  , _cVersion              :: !(Maybe Int)
  , _cHoldoutPercent       :: !(Maybe Int)
  , _cTreatmentDescription :: !(Maybe Text)
  , _cId                   :: !(Maybe Text)
  , _cCreationDate         :: !(Maybe Text)
  , _cMessageConfiguration :: !(Maybe MessageConfiguration)
  , _cDescription          :: !(Maybe Text)
  , _cSegmentId            :: !(Maybe Text)
  , _cAdditionalTreatments :: !(Maybe [TreatmentResource])
  , _cSegmentVersion       :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - The campaign status. An A/B test campaign will have a status of COMPLETED only when all treatments have a status of COMPLETED.
--
-- * 'cLastModifiedDate' - The date the campaign was last updated in ISO 8601 format.
--
-- * 'cSchedule' - The campaign schedule.
--
-- * 'cHook' - Campaign hook information.
--
-- * 'cTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'cLimits' - The campaign limits settings.
--
-- * 'cIsPaused' - Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
--
-- * 'cDefaultState' - The status of the campaign's default treatment. Only present for A/B test campaigns.
--
-- * 'cApplicationId' - The ID of the application to which the campaign applies.
--
-- * 'cName' - The custom name of the campaign.
--
-- * 'cVersion' - The campaign version number.
--
-- * 'cHoldoutPercent' - The allocated percentage of end users who will not receive messages from this campaign.
--
-- * 'cTreatmentDescription' - A custom description for the treatment.
--
-- * 'cId' - The unique campaign ID.
--
-- * 'cCreationDate' - The date the campaign was created in ISO 8601 format.
--
-- * 'cMessageConfiguration' - The message configuration settings.
--
-- * 'cDescription' - A description of the campaign.
--
-- * 'cSegmentId' - The ID of the segment to which the campaign sends messages.
--
-- * 'cAdditionalTreatments' - Treatments that are defined in addition to the default treatment.
--
-- * 'cSegmentVersion' - The version of the segment to which the campaign sends messages.
campaignResponse
    :: CampaignResponse
campaignResponse =
  CampaignResponse'
    { _cState = Nothing
    , _cLastModifiedDate = Nothing
    , _cSchedule = Nothing
    , _cHook = Nothing
    , _cTreatmentName = Nothing
    , _cLimits = Nothing
    , _cIsPaused = Nothing
    , _cDefaultState = Nothing
    , _cApplicationId = Nothing
    , _cName = Nothing
    , _cVersion = Nothing
    , _cHoldoutPercent = Nothing
    , _cTreatmentDescription = Nothing
    , _cId = Nothing
    , _cCreationDate = Nothing
    , _cMessageConfiguration = Nothing
    , _cDescription = Nothing
    , _cSegmentId = Nothing
    , _cAdditionalTreatments = Nothing
    , _cSegmentVersion = Nothing
    }


-- | The campaign status. An A/B test campaign will have a status of COMPLETED only when all treatments have a status of COMPLETED.
cState :: Lens' CampaignResponse (Maybe CampaignState)
cState = lens _cState (\ s a -> s{_cState = a})

-- | The date the campaign was last updated in ISO 8601 format.
cLastModifiedDate :: Lens' CampaignResponse (Maybe Text)
cLastModifiedDate = lens _cLastModifiedDate (\ s a -> s{_cLastModifiedDate = a})

-- | The campaign schedule.
cSchedule :: Lens' CampaignResponse (Maybe Schedule)
cSchedule = lens _cSchedule (\ s a -> s{_cSchedule = a})

-- | Campaign hook information.
cHook :: Lens' CampaignResponse (Maybe CampaignHook)
cHook = lens _cHook (\ s a -> s{_cHook = a})

-- | The custom name of a variation of the campaign used for A/B testing.
cTreatmentName :: Lens' CampaignResponse (Maybe Text)
cTreatmentName = lens _cTreatmentName (\ s a -> s{_cTreatmentName = a})

-- | The campaign limits settings.
cLimits :: Lens' CampaignResponse (Maybe CampaignLimits)
cLimits = lens _cLimits (\ s a -> s{_cLimits = a})

-- | Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
cIsPaused :: Lens' CampaignResponse (Maybe Bool)
cIsPaused = lens _cIsPaused (\ s a -> s{_cIsPaused = a})

-- | The status of the campaign's default treatment. Only present for A/B test campaigns.
cDefaultState :: Lens' CampaignResponse (Maybe CampaignState)
cDefaultState = lens _cDefaultState (\ s a -> s{_cDefaultState = a})

-- | The ID of the application to which the campaign applies.
cApplicationId :: Lens' CampaignResponse (Maybe Text)
cApplicationId = lens _cApplicationId (\ s a -> s{_cApplicationId = a})

-- | The custom name of the campaign.
cName :: Lens' CampaignResponse (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The campaign version number.
cVersion :: Lens' CampaignResponse (Maybe Int)
cVersion = lens _cVersion (\ s a -> s{_cVersion = a})

-- | The allocated percentage of end users who will not receive messages from this campaign.
cHoldoutPercent :: Lens' CampaignResponse (Maybe Int)
cHoldoutPercent = lens _cHoldoutPercent (\ s a -> s{_cHoldoutPercent = a})

-- | A custom description for the treatment.
cTreatmentDescription :: Lens' CampaignResponse (Maybe Text)
cTreatmentDescription = lens _cTreatmentDescription (\ s a -> s{_cTreatmentDescription = a})

-- | The unique campaign ID.
cId :: Lens' CampaignResponse (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | The date the campaign was created in ISO 8601 format.
cCreationDate :: Lens' CampaignResponse (Maybe Text)
cCreationDate = lens _cCreationDate (\ s a -> s{_cCreationDate = a})

-- | The message configuration settings.
cMessageConfiguration :: Lens' CampaignResponse (Maybe MessageConfiguration)
cMessageConfiguration = lens _cMessageConfiguration (\ s a -> s{_cMessageConfiguration = a})

-- | A description of the campaign.
cDescription :: Lens' CampaignResponse (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | The ID of the segment to which the campaign sends messages.
cSegmentId :: Lens' CampaignResponse (Maybe Text)
cSegmentId = lens _cSegmentId (\ s a -> s{_cSegmentId = a})

-- | Treatments that are defined in addition to the default treatment.
cAdditionalTreatments :: Lens' CampaignResponse [TreatmentResource]
cAdditionalTreatments = lens _cAdditionalTreatments (\ s a -> s{_cAdditionalTreatments = a}) . _Default . _Coerce

-- | The version of the segment to which the campaign sends messages.
cSegmentVersion :: Lens' CampaignResponse (Maybe Int)
cSegmentVersion = lens _cSegmentVersion (\ s a -> s{_cSegmentVersion = a})

instance FromJSON CampaignResponse where
        parseJSON
          = withObject "CampaignResponse"
              (\ x ->
                 CampaignResponse' <$>
                   (x .:? "State") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Schedule")
                     <*> (x .:? "Hook")
                     <*> (x .:? "TreatmentName")
                     <*> (x .:? "Limits")
                     <*> (x .:? "IsPaused")
                     <*> (x .:? "DefaultState")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "HoldoutPercent")
                     <*> (x .:? "TreatmentDescription")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "MessageConfiguration")
                     <*> (x .:? "Description")
                     <*> (x .:? "SegmentId")
                     <*> (x .:? "AdditionalTreatments" .!= mempty)
                     <*> (x .:? "SegmentVersion"))

instance Hashable CampaignResponse where

instance NFData CampaignResponse where

-- | SMS message configuration.
--
-- /See:/ 'campaignSmsMessage' smart constructor.
data CampaignSmsMessage = CampaignSmsMessage'
  { _csmBody        :: !(Maybe Text)
  , _csmMessageType :: !(Maybe MessageType)
  , _csmSenderId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignSmsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmBody' - The SMS text body.
--
-- * 'csmMessageType' - Is this is a transactional SMS message, otherwise a promotional message.
--
-- * 'csmSenderId' - Sender ID of sent message.
campaignSmsMessage
    :: CampaignSmsMessage
campaignSmsMessage =
  CampaignSmsMessage'
    {_csmBody = Nothing, _csmMessageType = Nothing, _csmSenderId = Nothing}


-- | The SMS text body.
csmBody :: Lens' CampaignSmsMessage (Maybe Text)
csmBody = lens _csmBody (\ s a -> s{_csmBody = a})

-- | Is this is a transactional SMS message, otherwise a promotional message.
csmMessageType :: Lens' CampaignSmsMessage (Maybe MessageType)
csmMessageType = lens _csmMessageType (\ s a -> s{_csmMessageType = a})

-- | Sender ID of sent message.
csmSenderId :: Lens' CampaignSmsMessage (Maybe Text)
csmSenderId = lens _csmSenderId (\ s a -> s{_csmSenderId = a})

instance FromJSON CampaignSmsMessage where
        parseJSON
          = withObject "CampaignSmsMessage"
              (\ x ->
                 CampaignSmsMessage' <$>
                   (x .:? "Body") <*> (x .:? "MessageType") <*>
                     (x .:? "SenderId"))

instance Hashable CampaignSmsMessage where

instance NFData CampaignSmsMessage where

instance ToJSON CampaignSmsMessage where
        toJSON CampaignSmsMessage'{..}
          = object
              (catMaybes
                 [("Body" .=) <$> _csmBody,
                  ("MessageType" .=) <$> _csmMessageType,
                  ("SenderId" .=) <$> _csmSenderId])

-- | State of the Campaign
--
-- /See:/ 'campaignState' smart constructor.
newtype CampaignState = CampaignState'
  { _csCampaignStatus :: Maybe CampaignStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCampaignStatus' - The status of the campaign, or the status of a treatment that belongs to an A/B test campaign. Valid values: SCHEDULED, EXECUTING, PENDING_NEXT_RUN, COMPLETED, PAUSED
campaignState
    :: CampaignState
campaignState = CampaignState' {_csCampaignStatus = Nothing}


-- | The status of the campaign, or the status of a treatment that belongs to an A/B test campaign. Valid values: SCHEDULED, EXECUTING, PENDING_NEXT_RUN, COMPLETED, PAUSED
csCampaignStatus :: Lens' CampaignState (Maybe CampaignStatus)
csCampaignStatus = lens _csCampaignStatus (\ s a -> s{_csCampaignStatus = a})

instance FromJSON CampaignState where
        parseJSON
          = withObject "CampaignState"
              (\ x -> CampaignState' <$> (x .:? "CampaignStatus"))

instance Hashable CampaignState where

instance NFData CampaignState where

-- | List of available campaigns.
--
-- /See:/ 'campaignsResponse' smart constructor.
data CampaignsResponse = CampaignsResponse'
  { _cNextToken :: !(Maybe Text)
  , _cItem      :: !(Maybe [CampaignResponse])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'cItem' - A list of campaigns.
campaignsResponse
    :: CampaignsResponse
campaignsResponse = CampaignsResponse' {_cNextToken = Nothing, _cItem = Nothing}


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
cNextToken :: Lens' CampaignsResponse (Maybe Text)
cNextToken = lens _cNextToken (\ s a -> s{_cNextToken = a})

-- | A list of campaigns.
cItem :: Lens' CampaignsResponse [CampaignResponse]
cItem = lens _cItem (\ s a -> s{_cItem = a}) . _Default . _Coerce

instance FromJSON CampaignsResponse where
        parseJSON
          = withObject "CampaignsResponse"
              (\ x ->
                 CampaignsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable CampaignsResponse where

instance NFData CampaignsResponse where

-- | Application Request.
--
-- /See:/ 'createApplicationRequest' smart constructor.
newtype CreateApplicationRequest = CreateApplicationRequest'
  { _carName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplicationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carName' - The display name of the application. Used in the Amazon Pinpoint console.
createApplicationRequest
    :: CreateApplicationRequest
createApplicationRequest = CreateApplicationRequest' {_carName = Nothing}


-- | The display name of the application. Used in the Amazon Pinpoint console.
carName :: Lens' CreateApplicationRequest (Maybe Text)
carName = lens _carName (\ s a -> s{_carName = a})

instance Hashable CreateApplicationRequest where

instance NFData CreateApplicationRequest where

instance ToJSON CreateApplicationRequest where
        toJSON CreateApplicationRequest'{..}
          = object (catMaybes [("Name" .=) <$> _carName])

-- | Default Message across push notification, email, and sms.
--
-- /See:/ 'defaultMessage' smart constructor.
data DefaultMessage = DefaultMessage'
  { _dmSubstitutions :: !(Maybe (Map Text [Text]))
  , _dmBody          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefaultMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'dmBody' - The message body of the notification, the email body or the text message.
defaultMessage
    :: DefaultMessage
defaultMessage = DefaultMessage' {_dmSubstitutions = Nothing, _dmBody = Nothing}


-- | Default message substitutions. Can be overridden by individual address substitutions.
dmSubstitutions :: Lens' DefaultMessage (HashMap Text [Text])
dmSubstitutions = lens _dmSubstitutions (\ s a -> s{_dmSubstitutions = a}) . _Default . _Map

-- | The message body of the notification, the email body or the text message.
dmBody :: Lens' DefaultMessage (Maybe Text)
dmBody = lens _dmBody (\ s a -> s{_dmBody = a})

instance Hashable DefaultMessage where

instance NFData DefaultMessage where

instance ToJSON DefaultMessage where
        toJSON DefaultMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _dmSubstitutions,
                  ("Body" .=) <$> _dmBody])

-- | Default Push Notification Message.
--
-- /See:/ 'defaultPushNotificationMessage' smart constructor.
data DefaultPushNotificationMessage = DefaultPushNotificationMessage'
  { _dpnmSubstitutions :: !(Maybe (Map Text [Text]))
  , _dpnmSilentPush    :: !(Maybe Bool)
  , _dpnmData          :: !(Maybe (Map Text Text))
  , _dpnmBody          :: !(Maybe Text)
  , _dpnmURL           :: !(Maybe Text)
  , _dpnmAction        :: !(Maybe Action)
  , _dpnmTitle         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefaultPushNotificationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpnmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'dpnmSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'dpnmData' - The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
--
-- * 'dpnmBody' - The message body of the notification, the email body or the text message.
--
-- * 'dpnmURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'dpnmAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
--
-- * 'dpnmTitle' - The message title that displays above the message on the user's device.
defaultPushNotificationMessage
    :: DefaultPushNotificationMessage
defaultPushNotificationMessage =
  DefaultPushNotificationMessage'
    { _dpnmSubstitutions = Nothing
    , _dpnmSilentPush = Nothing
    , _dpnmData = Nothing
    , _dpnmBody = Nothing
    , _dpnmURL = Nothing
    , _dpnmAction = Nothing
    , _dpnmTitle = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
dpnmSubstitutions :: Lens' DefaultPushNotificationMessage (HashMap Text [Text])
dpnmSubstitutions = lens _dpnmSubstitutions (\ s a -> s{_dpnmSubstitutions = a}) . _Default . _Map

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
dpnmSilentPush :: Lens' DefaultPushNotificationMessage (Maybe Bool)
dpnmSilentPush = lens _dpnmSilentPush (\ s a -> s{_dpnmSilentPush = a})

-- | The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
dpnmData :: Lens' DefaultPushNotificationMessage (HashMap Text Text)
dpnmData = lens _dpnmData (\ s a -> s{_dpnmData = a}) . _Default . _Map

-- | The message body of the notification, the email body or the text message.
dpnmBody :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmBody = lens _dpnmBody (\ s a -> s{_dpnmBody = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
dpnmURL :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmURL = lens _dpnmURL (\ s a -> s{_dpnmURL = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
dpnmAction :: Lens' DefaultPushNotificationMessage (Maybe Action)
dpnmAction = lens _dpnmAction (\ s a -> s{_dpnmAction = a})

-- | The message title that displays above the message on the user's device.
dpnmTitle :: Lens' DefaultPushNotificationMessage (Maybe Text)
dpnmTitle = lens _dpnmTitle (\ s a -> s{_dpnmTitle = a})

instance Hashable DefaultPushNotificationMessage
         where

instance NFData DefaultPushNotificationMessage where

instance ToJSON DefaultPushNotificationMessage where
        toJSON DefaultPushNotificationMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _dpnmSubstitutions,
                  ("SilentPush" .=) <$> _dpnmSilentPush,
                  ("Data" .=) <$> _dpnmData, ("Body" .=) <$> _dpnmBody,
                  ("Url" .=) <$> _dpnmURL,
                  ("Action" .=) <$> _dpnmAction,
                  ("Title" .=) <$> _dpnmTitle])

-- | The message configuration.
--
-- /See:/ 'directMessageConfiguration' smart constructor.
data DirectMessageConfiguration = DirectMessageConfiguration'
  { _dmcAPNSMessage :: !(Maybe APNSMessage)
  , _dmcGCMMessage :: !(Maybe GCMMessage)
  , _dmcDefaultMessage :: !(Maybe DefaultMessage)
  , _dmcADMMessage :: !(Maybe ADMMessage)
  , _dmcSMSMessage :: !(Maybe SMSMessage)
  , _dmcBaiduMessage :: !(Maybe BaiduMessage)
  , _dmcDefaultPushNotificationMessage :: !(Maybe DefaultPushNotificationMessage)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectMessageConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcAPNSMessage' - The message to APNS channels. Overrides the default push notification message.
--
-- * 'dmcGCMMessage' - The message to GCM channels. Overrides the default push notification message.
--
-- * 'dmcDefaultMessage' - The default message for all channels.
--
-- * 'dmcADMMessage' - The message to ADM channels. Overrides the default push notification message.
--
-- * 'dmcSMSMessage' - The message to SMS channels. Overrides the default message.
--
-- * 'dmcBaiduMessage' - The message to Baidu GCM channels. Overrides the default push notification message.
--
-- * 'dmcDefaultPushNotificationMessage' - The default push notification message for all push channels.
directMessageConfiguration
    :: DirectMessageConfiguration
directMessageConfiguration =
  DirectMessageConfiguration'
    { _dmcAPNSMessage = Nothing
    , _dmcGCMMessage = Nothing
    , _dmcDefaultMessage = Nothing
    , _dmcADMMessage = Nothing
    , _dmcSMSMessage = Nothing
    , _dmcBaiduMessage = Nothing
    , _dmcDefaultPushNotificationMessage = Nothing
    }


-- | The message to APNS channels. Overrides the default push notification message.
dmcAPNSMessage :: Lens' DirectMessageConfiguration (Maybe APNSMessage)
dmcAPNSMessage = lens _dmcAPNSMessage (\ s a -> s{_dmcAPNSMessage = a})

-- | The message to GCM channels. Overrides the default push notification message.
dmcGCMMessage :: Lens' DirectMessageConfiguration (Maybe GCMMessage)
dmcGCMMessage = lens _dmcGCMMessage (\ s a -> s{_dmcGCMMessage = a})

-- | The default message for all channels.
dmcDefaultMessage :: Lens' DirectMessageConfiguration (Maybe DefaultMessage)
dmcDefaultMessage = lens _dmcDefaultMessage (\ s a -> s{_dmcDefaultMessage = a})

-- | The message to ADM channels. Overrides the default push notification message.
dmcADMMessage :: Lens' DirectMessageConfiguration (Maybe ADMMessage)
dmcADMMessage = lens _dmcADMMessage (\ s a -> s{_dmcADMMessage = a})

-- | The message to SMS channels. Overrides the default message.
dmcSMSMessage :: Lens' DirectMessageConfiguration (Maybe SMSMessage)
dmcSMSMessage = lens _dmcSMSMessage (\ s a -> s{_dmcSMSMessage = a})

-- | The message to Baidu GCM channels. Overrides the default push notification message.
dmcBaiduMessage :: Lens' DirectMessageConfiguration (Maybe BaiduMessage)
dmcBaiduMessage = lens _dmcBaiduMessage (\ s a -> s{_dmcBaiduMessage = a})

-- | The default push notification message for all push channels.
dmcDefaultPushNotificationMessage :: Lens' DirectMessageConfiguration (Maybe DefaultPushNotificationMessage)
dmcDefaultPushNotificationMessage = lens _dmcDefaultPushNotificationMessage (\ s a -> s{_dmcDefaultPushNotificationMessage = a})

instance Hashable DirectMessageConfiguration where

instance NFData DirectMessageConfiguration where

instance ToJSON DirectMessageConfiguration where
        toJSON DirectMessageConfiguration'{..}
          = object
              (catMaybes
                 [("APNSMessage" .=) <$> _dmcAPNSMessage,
                  ("GCMMessage" .=) <$> _dmcGCMMessage,
                  ("DefaultMessage" .=) <$> _dmcDefaultMessage,
                  ("ADMMessage" .=) <$> _dmcADMMessage,
                  ("SMSMessage" .=) <$> _dmcSMSMessage,
                  ("BaiduMessage" .=) <$> _dmcBaiduMessage,
                  ("DefaultPushNotificationMessage" .=) <$>
                    _dmcDefaultPushNotificationMessage])

-- | Email Channel Request
--
-- /See:/ 'emailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { _ecrEnabled     :: !(Maybe Bool)
  , _ecrFromAddress :: !(Maybe Text)
  , _ecrIdentity    :: !(Maybe Text)
  , _ecrRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EmailChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecrEnabled' - If the channel is enabled for sending messages.
--
-- * 'ecrFromAddress' - The email address used to send emails from.
--
-- * 'ecrIdentity' - The ARN of an identity verified with SES.
--
-- * 'ecrRoleARN' - The ARN of an IAM Role used to submit events to Mobile Analytics' event ingestion service
emailChannelRequest
    :: EmailChannelRequest
emailChannelRequest =
  EmailChannelRequest'
    { _ecrEnabled = Nothing
    , _ecrFromAddress = Nothing
    , _ecrIdentity = Nothing
    , _ecrRoleARN = Nothing
    }


-- | If the channel is enabled for sending messages.
ecrEnabled :: Lens' EmailChannelRequest (Maybe Bool)
ecrEnabled = lens _ecrEnabled (\ s a -> s{_ecrEnabled = a})

-- | The email address used to send emails from.
ecrFromAddress :: Lens' EmailChannelRequest (Maybe Text)
ecrFromAddress = lens _ecrFromAddress (\ s a -> s{_ecrFromAddress = a})

-- | The ARN of an identity verified with SES.
ecrIdentity :: Lens' EmailChannelRequest (Maybe Text)
ecrIdentity = lens _ecrIdentity (\ s a -> s{_ecrIdentity = a})

-- | The ARN of an IAM Role used to submit events to Mobile Analytics' event ingestion service
ecrRoleARN :: Lens' EmailChannelRequest (Maybe Text)
ecrRoleARN = lens _ecrRoleARN (\ s a -> s{_ecrRoleARN = a})

instance Hashable EmailChannelRequest where

instance NFData EmailChannelRequest where

instance ToJSON EmailChannelRequest where
        toJSON EmailChannelRequest'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _ecrEnabled,
                  ("FromAddress" .=) <$> _ecrFromAddress,
                  ("Identity" .=) <$> _ecrIdentity,
                  ("RoleArn" .=) <$> _ecrRoleARN])

-- | Email Channel Response.
--
-- /See:/ 'emailChannelResponse' smart constructor.
data EmailChannelResponse = EmailChannelResponse'
  { _ecPlatform         :: !(Maybe Text)
  , _ecLastModifiedDate :: !(Maybe Text)
  , _ecEnabled          :: !(Maybe Bool)
  , _ecFromAddress      :: !(Maybe Text)
  , _ecIsArchived       :: !(Maybe Bool)
  , _ecApplicationId    :: !(Maybe Text)
  , _ecVersion          :: !(Maybe Int)
  , _ecId               :: !(Maybe Text)
  , _ecCreationDate     :: !(Maybe Text)
  , _ecLastModifiedBy   :: !(Maybe Text)
  , _ecIdentity         :: !(Maybe Text)
  , _ecHasCredential    :: !(Maybe Bool)
  , _ecRoleARN          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EmailChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecPlatform' - Platform type. Will be "EMAIL"
--
-- * 'ecLastModifiedDate' - Last date this was updated
--
-- * 'ecEnabled' - If the channel is enabled for sending messages.
--
-- * 'ecFromAddress' - The email address used to send emails from.
--
-- * 'ecIsArchived' - Is this channel archived
--
-- * 'ecApplicationId' - The unique ID of the application to which the email channel belongs.
--
-- * 'ecVersion' - Version of channel
--
-- * 'ecId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'ecCreationDate' - The date that the settings were last updated in ISO 8601 format.
--
-- * 'ecLastModifiedBy' - Who last updated this entry
--
-- * 'ecIdentity' - The ARN of an identity verified with SES.
--
-- * 'ecHasCredential' - If the channel is registered with a credential for authentication.
--
-- * 'ecRoleARN' - The ARN of an IAM Role used to submit events to Mobile Analytics' event ingestion service
emailChannelResponse
    :: EmailChannelResponse
emailChannelResponse =
  EmailChannelResponse'
    { _ecPlatform = Nothing
    , _ecLastModifiedDate = Nothing
    , _ecEnabled = Nothing
    , _ecFromAddress = Nothing
    , _ecIsArchived = Nothing
    , _ecApplicationId = Nothing
    , _ecVersion = Nothing
    , _ecId = Nothing
    , _ecCreationDate = Nothing
    , _ecLastModifiedBy = Nothing
    , _ecIdentity = Nothing
    , _ecHasCredential = Nothing
    , _ecRoleARN = Nothing
    }


-- | Platform type. Will be "EMAIL"
ecPlatform :: Lens' EmailChannelResponse (Maybe Text)
ecPlatform = lens _ecPlatform (\ s a -> s{_ecPlatform = a})

-- | Last date this was updated
ecLastModifiedDate :: Lens' EmailChannelResponse (Maybe Text)
ecLastModifiedDate = lens _ecLastModifiedDate (\ s a -> s{_ecLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
ecEnabled :: Lens' EmailChannelResponse (Maybe Bool)
ecEnabled = lens _ecEnabled (\ s a -> s{_ecEnabled = a})

-- | The email address used to send emails from.
ecFromAddress :: Lens' EmailChannelResponse (Maybe Text)
ecFromAddress = lens _ecFromAddress (\ s a -> s{_ecFromAddress = a})

-- | Is this channel archived
ecIsArchived :: Lens' EmailChannelResponse (Maybe Bool)
ecIsArchived = lens _ecIsArchived (\ s a -> s{_ecIsArchived = a})

-- | The unique ID of the application to which the email channel belongs.
ecApplicationId :: Lens' EmailChannelResponse (Maybe Text)
ecApplicationId = lens _ecApplicationId (\ s a -> s{_ecApplicationId = a})

-- | Version of channel
ecVersion :: Lens' EmailChannelResponse (Maybe Int)
ecVersion = lens _ecVersion (\ s a -> s{_ecVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
ecId :: Lens' EmailChannelResponse (Maybe Text)
ecId = lens _ecId (\ s a -> s{_ecId = a})

-- | The date that the settings were last updated in ISO 8601 format.
ecCreationDate :: Lens' EmailChannelResponse (Maybe Text)
ecCreationDate = lens _ecCreationDate (\ s a -> s{_ecCreationDate = a})

-- | Who last updated this entry
ecLastModifiedBy :: Lens' EmailChannelResponse (Maybe Text)
ecLastModifiedBy = lens _ecLastModifiedBy (\ s a -> s{_ecLastModifiedBy = a})

-- | The ARN of an identity verified with SES.
ecIdentity :: Lens' EmailChannelResponse (Maybe Text)
ecIdentity = lens _ecIdentity (\ s a -> s{_ecIdentity = a})

-- | If the channel is registered with a credential for authentication.
ecHasCredential :: Lens' EmailChannelResponse (Maybe Bool)
ecHasCredential = lens _ecHasCredential (\ s a -> s{_ecHasCredential = a})

-- | The ARN of an IAM Role used to submit events to Mobile Analytics' event ingestion service
ecRoleARN :: Lens' EmailChannelResponse (Maybe Text)
ecRoleARN = lens _ecRoleARN (\ s a -> s{_ecRoleARN = a})

instance FromJSON EmailChannelResponse where
        parseJSON
          = withObject "EmailChannelResponse"
              (\ x ->
                 EmailChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "FromAddress")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "Identity")
                     <*> (x .:? "HasCredential")
                     <*> (x .:? "RoleArn"))

instance Hashable EmailChannelResponse where

instance NFData EmailChannelResponse where

-- | Endpoint update request
--
-- /See:/ 'endpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
  { _ebiRequestId      :: !(Maybe Text)
  , _ebiMetrics        :: !(Maybe (Map Text Double))
  , _ebiLocation       :: !(Maybe EndpointLocation)
  , _ebiDemographic    :: !(Maybe EndpointDemographic)
  , _ebiAddress        :: !(Maybe Text)
  , _ebiEffectiveDate  :: !(Maybe Text)
  , _ebiUser           :: !(Maybe EndpointUser)
  , _ebiAttributes     :: !(Maybe (Map Text [Text]))
  , _ebiEndpointStatus :: !(Maybe Text)
  , _ebiOptOut         :: !(Maybe Text)
  , _ebiId             :: !(Maybe Text)
  , _ebiChannelType    :: !(Maybe ChannelType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointBatchItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebiRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'ebiMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'ebiLocation' - The endpoint location attributes.
--
-- * 'ebiDemographic' - The endpoint demographic attributes.
--
-- * 'ebiAddress' - The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
--
-- * 'ebiEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'ebiUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'ebiAttributes' - Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
--
-- * 'ebiEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'ebiOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
--
-- * 'ebiId' - The unique Id for the Endpoint in the batch.
--
-- * 'ebiChannelType' - The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
endpointBatchItem
    :: EndpointBatchItem
endpointBatchItem =
  EndpointBatchItem'
    { _ebiRequestId = Nothing
    , _ebiMetrics = Nothing
    , _ebiLocation = Nothing
    , _ebiDemographic = Nothing
    , _ebiAddress = Nothing
    , _ebiEffectiveDate = Nothing
    , _ebiUser = Nothing
    , _ebiAttributes = Nothing
    , _ebiEndpointStatus = Nothing
    , _ebiOptOut = Nothing
    , _ebiId = Nothing
    , _ebiChannelType = Nothing
    }


-- | The unique ID for the most recent request to update the endpoint.
ebiRequestId :: Lens' EndpointBatchItem (Maybe Text)
ebiRequestId = lens _ebiRequestId (\ s a -> s{_ebiRequestId = a})

-- | Custom metrics that your app reports to Amazon Pinpoint.
ebiMetrics :: Lens' EndpointBatchItem (HashMap Text Double)
ebiMetrics = lens _ebiMetrics (\ s a -> s{_ebiMetrics = a}) . _Default . _Map

-- | The endpoint location attributes.
ebiLocation :: Lens' EndpointBatchItem (Maybe EndpointLocation)
ebiLocation = lens _ebiLocation (\ s a -> s{_ebiLocation = a})

-- | The endpoint demographic attributes.
ebiDemographic :: Lens' EndpointBatchItem (Maybe EndpointDemographic)
ebiDemographic = lens _ebiDemographic (\ s a -> s{_ebiDemographic = a})

-- | The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
ebiAddress :: Lens' EndpointBatchItem (Maybe Text)
ebiAddress = lens _ebiAddress (\ s a -> s{_ebiAddress = a})

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
ebiEffectiveDate :: Lens' EndpointBatchItem (Maybe Text)
ebiEffectiveDate = lens _ebiEffectiveDate (\ s a -> s{_ebiEffectiveDate = a})

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
ebiUser :: Lens' EndpointBatchItem (Maybe EndpointUser)
ebiUser = lens _ebiUser (\ s a -> s{_ebiUser = a})

-- | Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
ebiAttributes :: Lens' EndpointBatchItem (HashMap Text [Text])
ebiAttributes = lens _ebiAttributes (\ s a -> s{_ebiAttributes = a}) . _Default . _Map

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
ebiEndpointStatus :: Lens' EndpointBatchItem (Maybe Text)
ebiEndpointStatus = lens _ebiEndpointStatus (\ s a -> s{_ebiEndpointStatus = a})

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
ebiOptOut :: Lens' EndpointBatchItem (Maybe Text)
ebiOptOut = lens _ebiOptOut (\ s a -> s{_ebiOptOut = a})

-- | The unique Id for the Endpoint in the batch.
ebiId :: Lens' EndpointBatchItem (Maybe Text)
ebiId = lens _ebiId (\ s a -> s{_ebiId = a})

-- | The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
ebiChannelType :: Lens' EndpointBatchItem (Maybe ChannelType)
ebiChannelType = lens _ebiChannelType (\ s a -> s{_ebiChannelType = a})

instance Hashable EndpointBatchItem where

instance NFData EndpointBatchItem where

instance ToJSON EndpointBatchItem where
        toJSON EndpointBatchItem'{..}
          = object
              (catMaybes
                 [("RequestId" .=) <$> _ebiRequestId,
                  ("Metrics" .=) <$> _ebiMetrics,
                  ("Location" .=) <$> _ebiLocation,
                  ("Demographic" .=) <$> _ebiDemographic,
                  ("Address" .=) <$> _ebiAddress,
                  ("EffectiveDate" .=) <$> _ebiEffectiveDate,
                  ("User" .=) <$> _ebiUser,
                  ("Attributes" .=) <$> _ebiAttributes,
                  ("EndpointStatus" .=) <$> _ebiEndpointStatus,
                  ("OptOut" .=) <$> _ebiOptOut, ("Id" .=) <$> _ebiId,
                  ("ChannelType" .=) <$> _ebiChannelType])

-- | Endpoint batch update request.
--
-- /See:/ 'endpointBatchRequest' smart constructor.
newtype EndpointBatchRequest = EndpointBatchRequest'
  { _ebrItem :: Maybe [EndpointBatchItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebrItem' - List of items to update. Maximum 100 items
endpointBatchRequest
    :: EndpointBatchRequest
endpointBatchRequest = EndpointBatchRequest' {_ebrItem = Nothing}


-- | List of items to update. Maximum 100 items
ebrItem :: Lens' EndpointBatchRequest [EndpointBatchItem]
ebrItem = lens _ebrItem (\ s a -> s{_ebrItem = a}) . _Default . _Coerce

instance Hashable EndpointBatchRequest where

instance NFData EndpointBatchRequest where

instance ToJSON EndpointBatchRequest where
        toJSON EndpointBatchRequest'{..}
          = object (catMaybes [("Item" .=) <$> _ebrItem])

-- | Endpoint demographic data
--
-- /See:/ 'endpointDemographic' smart constructor.
data EndpointDemographic = EndpointDemographic'
  { _edPlatform        :: !(Maybe Text)
  , _edPlatformVersion :: !(Maybe Text)
  , _edLocale          :: !(Maybe Text)
  , _edAppVersion      :: !(Maybe Text)
  , _edModel           :: !(Maybe Text)
  , _edMake            :: !(Maybe Text)
  , _edModelVersion    :: !(Maybe Text)
  , _edTimezone        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointDemographic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edPlatform' - The endpoint platform, such as ios or android.
--
-- * 'edPlatformVersion' - The endpoint platform version.
--
-- * 'edLocale' - The endpoint locale in the following format: The ISO 639-1 alpha-2 code, followed by an underscore, followed by an ISO 3166-1 alpha-2 value.
--
-- * 'edAppVersion' - The version of the application associated with the endpoint.
--
-- * 'edModel' - The endpoint model, such as iPhone.
--
-- * 'edMake' - The endpoint make, such as such as Apple or Samsung.
--
-- * 'edModelVersion' - The endpoint model version.
--
-- * 'edTimezone' - The timezone of the endpoint. Specified as a tz database value, such as Americas/Los_Angeles.
endpointDemographic
    :: EndpointDemographic
endpointDemographic =
  EndpointDemographic'
    { _edPlatform = Nothing
    , _edPlatformVersion = Nothing
    , _edLocale = Nothing
    , _edAppVersion = Nothing
    , _edModel = Nothing
    , _edMake = Nothing
    , _edModelVersion = Nothing
    , _edTimezone = Nothing
    }


-- | The endpoint platform, such as ios or android.
edPlatform :: Lens' EndpointDemographic (Maybe Text)
edPlatform = lens _edPlatform (\ s a -> s{_edPlatform = a})

-- | The endpoint platform version.
edPlatformVersion :: Lens' EndpointDemographic (Maybe Text)
edPlatformVersion = lens _edPlatformVersion (\ s a -> s{_edPlatformVersion = a})

-- | The endpoint locale in the following format: The ISO 639-1 alpha-2 code, followed by an underscore, followed by an ISO 3166-1 alpha-2 value.
edLocale :: Lens' EndpointDemographic (Maybe Text)
edLocale = lens _edLocale (\ s a -> s{_edLocale = a})

-- | The version of the application associated with the endpoint.
edAppVersion :: Lens' EndpointDemographic (Maybe Text)
edAppVersion = lens _edAppVersion (\ s a -> s{_edAppVersion = a})

-- | The endpoint model, such as iPhone.
edModel :: Lens' EndpointDemographic (Maybe Text)
edModel = lens _edModel (\ s a -> s{_edModel = a})

-- | The endpoint make, such as such as Apple or Samsung.
edMake :: Lens' EndpointDemographic (Maybe Text)
edMake = lens _edMake (\ s a -> s{_edMake = a})

-- | The endpoint model version.
edModelVersion :: Lens' EndpointDemographic (Maybe Text)
edModelVersion = lens _edModelVersion (\ s a -> s{_edModelVersion = a})

-- | The timezone of the endpoint. Specified as a tz database value, such as Americas/Los_Angeles.
edTimezone :: Lens' EndpointDemographic (Maybe Text)
edTimezone = lens _edTimezone (\ s a -> s{_edTimezone = a})

instance FromJSON EndpointDemographic where
        parseJSON
          = withObject "EndpointDemographic"
              (\ x ->
                 EndpointDemographic' <$>
                   (x .:? "Platform") <*> (x .:? "PlatformVersion") <*>
                     (x .:? "Locale")
                     <*> (x .:? "AppVersion")
                     <*> (x .:? "Model")
                     <*> (x .:? "Make")
                     <*> (x .:? "ModelVersion")
                     <*> (x .:? "Timezone"))

instance Hashable EndpointDemographic where

instance NFData EndpointDemographic where

instance ToJSON EndpointDemographic where
        toJSON EndpointDemographic'{..}
          = object
              (catMaybes
                 [("Platform" .=) <$> _edPlatform,
                  ("PlatformVersion" .=) <$> _edPlatformVersion,
                  ("Locale" .=) <$> _edLocale,
                  ("AppVersion" .=) <$> _edAppVersion,
                  ("Model" .=) <$> _edModel, ("Make" .=) <$> _edMake,
                  ("ModelVersion" .=) <$> _edModelVersion,
                  ("Timezone" .=) <$> _edTimezone])

-- | Endpoint location data
--
-- /See:/ 'endpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { _elPostalCode :: !(Maybe Text)
  , _elLatitude   :: !(Maybe Double)
  , _elCountry    :: !(Maybe Text)
  , _elCity       :: !(Maybe Text)
  , _elRegion     :: !(Maybe Text)
  , _elLongitude  :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elPostalCode' - The postal code or zip code of the endpoint.
--
-- * 'elLatitude' - The latitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
--
-- * 'elCountry' - Country according to ISO 3166-1 Alpha-2 codes. For example, US.
--
-- * 'elCity' - The city where the endpoint is located.
--
-- * 'elRegion' - The region of the endpoint location. For example, corresponds to a state in US.
--
-- * 'elLongitude' - The longitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
endpointLocation
    :: EndpointLocation
endpointLocation =
  EndpointLocation'
    { _elPostalCode = Nothing
    , _elLatitude = Nothing
    , _elCountry = Nothing
    , _elCity = Nothing
    , _elRegion = Nothing
    , _elLongitude = Nothing
    }


-- | The postal code or zip code of the endpoint.
elPostalCode :: Lens' EndpointLocation (Maybe Text)
elPostalCode = lens _elPostalCode (\ s a -> s{_elPostalCode = a})

-- | The latitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
elLatitude :: Lens' EndpointLocation (Maybe Double)
elLatitude = lens _elLatitude (\ s a -> s{_elLatitude = a})

-- | Country according to ISO 3166-1 Alpha-2 codes. For example, US.
elCountry :: Lens' EndpointLocation (Maybe Text)
elCountry = lens _elCountry (\ s a -> s{_elCountry = a})

-- | The city where the endpoint is located.
elCity :: Lens' EndpointLocation (Maybe Text)
elCity = lens _elCity (\ s a -> s{_elCity = a})

-- | The region of the endpoint location. For example, corresponds to a state in US.
elRegion :: Lens' EndpointLocation (Maybe Text)
elRegion = lens _elRegion (\ s a -> s{_elRegion = a})

-- | The longitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
elLongitude :: Lens' EndpointLocation (Maybe Double)
elLongitude = lens _elLongitude (\ s a -> s{_elLongitude = a})

instance FromJSON EndpointLocation where
        parseJSON
          = withObject "EndpointLocation"
              (\ x ->
                 EndpointLocation' <$>
                   (x .:? "PostalCode") <*> (x .:? "Latitude") <*>
                     (x .:? "Country")
                     <*> (x .:? "City")
                     <*> (x .:? "Region")
                     <*> (x .:? "Longitude"))

instance Hashable EndpointLocation where

instance NFData EndpointLocation where

instance ToJSON EndpointLocation where
        toJSON EndpointLocation'{..}
          = object
              (catMaybes
                 [("PostalCode" .=) <$> _elPostalCode,
                  ("Latitude" .=) <$> _elLatitude,
                  ("Country" .=) <$> _elCountry,
                  ("City" .=) <$> _elCity, ("Region" .=) <$> _elRegion,
                  ("Longitude" .=) <$> _elLongitude])

-- | The result from sending a message to an endpoint.
--
-- /See:/ 'endpointMessageResult' smart constructor.
data EndpointMessageResult = EndpointMessageResult'
  { _emrDeliveryStatus :: !(Maybe DeliveryStatus)
  , _emrAddress        :: !(Maybe Text)
  , _emrStatusMessage  :: !(Maybe Text)
  , _emrUpdatedToken   :: !(Maybe Text)
  , _emrStatusCode     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointMessageResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emrDeliveryStatus' - Delivery status of message.
--
-- * 'emrAddress' - Address that endpoint message was delivered to.
--
-- * 'emrStatusMessage' - Status message for message delivery.
--
-- * 'emrUpdatedToken' - If token was updated as part of delivery. (This is GCM Specific)
--
-- * 'emrStatusCode' - Downstream service status code.
endpointMessageResult
    :: EndpointMessageResult
endpointMessageResult =
  EndpointMessageResult'
    { _emrDeliveryStatus = Nothing
    , _emrAddress = Nothing
    , _emrStatusMessage = Nothing
    , _emrUpdatedToken = Nothing
    , _emrStatusCode = Nothing
    }


-- | Delivery status of message.
emrDeliveryStatus :: Lens' EndpointMessageResult (Maybe DeliveryStatus)
emrDeliveryStatus = lens _emrDeliveryStatus (\ s a -> s{_emrDeliveryStatus = a})

-- | Address that endpoint message was delivered to.
emrAddress :: Lens' EndpointMessageResult (Maybe Text)
emrAddress = lens _emrAddress (\ s a -> s{_emrAddress = a})

-- | Status message for message delivery.
emrStatusMessage :: Lens' EndpointMessageResult (Maybe Text)
emrStatusMessage = lens _emrStatusMessage (\ s a -> s{_emrStatusMessage = a})

-- | If token was updated as part of delivery. (This is GCM Specific)
emrUpdatedToken :: Lens' EndpointMessageResult (Maybe Text)
emrUpdatedToken = lens _emrUpdatedToken (\ s a -> s{_emrUpdatedToken = a})

-- | Downstream service status code.
emrStatusCode :: Lens' EndpointMessageResult (Maybe Int)
emrStatusCode = lens _emrStatusCode (\ s a -> s{_emrStatusCode = a})

instance FromJSON EndpointMessageResult where
        parseJSON
          = withObject "EndpointMessageResult"
              (\ x ->
                 EndpointMessageResult' <$>
                   (x .:? "DeliveryStatus") <*> (x .:? "Address") <*>
                     (x .:? "StatusMessage")
                     <*> (x .:? "UpdatedToken")
                     <*> (x .:? "StatusCode"))

instance Hashable EndpointMessageResult where

instance NFData EndpointMessageResult where

-- | Endpoint update request
--
-- /See:/ 'endpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { _erRequestId      :: !(Maybe Text)
  , _erMetrics        :: !(Maybe (Map Text Double))
  , _erLocation       :: !(Maybe EndpointLocation)
  , _erDemographic    :: !(Maybe EndpointDemographic)
  , _erAddress        :: !(Maybe Text)
  , _erEffectiveDate  :: !(Maybe Text)
  , _erUser           :: !(Maybe EndpointUser)
  , _erAttributes     :: !(Maybe (Map Text [Text]))
  , _erEndpointStatus :: !(Maybe Text)
  , _erOptOut         :: !(Maybe Text)
  , _erChannelType    :: !(Maybe ChannelType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'erMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'erLocation' - The endpoint location attributes.
--
-- * 'erDemographic' - The endpoint demographic attributes.
--
-- * 'erAddress' - The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
--
-- * 'erEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'erUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'erAttributes' - Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
--
-- * 'erEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'erOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
--
-- * 'erChannelType' - The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
endpointRequest
    :: EndpointRequest
endpointRequest =
  EndpointRequest'
    { _erRequestId = Nothing
    , _erMetrics = Nothing
    , _erLocation = Nothing
    , _erDemographic = Nothing
    , _erAddress = Nothing
    , _erEffectiveDate = Nothing
    , _erUser = Nothing
    , _erAttributes = Nothing
    , _erEndpointStatus = Nothing
    , _erOptOut = Nothing
    , _erChannelType = Nothing
    }


-- | The unique ID for the most recent request to update the endpoint.
erRequestId :: Lens' EndpointRequest (Maybe Text)
erRequestId = lens _erRequestId (\ s a -> s{_erRequestId = a})

-- | Custom metrics that your app reports to Amazon Pinpoint.
erMetrics :: Lens' EndpointRequest (HashMap Text Double)
erMetrics = lens _erMetrics (\ s a -> s{_erMetrics = a}) . _Default . _Map

-- | The endpoint location attributes.
erLocation :: Lens' EndpointRequest (Maybe EndpointLocation)
erLocation = lens _erLocation (\ s a -> s{_erLocation = a})

-- | The endpoint demographic attributes.
erDemographic :: Lens' EndpointRequest (Maybe EndpointDemographic)
erDemographic = lens _erDemographic (\ s a -> s{_erDemographic = a})

-- | The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
erAddress :: Lens' EndpointRequest (Maybe Text)
erAddress = lens _erAddress (\ s a -> s{_erAddress = a})

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
erEffectiveDate :: Lens' EndpointRequest (Maybe Text)
erEffectiveDate = lens _erEffectiveDate (\ s a -> s{_erEffectiveDate = a})

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
erUser :: Lens' EndpointRequest (Maybe EndpointUser)
erUser = lens _erUser (\ s a -> s{_erUser = a})

-- | Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
erAttributes :: Lens' EndpointRequest (HashMap Text [Text])
erAttributes = lens _erAttributes (\ s a -> s{_erAttributes = a}) . _Default . _Map

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
erEndpointStatus :: Lens' EndpointRequest (Maybe Text)
erEndpointStatus = lens _erEndpointStatus (\ s a -> s{_erEndpointStatus = a})

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
erOptOut :: Lens' EndpointRequest (Maybe Text)
erOptOut = lens _erOptOut (\ s a -> s{_erOptOut = a})

-- | The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
erChannelType :: Lens' EndpointRequest (Maybe ChannelType)
erChannelType = lens _erChannelType (\ s a -> s{_erChannelType = a})

instance Hashable EndpointRequest where

instance NFData EndpointRequest where

instance ToJSON EndpointRequest where
        toJSON EndpointRequest'{..}
          = object
              (catMaybes
                 [("RequestId" .=) <$> _erRequestId,
                  ("Metrics" .=) <$> _erMetrics,
                  ("Location" .=) <$> _erLocation,
                  ("Demographic" .=) <$> _erDemographic,
                  ("Address" .=) <$> _erAddress,
                  ("EffectiveDate" .=) <$> _erEffectiveDate,
                  ("User" .=) <$> _erUser,
                  ("Attributes" .=) <$> _erAttributes,
                  ("EndpointStatus" .=) <$> _erEndpointStatus,
                  ("OptOut" .=) <$> _erOptOut,
                  ("ChannelType" .=) <$> _erChannelType])

-- | Endpoint response
--
-- /See:/ 'endpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
  { _eRequestId      :: !(Maybe Text)
  , _eMetrics        :: !(Maybe (Map Text Double))
  , _eLocation       :: !(Maybe EndpointLocation)
  , _eDemographic    :: !(Maybe EndpointDemographic)
  , _eCohortId       :: !(Maybe Text)
  , _eAddress        :: !(Maybe Text)
  , _eEffectiveDate  :: !(Maybe Text)
  , _eUser           :: !(Maybe EndpointUser)
  , _eApplicationId  :: !(Maybe Text)
  , _eAttributes     :: !(Maybe (Map Text [Text]))
  , _eEndpointStatus :: !(Maybe Text)
  , _eOptOut         :: !(Maybe Text)
  , _eId             :: !(Maybe Text)
  , _eCreationDate   :: !(Maybe Text)
  , _eChannelType    :: !(Maybe ChannelType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'eMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'eLocation' - The endpoint location attributes.
--
-- * 'eDemographic' - The endpoint demographic attributes.
--
-- * 'eCohortId' - A number from 0 - 99 that represents the cohort the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an app. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for a campaign.
--
-- * 'eAddress' - The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
--
-- * 'eEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'eUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'eApplicationId' - The ID of the application associated with the endpoint.
--
-- * 'eAttributes' - Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
--
-- * 'eEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'eOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
--
-- * 'eId' - The unique ID that you assigned to the endpoint. The ID should be a globally unique identifier (GUID) to ensure that it is unique compared to all other endpoints for the application.
--
-- * 'eCreationDate' - The last time the endpoint was created. Provided in ISO 8601 format.
--
-- * 'eChannelType' - The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
endpointResponse
    :: EndpointResponse
endpointResponse =
  EndpointResponse'
    { _eRequestId = Nothing
    , _eMetrics = Nothing
    , _eLocation = Nothing
    , _eDemographic = Nothing
    , _eCohortId = Nothing
    , _eAddress = Nothing
    , _eEffectiveDate = Nothing
    , _eUser = Nothing
    , _eApplicationId = Nothing
    , _eAttributes = Nothing
    , _eEndpointStatus = Nothing
    , _eOptOut = Nothing
    , _eId = Nothing
    , _eCreationDate = Nothing
    , _eChannelType = Nothing
    }


-- | The unique ID for the most recent request to update the endpoint.
eRequestId :: Lens' EndpointResponse (Maybe Text)
eRequestId = lens _eRequestId (\ s a -> s{_eRequestId = a})

-- | Custom metrics that your app reports to Amazon Pinpoint.
eMetrics :: Lens' EndpointResponse (HashMap Text Double)
eMetrics = lens _eMetrics (\ s a -> s{_eMetrics = a}) . _Default . _Map

-- | The endpoint location attributes.
eLocation :: Lens' EndpointResponse (Maybe EndpointLocation)
eLocation = lens _eLocation (\ s a -> s{_eLocation = a})

-- | The endpoint demographic attributes.
eDemographic :: Lens' EndpointResponse (Maybe EndpointDemographic)
eDemographic = lens _eDemographic (\ s a -> s{_eDemographic = a})

-- | A number from 0 - 99 that represents the cohort the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an app. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for a campaign.
eCohortId :: Lens' EndpointResponse (Maybe Text)
eCohortId = lens _eCohortId (\ s a -> s{_eCohortId = a})

-- | The address or token of the endpoint as provided by your push provider (e.g. DeviceToken or RegistrationId).
eAddress :: Lens' EndpointResponse (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
eEffectiveDate :: Lens' EndpointResponse (Maybe Text)
eEffectiveDate = lens _eEffectiveDate (\ s a -> s{_eEffectiveDate = a})

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
eUser :: Lens' EndpointResponse (Maybe EndpointUser)
eUser = lens _eUser (\ s a -> s{_eUser = a})

-- | The ID of the application associated with the endpoint.
eApplicationId :: Lens' EndpointResponse (Maybe Text)
eApplicationId = lens _eApplicationId (\ s a -> s{_eApplicationId = a})

-- | Custom attributes that describe the endpoint by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
eAttributes :: Lens' EndpointResponse (HashMap Text [Text])
eAttributes = lens _eAttributes (\ s a -> s{_eAttributes = a}) . _Default . _Map

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
eEndpointStatus :: Lens' EndpointResponse (Maybe Text)
eEndpointStatus = lens _eEndpointStatus (\ s a -> s{_eEndpointStatus = a})

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL - User has opted out of all messages. NONE - Users has not opted out and receives all messages.
eOptOut :: Lens' EndpointResponse (Maybe Text)
eOptOut = lens _eOptOut (\ s a -> s{_eOptOut = a})

-- | The unique ID that you assigned to the endpoint. The ID should be a globally unique identifier (GUID) to ensure that it is unique compared to all other endpoints for the application.
eId :: Lens' EndpointResponse (Maybe Text)
eId = lens _eId (\ s a -> s{_eId = a})

-- | The last time the endpoint was created. Provided in ISO 8601 format.
eCreationDate :: Lens' EndpointResponse (Maybe Text)
eCreationDate = lens _eCreationDate (\ s a -> s{_eCreationDate = a})

-- | The channel type. Valid values: GCM | APNS | APNS_SANDBOX | APNS_VOIP | APNS_VOIP_SANDBOX | ADM | SMS | EMAIL | BAIDU
eChannelType :: Lens' EndpointResponse (Maybe ChannelType)
eChannelType = lens _eChannelType (\ s a -> s{_eChannelType = a})

instance FromJSON EndpointResponse where
        parseJSON
          = withObject "EndpointResponse"
              (\ x ->
                 EndpointResponse' <$>
                   (x .:? "RequestId") <*> (x .:? "Metrics" .!= mempty)
                     <*> (x .:? "Location")
                     <*> (x .:? "Demographic")
                     <*> (x .:? "CohortId")
                     <*> (x .:? "Address")
                     <*> (x .:? "EffectiveDate")
                     <*> (x .:? "User")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "EndpointStatus")
                     <*> (x .:? "OptOut")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ChannelType"))

instance Hashable EndpointResponse where

instance NFData EndpointResponse where

-- | Endpoint send configuration.
--
-- /See:/ 'endpointSendConfiguration' smart constructor.
data EndpointSendConfiguration = EndpointSendConfiguration'
  { _escSubstitutions :: !(Maybe (Map Text [Text]))
  , _escTitleOverride :: !(Maybe Text)
  , _escContext       :: !(Maybe (Map Text Text))
  , _escRawContent    :: !(Maybe Text)
  , _escBodyOverride  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointSendConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'escSubstitutions' - A map of substitution values for the message to be merged with the DefaultMessage's substitutions. Substitutions on this map take precedence over the all other substitutions.
--
-- * 'escTitleOverride' - Title override. If specified will override default title if applicable.
--
-- * 'escContext' - A map of custom attributes to attributes to be attached to the message for this address. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
--
-- * 'escRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'escBodyOverride' - Body override. If specified will override default body.
endpointSendConfiguration
    :: EndpointSendConfiguration
endpointSendConfiguration =
  EndpointSendConfiguration'
    { _escSubstitutions = Nothing
    , _escTitleOverride = Nothing
    , _escContext = Nothing
    , _escRawContent = Nothing
    , _escBodyOverride = Nothing
    }


-- | A map of substitution values for the message to be merged with the DefaultMessage's substitutions. Substitutions on this map take precedence over the all other substitutions.
escSubstitutions :: Lens' EndpointSendConfiguration (HashMap Text [Text])
escSubstitutions = lens _escSubstitutions (\ s a -> s{_escSubstitutions = a}) . _Default . _Map

-- | Title override. If specified will override default title if applicable.
escTitleOverride :: Lens' EndpointSendConfiguration (Maybe Text)
escTitleOverride = lens _escTitleOverride (\ s a -> s{_escTitleOverride = a})

-- | A map of custom attributes to attributes to be attached to the message for this address. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
escContext :: Lens' EndpointSendConfiguration (HashMap Text Text)
escContext = lens _escContext (\ s a -> s{_escContext = a}) . _Default . _Map

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
escRawContent :: Lens' EndpointSendConfiguration (Maybe Text)
escRawContent = lens _escRawContent (\ s a -> s{_escRawContent = a})

-- | Body override. If specified will override default body.
escBodyOverride :: Lens' EndpointSendConfiguration (Maybe Text)
escBodyOverride = lens _escBodyOverride (\ s a -> s{_escBodyOverride = a})

instance Hashable EndpointSendConfiguration where

instance NFData EndpointSendConfiguration where

instance ToJSON EndpointSendConfiguration where
        toJSON EndpointSendConfiguration'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _escSubstitutions,
                  ("TitleOverride" .=) <$> _escTitleOverride,
                  ("Context" .=) <$> _escContext,
                  ("RawContent" .=) <$> _escRawContent,
                  ("BodyOverride" .=) <$> _escBodyOverride])

-- | Endpoint user specific custom userAttributes
--
-- /See:/ 'endpointUser' smart constructor.
data EndpointUser = EndpointUser'
  { _euUserAttributes :: !(Maybe (Map Text [Text]))
  , _euUserId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EndpointUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'euUserAttributes' - Custom attributes that describe an end user by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
--
-- * 'euUserId' - The unique ID of the user.
endpointUser
    :: EndpointUser
endpointUser = EndpointUser' {_euUserAttributes = Nothing, _euUserId = Nothing}


-- | Custom attributes that describe an end user by associating a name with an array of values. For example, an attribute named "interests" might have the values ["science", "politics", "travel"]. You can use these attributes as selection criteria when you create a segment of users to engage with a messaging campaign. The following characters are not recommended in attribute names: # : ? \ /. The Amazon Pinpoint console does not display attributes that include these characters in the name. This limitation does not apply to attribute values.
euUserAttributes :: Lens' EndpointUser (HashMap Text [Text])
euUserAttributes = lens _euUserAttributes (\ s a -> s{_euUserAttributes = a}) . _Default . _Map

-- | The unique ID of the user.
euUserId :: Lens' EndpointUser (Maybe Text)
euUserId = lens _euUserId (\ s a -> s{_euUserId = a})

instance FromJSON EndpointUser where
        parseJSON
          = withObject "EndpointUser"
              (\ x ->
                 EndpointUser' <$>
                   (x .:? "UserAttributes" .!= mempty) <*>
                     (x .:? "UserId"))

instance Hashable EndpointUser where

instance NFData EndpointUser where

instance ToJSON EndpointUser where
        toJSON EndpointUser'{..}
          = object
              (catMaybes
                 [("UserAttributes" .=) <$> _euUserAttributes,
                  ("UserId" .=) <$> _euUserId])

-- | Model for an event publishing subscription export.
--
-- /See:/ 'eventStream' smart constructor.
data EventStream = EventStream'
  { _esLastUpdatedBy        :: !(Maybe Text)
  , _esLastModifiedDate     :: !(Maybe Text)
  , _esDestinationStreamARN :: !(Maybe Text)
  , _esApplicationId        :: !(Maybe Text)
  , _esExternalId           :: !(Maybe Text)
  , _esRoleARN              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esLastUpdatedBy' - The IAM user who last modified the event stream.
--
-- * 'esLastModifiedDate' - The date the event stream was last updated in ISO 8601 format.
--
-- * 'esDestinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis stream or Firehose delivery stream to which you want to publish events.  Firehose ARN: arn:aws:firehose:REGION:ACCOUNT_ID:deliverystream/STREAM_NAME  Kinesis ARN: arn:aws:kinesis:REGION:ACCOUNT_ID:stream/STREAM_NAME
--
-- * 'esApplicationId' - The ID of the application from which events should be published.
--
-- * 'esExternalId' - DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
--
-- * 'esRoleARN' - The IAM role that authorizes Amazon Pinpoint to publish events to the stream in your account.
eventStream
    :: EventStream
eventStream =
  EventStream'
    { _esLastUpdatedBy = Nothing
    , _esLastModifiedDate = Nothing
    , _esDestinationStreamARN = Nothing
    , _esApplicationId = Nothing
    , _esExternalId = Nothing
    , _esRoleARN = Nothing
    }


-- | The IAM user who last modified the event stream.
esLastUpdatedBy :: Lens' EventStream (Maybe Text)
esLastUpdatedBy = lens _esLastUpdatedBy (\ s a -> s{_esLastUpdatedBy = a})

-- | The date the event stream was last updated in ISO 8601 format.
esLastModifiedDate :: Lens' EventStream (Maybe Text)
esLastModifiedDate = lens _esLastModifiedDate (\ s a -> s{_esLastModifiedDate = a})

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream or Firehose delivery stream to which you want to publish events.  Firehose ARN: arn:aws:firehose:REGION:ACCOUNT_ID:deliverystream/STREAM_NAME  Kinesis ARN: arn:aws:kinesis:REGION:ACCOUNT_ID:stream/STREAM_NAME
esDestinationStreamARN :: Lens' EventStream (Maybe Text)
esDestinationStreamARN = lens _esDestinationStreamARN (\ s a -> s{_esDestinationStreamARN = a})

-- | The ID of the application from which events should be published.
esApplicationId :: Lens' EventStream (Maybe Text)
esApplicationId = lens _esApplicationId (\ s a -> s{_esApplicationId = a})

-- | DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
esExternalId :: Lens' EventStream (Maybe Text)
esExternalId = lens _esExternalId (\ s a -> s{_esExternalId = a})

-- | The IAM role that authorizes Amazon Pinpoint to publish events to the stream in your account.
esRoleARN :: Lens' EventStream (Maybe Text)
esRoleARN = lens _esRoleARN (\ s a -> s{_esRoleARN = a})

instance FromJSON EventStream where
        parseJSON
          = withObject "EventStream"
              (\ x ->
                 EventStream' <$>
                   (x .:? "LastUpdatedBy") <*>
                     (x .:? "LastModifiedDate")
                     <*> (x .:? "DestinationStreamArn")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "RoleArn"))

instance Hashable EventStream where

instance NFData EventStream where

-- | /See:/ 'exportJobRequest' smart constructor.
data ExportJobRequest = ExportJobRequest'
  { _eS3URLPrefix :: !(Maybe Text)
  , _eSegmentId   :: !(Maybe Text)
  , _eRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJobRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eS3URLPrefix' - A URL that points to the location within an Amazon S3 bucket that will receive the export. The location is typically a folder with multiple files. The URL should follow this format: s3://bucket-name/folder-name/ Amazon Pinpoint will export endpoints to this location.
--
-- * 'eSegmentId' - The ID of the segment to export endpoints from. If not present all endpoints will be exported.
--
-- * 'eRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that endpoints will be exported to.
exportJobRequest
    :: ExportJobRequest
exportJobRequest =
  ExportJobRequest'
    {_eS3URLPrefix = Nothing, _eSegmentId = Nothing, _eRoleARN = Nothing}


-- | A URL that points to the location within an Amazon S3 bucket that will receive the export. The location is typically a folder with multiple files. The URL should follow this format: s3://bucket-name/folder-name/ Amazon Pinpoint will export endpoints to this location.
eS3URLPrefix :: Lens' ExportJobRequest (Maybe Text)
eS3URLPrefix = lens _eS3URLPrefix (\ s a -> s{_eS3URLPrefix = a})

-- | The ID of the segment to export endpoints from. If not present all endpoints will be exported.
eSegmentId :: Lens' ExportJobRequest (Maybe Text)
eSegmentId = lens _eSegmentId (\ s a -> s{_eSegmentId = a})

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that endpoints will be exported to.
eRoleARN :: Lens' ExportJobRequest (Maybe Text)
eRoleARN = lens _eRoleARN (\ s a -> s{_eRoleARN = a})

instance Hashable ExportJobRequest where

instance NFData ExportJobRequest where

instance ToJSON ExportJobRequest where
        toJSON ExportJobRequest'{..}
          = object
              (catMaybes
                 [("S3UrlPrefix" .=) <$> _eS3URLPrefix,
                  ("SegmentId" .=) <$> _eSegmentId,
                  ("RoleArn" .=) <$> _eRoleARN])

-- | /See:/ 'exportJobResource' smart constructor.
data ExportJobResource = ExportJobResource'
  { _ejrS3URLPrefix :: !(Maybe Text)
  , _ejrSegmentId   :: !(Maybe Text)
  , _ejrRoleARN     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejrS3URLPrefix' - A URL that points to the location within an Amazon S3 bucket that will receive the export. The location is typically a folder with multiple files. The URL should follow this format: s3://bucket-name/folder-name/ Amazon Pinpoint will export endpoints to this location.
--
-- * 'ejrSegmentId' - The ID of the segment to export endpoints from. If not present, all endpoints are exported.
--
-- * 'ejrRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that endpoints will be exported to.
exportJobResource
    :: ExportJobResource
exportJobResource =
  ExportJobResource'
    {_ejrS3URLPrefix = Nothing, _ejrSegmentId = Nothing, _ejrRoleARN = Nothing}


-- | A URL that points to the location within an Amazon S3 bucket that will receive the export. The location is typically a folder with multiple files. The URL should follow this format: s3://bucket-name/folder-name/ Amazon Pinpoint will export endpoints to this location.
ejrS3URLPrefix :: Lens' ExportJobResource (Maybe Text)
ejrS3URLPrefix = lens _ejrS3URLPrefix (\ s a -> s{_ejrS3URLPrefix = a})

-- | The ID of the segment to export endpoints from. If not present, all endpoints are exported.
ejrSegmentId :: Lens' ExportJobResource (Maybe Text)
ejrSegmentId = lens _ejrSegmentId (\ s a -> s{_ejrSegmentId = a})

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that endpoints will be exported to.
ejrRoleARN :: Lens' ExportJobResource (Maybe Text)
ejrRoleARN = lens _ejrRoleARN (\ s a -> s{_ejrRoleARN = a})

instance FromJSON ExportJobResource where
        parseJSON
          = withObject "ExportJobResource"
              (\ x ->
                 ExportJobResource' <$>
                   (x .:? "S3UrlPrefix") <*> (x .:? "SegmentId") <*>
                     (x .:? "RoleArn"))

instance Hashable ExportJobResource where

instance NFData ExportJobResource where

-- | /See:/ 'exportJobResponse' smart constructor.
data ExportJobResponse = ExportJobResponse'
  { _ejCompletedPieces :: !(Maybe Int)
  , _ejFailedPieces    :: !(Maybe Int)
  , _ejDefinition      :: !(Maybe ExportJobResource)
  , _ejTotalProcessed  :: !(Maybe Int)
  , _ejFailures        :: !(Maybe [Text])
  , _ejTotalPieces     :: !(Maybe Int)
  , _ejApplicationId   :: !(Maybe Text)
  , _ejId              :: !(Maybe Text)
  , _ejCreationDate    :: !(Maybe Text)
  , _ejType            :: !(Maybe Text)
  , _ejCompletionDate  :: !(Maybe Text)
  , _ejJobStatus       :: !(Maybe JobStatus)
  , _ejTotalFailures   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejCompletedPieces' - The number of pieces that have successfully completed as of the time of the request.
--
-- * 'ejFailedPieces' - The number of pieces that failed to be processed as of the time of the request.
--
-- * 'ejDefinition' - The export job settings.
--
-- * 'ejTotalProcessed' - The number of endpoints that were processed by the job.
--
-- * 'ejFailures' - Provides up to 100 of the first failed entries for the job, if any exist.
--
-- * 'ejTotalPieces' - The total number of pieces that must be processed to finish the job. Each piece is an approximately equal portion of the endpoints.
--
-- * 'ejApplicationId' - The unique ID of the application to which the job applies.
--
-- * 'ejId' - The unique ID of the job.
--
-- * 'ejCreationDate' - The date the job was created in ISO 8601 format.
--
-- * 'ejType' - The job type. Will be 'EXPORT'.
--
-- * 'ejCompletionDate' - The date the job completed in ISO 8601 format.
--
-- * 'ejJobStatus' - The status of the job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed.
--
-- * 'ejTotalFailures' - The number of endpoints that were not processed; for example, because of syntax errors.
exportJobResponse
    :: ExportJobResponse
exportJobResponse =
  ExportJobResponse'
    { _ejCompletedPieces = Nothing
    , _ejFailedPieces = Nothing
    , _ejDefinition = Nothing
    , _ejTotalProcessed = Nothing
    , _ejFailures = Nothing
    , _ejTotalPieces = Nothing
    , _ejApplicationId = Nothing
    , _ejId = Nothing
    , _ejCreationDate = Nothing
    , _ejType = Nothing
    , _ejCompletionDate = Nothing
    , _ejJobStatus = Nothing
    , _ejTotalFailures = Nothing
    }


-- | The number of pieces that have successfully completed as of the time of the request.
ejCompletedPieces :: Lens' ExportJobResponse (Maybe Int)
ejCompletedPieces = lens _ejCompletedPieces (\ s a -> s{_ejCompletedPieces = a})

-- | The number of pieces that failed to be processed as of the time of the request.
ejFailedPieces :: Lens' ExportJobResponse (Maybe Int)
ejFailedPieces = lens _ejFailedPieces (\ s a -> s{_ejFailedPieces = a})

-- | The export job settings.
ejDefinition :: Lens' ExportJobResponse (Maybe ExportJobResource)
ejDefinition = lens _ejDefinition (\ s a -> s{_ejDefinition = a})

-- | The number of endpoints that were processed by the job.
ejTotalProcessed :: Lens' ExportJobResponse (Maybe Int)
ejTotalProcessed = lens _ejTotalProcessed (\ s a -> s{_ejTotalProcessed = a})

-- | Provides up to 100 of the first failed entries for the job, if any exist.
ejFailures :: Lens' ExportJobResponse [Text]
ejFailures = lens _ejFailures (\ s a -> s{_ejFailures = a}) . _Default . _Coerce

-- | The total number of pieces that must be processed to finish the job. Each piece is an approximately equal portion of the endpoints.
ejTotalPieces :: Lens' ExportJobResponse (Maybe Int)
ejTotalPieces = lens _ejTotalPieces (\ s a -> s{_ejTotalPieces = a})

-- | The unique ID of the application to which the job applies.
ejApplicationId :: Lens' ExportJobResponse (Maybe Text)
ejApplicationId = lens _ejApplicationId (\ s a -> s{_ejApplicationId = a})

-- | The unique ID of the job.
ejId :: Lens' ExportJobResponse (Maybe Text)
ejId = lens _ejId (\ s a -> s{_ejId = a})

-- | The date the job was created in ISO 8601 format.
ejCreationDate :: Lens' ExportJobResponse (Maybe Text)
ejCreationDate = lens _ejCreationDate (\ s a -> s{_ejCreationDate = a})

-- | The job type. Will be 'EXPORT'.
ejType :: Lens' ExportJobResponse (Maybe Text)
ejType = lens _ejType (\ s a -> s{_ejType = a})

-- | The date the job completed in ISO 8601 format.
ejCompletionDate :: Lens' ExportJobResponse (Maybe Text)
ejCompletionDate = lens _ejCompletionDate (\ s a -> s{_ejCompletionDate = a})

-- | The status of the job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed.
ejJobStatus :: Lens' ExportJobResponse (Maybe JobStatus)
ejJobStatus = lens _ejJobStatus (\ s a -> s{_ejJobStatus = a})

-- | The number of endpoints that were not processed; for example, because of syntax errors.
ejTotalFailures :: Lens' ExportJobResponse (Maybe Int)
ejTotalFailures = lens _ejTotalFailures (\ s a -> s{_ejTotalFailures = a})

instance FromJSON ExportJobResponse where
        parseJSON
          = withObject "ExportJobResponse"
              (\ x ->
                 ExportJobResponse' <$>
                   (x .:? "CompletedPieces") <*> (x .:? "FailedPieces")
                     <*> (x .:? "Definition")
                     <*> (x .:? "TotalProcessed")
                     <*> (x .:? "Failures" .!= mempty)
                     <*> (x .:? "TotalPieces")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "JobStatus")
                     <*> (x .:? "TotalFailures"))

instance Hashable ExportJobResponse where

instance NFData ExportJobResponse where

-- | Export job list.
--
-- /See:/ 'exportJobsResponse' smart constructor.
data ExportJobsResponse = ExportJobsResponse'
  { _ejNextToken :: !(Maybe Text)
  , _ejItem      :: !(Maybe [ExportJobResponse])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'ejItem' - A list of export jobs for the application.
exportJobsResponse
    :: ExportJobsResponse
exportJobsResponse =
  ExportJobsResponse' {_ejNextToken = Nothing, _ejItem = Nothing}


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
ejNextToken :: Lens' ExportJobsResponse (Maybe Text)
ejNextToken = lens _ejNextToken (\ s a -> s{_ejNextToken = a})

-- | A list of export jobs for the application.
ejItem :: Lens' ExportJobsResponse [ExportJobResponse]
ejItem = lens _ejItem (\ s a -> s{_ejItem = a}) . _Default . _Coerce

instance FromJSON ExportJobsResponse where
        parseJSON
          = withObject "ExportJobsResponse"
              (\ x ->
                 ExportJobsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable ExportJobsResponse where

instance NFData ExportJobsResponse where

-- | Google Cloud Messaging credentials
--
-- /See:/ 'gcmChannelRequest' smart constructor.
data GCMChannelRequest = GCMChannelRequest'
  { _gcrAPIKey  :: !(Maybe Text)
  , _gcrEnabled :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GCMChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrAPIKey' - Platform credential API key from Google.
--
-- * 'gcrEnabled' - If the channel is enabled for sending messages.
gcmChannelRequest
    :: GCMChannelRequest
gcmChannelRequest =
  GCMChannelRequest' {_gcrAPIKey = Nothing, _gcrEnabled = Nothing}


-- | Platform credential API key from Google.
gcrAPIKey :: Lens' GCMChannelRequest (Maybe Text)
gcrAPIKey = lens _gcrAPIKey (\ s a -> s{_gcrAPIKey = a})

-- | If the channel is enabled for sending messages.
gcrEnabled :: Lens' GCMChannelRequest (Maybe Bool)
gcrEnabled = lens _gcrEnabled (\ s a -> s{_gcrEnabled = a})

instance Hashable GCMChannelRequest where

instance NFData GCMChannelRequest where

instance ToJSON GCMChannelRequest where
        toJSON GCMChannelRequest'{..}
          = object
              (catMaybes
                 [("ApiKey" .=) <$> _gcrAPIKey,
                  ("Enabled" .=) <$> _gcrEnabled])

-- | Google Cloud Messaging channel definition
--
-- /See:/ 'gcmChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { _gcPlatform         :: !(Maybe Text)
  , _gcLastModifiedDate :: !(Maybe Text)
  , _gcEnabled          :: !(Maybe Bool)
  , _gcCredential       :: !(Maybe Text)
  , _gcIsArchived       :: !(Maybe Bool)
  , _gcApplicationId    :: !(Maybe Text)
  , _gcVersion          :: !(Maybe Int)
  , _gcId               :: !(Maybe Text)
  , _gcCreationDate     :: !(Maybe Text)
  , _gcLastModifiedBy   :: !(Maybe Text)
  , _gcHasCredential    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcPlatform' - The platform type. Will be GCM
--
-- * 'gcLastModifiedDate' - Last date this was updated
--
-- * 'gcEnabled' - If the channel is enabled for sending messages.
--
-- * 'gcCredential' - The GCM API key from Google.
--
-- * 'gcIsArchived' - Is this channel archived
--
-- * 'gcApplicationId' - The ID of the application to which the channel applies.
--
-- * 'gcVersion' - Version of channel
--
-- * 'gcId' - Channel ID. Not used. Present only for backwards compatibility.
--
-- * 'gcCreationDate' - When was this segment created
--
-- * 'gcLastModifiedBy' - Who last updated this entry
--
-- * 'gcHasCredential' - Indicates whether the channel is configured with FCM or GCM credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with FCM or GCM. Provide your credentials by setting the ApiKey attribute.
gcmChannelResponse
    :: GCMChannelResponse
gcmChannelResponse =
  GCMChannelResponse'
    { _gcPlatform = Nothing
    , _gcLastModifiedDate = Nothing
    , _gcEnabled = Nothing
    , _gcCredential = Nothing
    , _gcIsArchived = Nothing
    , _gcApplicationId = Nothing
    , _gcVersion = Nothing
    , _gcId = Nothing
    , _gcCreationDate = Nothing
    , _gcLastModifiedBy = Nothing
    , _gcHasCredential = Nothing
    }


-- | The platform type. Will be GCM
gcPlatform :: Lens' GCMChannelResponse (Maybe Text)
gcPlatform = lens _gcPlatform (\ s a -> s{_gcPlatform = a})

-- | Last date this was updated
gcLastModifiedDate :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedDate = lens _gcLastModifiedDate (\ s a -> s{_gcLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
gcEnabled :: Lens' GCMChannelResponse (Maybe Bool)
gcEnabled = lens _gcEnabled (\ s a -> s{_gcEnabled = a})

-- | The GCM API key from Google.
gcCredential :: Lens' GCMChannelResponse (Maybe Text)
gcCredential = lens _gcCredential (\ s a -> s{_gcCredential = a})

-- | Is this channel archived
gcIsArchived :: Lens' GCMChannelResponse (Maybe Bool)
gcIsArchived = lens _gcIsArchived (\ s a -> s{_gcIsArchived = a})

-- | The ID of the application to which the channel applies.
gcApplicationId :: Lens' GCMChannelResponse (Maybe Text)
gcApplicationId = lens _gcApplicationId (\ s a -> s{_gcApplicationId = a})

-- | Version of channel
gcVersion :: Lens' GCMChannelResponse (Maybe Int)
gcVersion = lens _gcVersion (\ s a -> s{_gcVersion = a})

-- | Channel ID. Not used. Present only for backwards compatibility.
gcId :: Lens' GCMChannelResponse (Maybe Text)
gcId = lens _gcId (\ s a -> s{_gcId = a})

-- | When was this segment created
gcCreationDate :: Lens' GCMChannelResponse (Maybe Text)
gcCreationDate = lens _gcCreationDate (\ s a -> s{_gcCreationDate = a})

-- | Who last updated this entry
gcLastModifiedBy :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedBy = lens _gcLastModifiedBy (\ s a -> s{_gcLastModifiedBy = a})

-- | Indicates whether the channel is configured with FCM or GCM credentials. Amazon Pinpoint uses your credentials to authenticate push notifications with FCM or GCM. Provide your credentials by setting the ApiKey attribute.
gcHasCredential :: Lens' GCMChannelResponse (Maybe Bool)
gcHasCredential = lens _gcHasCredential (\ s a -> s{_gcHasCredential = a})

instance FromJSON GCMChannelResponse where
        parseJSON
          = withObject "GCMChannelResponse"
              (\ x ->
                 GCMChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Enabled")
                     <*> (x .:? "Credential")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable GCMChannelResponse where

instance NFData GCMChannelResponse where

-- | GCM Message.
--
-- /See:/ 'gcmMessage' smart constructor.
data GCMMessage = GCMMessage'
  { _gmSubstitutions         :: !(Maybe (Map Text [Text]))
  , _gmSilentPush            :: !(Maybe Bool)
  , _gmImageIconURL          :: !(Maybe Text)
  , _gmPriority              :: !(Maybe Text)
  , _gmRawContent            :: !(Maybe Text)
  , _gmData                  :: !(Maybe (Map Text Text))
  , _gmRestrictedPackageName :: !(Maybe Text)
  , _gmSmallImageIconURL     :: !(Maybe Text)
  , _gmBody                  :: !(Maybe Text)
  , _gmTimeToLive            :: !(Maybe Int)
  , _gmURL                   :: !(Maybe Text)
  , _gmSound                 :: !(Maybe Text)
  , _gmAction                :: !(Maybe Action)
  , _gmCollapseKey           :: !(Maybe Text)
  , _gmImageURL              :: !(Maybe Text)
  , _gmTitle                 :: !(Maybe Text)
  , _gmIconReference         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GCMMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'gmSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'gmImageIconURL' - The URL that points to an image used as the large icon to the notification content view.
--
-- * 'gmPriority' - The message priority. Amazon Pinpoint uses this value to set the FCM or GCM priority parameter when it sends the message. Accepts the following values: "Normal" - Messages might be delayed. Delivery is optimized for battery usage on the receiving device. Use normal priority unless immediate delivery is required. "High" - Messages are sent immediately and might wake a sleeping device. The equivalent values for APNs messages are "5" and "10". Amazon Pinpoint accepts these values here and converts them. For more information, see About FCM Messages in the Firebase documentation.
--
-- * 'gmRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'gmData' - The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
--
-- * 'gmRestrictedPackageName' - This parameter specifies the package name of the application where the registration tokens must match in order to receive the message.
--
-- * 'gmSmallImageIconURL' - The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
--
-- * 'gmBody' - The message body of the notification, the email body or the text message.
--
-- * 'gmTimeToLive' - The length of time (in seconds) that FCM or GCM stores and attempts to deliver the message. If unspecified, the value defaults to the maximum, which is 2,419,200 seconds (28 days). Amazon Pinpoint uses this value to set the FCM or GCM time_to_live parameter.
--
-- * 'gmURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'gmSound' - Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
--
-- * 'gmAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
--
-- * 'gmCollapseKey' - This parameter identifies a group of messages (e.g., with collapse_key: "Updates Available") that can be collapsed, so that only the last message gets sent when delivery can be resumed. This is intended to avoid sending too many of the same messages when the device comes back online or becomes active.
--
-- * 'gmImageURL' - The URL that points to an image used in the push notification.
--
-- * 'gmTitle' - The message title that displays above the message on the user's device.
--
-- * 'gmIconReference' - The icon image name of the asset saved in your application.
gcmMessage
    :: GCMMessage
gcmMessage =
  GCMMessage'
    { _gmSubstitutions = Nothing
    , _gmSilentPush = Nothing
    , _gmImageIconURL = Nothing
    , _gmPriority = Nothing
    , _gmRawContent = Nothing
    , _gmData = Nothing
    , _gmRestrictedPackageName = Nothing
    , _gmSmallImageIconURL = Nothing
    , _gmBody = Nothing
    , _gmTimeToLive = Nothing
    , _gmURL = Nothing
    , _gmSound = Nothing
    , _gmAction = Nothing
    , _gmCollapseKey = Nothing
    , _gmImageURL = Nothing
    , _gmTitle = Nothing
    , _gmIconReference = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
gmSubstitutions :: Lens' GCMMessage (HashMap Text [Text])
gmSubstitutions = lens _gmSubstitutions (\ s a -> s{_gmSubstitutions = a}) . _Default . _Map

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
gmSilentPush :: Lens' GCMMessage (Maybe Bool)
gmSilentPush = lens _gmSilentPush (\ s a -> s{_gmSilentPush = a})

-- | The URL that points to an image used as the large icon to the notification content view.
gmImageIconURL :: Lens' GCMMessage (Maybe Text)
gmImageIconURL = lens _gmImageIconURL (\ s a -> s{_gmImageIconURL = a})

-- | The message priority. Amazon Pinpoint uses this value to set the FCM or GCM priority parameter when it sends the message. Accepts the following values: "Normal" - Messages might be delayed. Delivery is optimized for battery usage on the receiving device. Use normal priority unless immediate delivery is required. "High" - Messages are sent immediately and might wake a sleeping device. The equivalent values for APNs messages are "5" and "10". Amazon Pinpoint accepts these values here and converts them. For more information, see About FCM Messages in the Firebase documentation.
gmPriority :: Lens' GCMMessage (Maybe Text)
gmPriority = lens _gmPriority (\ s a -> s{_gmPriority = a})

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
gmRawContent :: Lens' GCMMessage (Maybe Text)
gmRawContent = lens _gmRawContent (\ s a -> s{_gmRawContent = a})

-- | The data payload used for a silent push. This payload is added to the notifications' data.pinpoint.jsonBody' object
gmData :: Lens' GCMMessage (HashMap Text Text)
gmData = lens _gmData (\ s a -> s{_gmData = a}) . _Default . _Map

-- | This parameter specifies the package name of the application where the registration tokens must match in order to receive the message.
gmRestrictedPackageName :: Lens' GCMMessage (Maybe Text)
gmRestrictedPackageName = lens _gmRestrictedPackageName (\ s a -> s{_gmRestrictedPackageName = a})

-- | The URL that points to an image used as the small icon for the notification which will be used to represent the notification in the status bar and content view
gmSmallImageIconURL :: Lens' GCMMessage (Maybe Text)
gmSmallImageIconURL = lens _gmSmallImageIconURL (\ s a -> s{_gmSmallImageIconURL = a})

-- | The message body of the notification, the email body or the text message.
gmBody :: Lens' GCMMessage (Maybe Text)
gmBody = lens _gmBody (\ s a -> s{_gmBody = a})

-- | The length of time (in seconds) that FCM or GCM stores and attempts to deliver the message. If unspecified, the value defaults to the maximum, which is 2,419,200 seconds (28 days). Amazon Pinpoint uses this value to set the FCM or GCM time_to_live parameter.
gmTimeToLive :: Lens' GCMMessage (Maybe Int)
gmTimeToLive = lens _gmTimeToLive (\ s a -> s{_gmTimeToLive = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
gmURL :: Lens' GCMMessage (Maybe Text)
gmURL = lens _gmURL (\ s a -> s{_gmURL = a})

-- | Indicates a sound to play when the device receives the notification. Supports default, or the filename of a sound resource bundled in the app. Android sound files must reside in /res/raw/
gmSound :: Lens' GCMMessage (Maybe Text)
gmSound = lens _gmSound (\ s a -> s{_gmSound = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify. Possible values include: OPEN_APP | DEEP_LINK | URL
gmAction :: Lens' GCMMessage (Maybe Action)
gmAction = lens _gmAction (\ s a -> s{_gmAction = a})

-- | This parameter identifies a group of messages (e.g., with collapse_key: "Updates Available") that can be collapsed, so that only the last message gets sent when delivery can be resumed. This is intended to avoid sending too many of the same messages when the device comes back online or becomes active.
gmCollapseKey :: Lens' GCMMessage (Maybe Text)
gmCollapseKey = lens _gmCollapseKey (\ s a -> s{_gmCollapseKey = a})

-- | The URL that points to an image used in the push notification.
gmImageURL :: Lens' GCMMessage (Maybe Text)
gmImageURL = lens _gmImageURL (\ s a -> s{_gmImageURL = a})

-- | The message title that displays above the message on the user's device.
gmTitle :: Lens' GCMMessage (Maybe Text)
gmTitle = lens _gmTitle (\ s a -> s{_gmTitle = a})

-- | The icon image name of the asset saved in your application.
gmIconReference :: Lens' GCMMessage (Maybe Text)
gmIconReference = lens _gmIconReference (\ s a -> s{_gmIconReference = a})

instance Hashable GCMMessage where

instance NFData GCMMessage where

instance ToJSON GCMMessage where
        toJSON GCMMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _gmSubstitutions,
                  ("SilentPush" .=) <$> _gmSilentPush,
                  ("ImageIconUrl" .=) <$> _gmImageIconURL,
                  ("Priority" .=) <$> _gmPriority,
                  ("RawContent" .=) <$> _gmRawContent,
                  ("Data" .=) <$> _gmData,
                  ("RestrictedPackageName" .=) <$>
                    _gmRestrictedPackageName,
                  ("SmallImageIconUrl" .=) <$> _gmSmallImageIconURL,
                  ("Body" .=) <$> _gmBody,
                  ("TimeToLive" .=) <$> _gmTimeToLive,
                  ("Url" .=) <$> _gmURL, ("Sound" .=) <$> _gmSound,
                  ("Action" .=) <$> _gmAction,
                  ("CollapseKey" .=) <$> _gmCollapseKey,
                  ("ImageUrl" .=) <$> _gmImageURL,
                  ("Title" .=) <$> _gmTitle,
                  ("IconReference" .=) <$> _gmIconReference])

-- | /See:/ 'importJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
  { _iSegmentName       :: !(Maybe Text)
  , _iFormat            :: !(Maybe DefinitionFormat)
  , _iDefineSegment     :: !(Maybe Bool)
  , _iRegisterEndpoints :: !(Maybe Bool)
  , _iExternalId        :: !(Maybe Text)
  , _iS3URL             :: !(Maybe Text)
  , _iSegmentId         :: !(Maybe Text)
  , _iRoleARN           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportJobRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iSegmentName' - A custom name for the segment created by the import job. Use if DefineSegment is true.
--
-- * 'iFormat' - The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
--
-- * 'iDefineSegment' - Sets whether the endpoints create a segment when they are imported.
--
-- * 'iRegisterEndpoints' - Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
--
-- * 'iExternalId' - DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
--
-- * 'iS3URL' - A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
--
-- * 'iSegmentId' - The ID of the segment to update if the import job is meant to update an existing segment.
--
-- * 'iRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
importJobRequest
    :: ImportJobRequest
importJobRequest =
  ImportJobRequest'
    { _iSegmentName = Nothing
    , _iFormat = Nothing
    , _iDefineSegment = Nothing
    , _iRegisterEndpoints = Nothing
    , _iExternalId = Nothing
    , _iS3URL = Nothing
    , _iSegmentId = Nothing
    , _iRoleARN = Nothing
    }


-- | A custom name for the segment created by the import job. Use if DefineSegment is true.
iSegmentName :: Lens' ImportJobRequest (Maybe Text)
iSegmentName = lens _iSegmentName (\ s a -> s{_iSegmentName = a})

-- | The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
iFormat :: Lens' ImportJobRequest (Maybe DefinitionFormat)
iFormat = lens _iFormat (\ s a -> s{_iFormat = a})

-- | Sets whether the endpoints create a segment when they are imported.
iDefineSegment :: Lens' ImportJobRequest (Maybe Bool)
iDefineSegment = lens _iDefineSegment (\ s a -> s{_iDefineSegment = a})

-- | Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
iRegisterEndpoints :: Lens' ImportJobRequest (Maybe Bool)
iRegisterEndpoints = lens _iRegisterEndpoints (\ s a -> s{_iRegisterEndpoints = a})

-- | DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
iExternalId :: Lens' ImportJobRequest (Maybe Text)
iExternalId = lens _iExternalId (\ s a -> s{_iExternalId = a})

-- | A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
iS3URL :: Lens' ImportJobRequest (Maybe Text)
iS3URL = lens _iS3URL (\ s a -> s{_iS3URL = a})

-- | The ID of the segment to update if the import job is meant to update an existing segment.
iSegmentId :: Lens' ImportJobRequest (Maybe Text)
iSegmentId = lens _iSegmentId (\ s a -> s{_iSegmentId = a})

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
iRoleARN :: Lens' ImportJobRequest (Maybe Text)
iRoleARN = lens _iRoleARN (\ s a -> s{_iRoleARN = a})

instance Hashable ImportJobRequest where

instance NFData ImportJobRequest where

instance ToJSON ImportJobRequest where
        toJSON ImportJobRequest'{..}
          = object
              (catMaybes
                 [("SegmentName" .=) <$> _iSegmentName,
                  ("Format" .=) <$> _iFormat,
                  ("DefineSegment" .=) <$> _iDefineSegment,
                  ("RegisterEndpoints" .=) <$> _iRegisterEndpoints,
                  ("ExternalId" .=) <$> _iExternalId,
                  ("S3Url" .=) <$> _iS3URL,
                  ("SegmentId" .=) <$> _iSegmentId,
                  ("RoleArn" .=) <$> _iRoleARN])

-- | /See:/ 'importJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
  { _ijrSegmentName       :: !(Maybe Text)
  , _ijrFormat            :: !(Maybe DefinitionFormat)
  , _ijrDefineSegment     :: !(Maybe Bool)
  , _ijrRegisterEndpoints :: !(Maybe Bool)
  , _ijrExternalId        :: !(Maybe Text)
  , _ijrS3URL             :: !(Maybe Text)
  , _ijrSegmentId         :: !(Maybe Text)
  , _ijrRoleARN           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportJobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijrSegmentName' - A custom name for the segment created by the import job. Use if DefineSegment is true.
--
-- * 'ijrFormat' - The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
--
-- * 'ijrDefineSegment' - Sets whether the endpoints create a segment when they are imported.
--
-- * 'ijrRegisterEndpoints' - Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
--
-- * 'ijrExternalId' - DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
--
-- * 'ijrS3URL' - A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
--
-- * 'ijrSegmentId' - The ID of the segment to update if the import job is meant to update an existing segment.
--
-- * 'ijrRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
importJobResource
    :: ImportJobResource
importJobResource =
  ImportJobResource'
    { _ijrSegmentName = Nothing
    , _ijrFormat = Nothing
    , _ijrDefineSegment = Nothing
    , _ijrRegisterEndpoints = Nothing
    , _ijrExternalId = Nothing
    , _ijrS3URL = Nothing
    , _ijrSegmentId = Nothing
    , _ijrRoleARN = Nothing
    }


-- | A custom name for the segment created by the import job. Use if DefineSegment is true.
ijrSegmentName :: Lens' ImportJobResource (Maybe Text)
ijrSegmentName = lens _ijrSegmentName (\ s a -> s{_ijrSegmentName = a})

-- | The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
ijrFormat :: Lens' ImportJobResource (Maybe DefinitionFormat)
ijrFormat = lens _ijrFormat (\ s a -> s{_ijrFormat = a})

-- | Sets whether the endpoints create a segment when they are imported.
ijrDefineSegment :: Lens' ImportJobResource (Maybe Bool)
ijrDefineSegment = lens _ijrDefineSegment (\ s a -> s{_ijrDefineSegment = a})

-- | Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
ijrRegisterEndpoints :: Lens' ImportJobResource (Maybe Bool)
ijrRegisterEndpoints = lens _ijrRegisterEndpoints (\ s a -> s{_ijrRegisterEndpoints = a})

-- | DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
ijrExternalId :: Lens' ImportJobResource (Maybe Text)
ijrExternalId = lens _ijrExternalId (\ s a -> s{_ijrExternalId = a})

-- | A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
ijrS3URL :: Lens' ImportJobResource (Maybe Text)
ijrS3URL = lens _ijrS3URL (\ s a -> s{_ijrS3URL = a})

-- | The ID of the segment to update if the import job is meant to update an existing segment.
ijrSegmentId :: Lens' ImportJobResource (Maybe Text)
ijrSegmentId = lens _ijrSegmentId (\ s a -> s{_ijrSegmentId = a})

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
ijrRoleARN :: Lens' ImportJobResource (Maybe Text)
ijrRoleARN = lens _ijrRoleARN (\ s a -> s{_ijrRoleARN = a})

instance FromJSON ImportJobResource where
        parseJSON
          = withObject "ImportJobResource"
              (\ x ->
                 ImportJobResource' <$>
                   (x .:? "SegmentName") <*> (x .:? "Format") <*>
                     (x .:? "DefineSegment")
                     <*> (x .:? "RegisterEndpoints")
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "S3Url")
                     <*> (x .:? "SegmentId")
                     <*> (x .:? "RoleArn"))

instance Hashable ImportJobResource where

instance NFData ImportJobResource where

-- | /See:/ 'importJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
  { _ijCompletedPieces :: !(Maybe Int)
  , _ijFailedPieces    :: !(Maybe Int)
  , _ijDefinition      :: !(Maybe ImportJobResource)
  , _ijTotalProcessed  :: !(Maybe Int)
  , _ijFailures        :: !(Maybe [Text])
  , _ijTotalPieces     :: !(Maybe Int)
  , _ijApplicationId   :: !(Maybe Text)
  , _ijId              :: !(Maybe Text)
  , _ijCreationDate    :: !(Maybe Text)
  , _ijType            :: !(Maybe Text)
  , _ijCompletionDate  :: !(Maybe Text)
  , _ijJobStatus       :: !(Maybe JobStatus)
  , _ijTotalFailures   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijCompletedPieces' - The number of pieces that have successfully imported as of the time of the request.
--
-- * 'ijFailedPieces' - The number of pieces that have failed to import as of the time of the request.
--
-- * 'ijDefinition' - The import job settings.
--
-- * 'ijTotalProcessed' - The number of endpoints that were processed by the import job.
--
-- * 'ijFailures' - Provides up to 100 of the first failed entries for the job, if any exist.
--
-- * 'ijTotalPieces' - The total number of pieces that must be imported to finish the job. Each piece is an approximately equal portion of the endpoints to import.
--
-- * 'ijApplicationId' - The unique ID of the application to which the import job applies.
--
-- * 'ijId' - The unique ID of the import job.
--
-- * 'ijCreationDate' - The date the import job was created in ISO 8601 format.
--
-- * 'ijType' - The job type. Will be Import.
--
-- * 'ijCompletionDate' - The date the import job completed in ISO 8601 format.
--
-- * 'ijJobStatus' - The status of the import job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed to import.
--
-- * 'ijTotalFailures' - The number of endpoints that failed to import; for example, because of syntax errors.
importJobResponse
    :: ImportJobResponse
importJobResponse =
  ImportJobResponse'
    { _ijCompletedPieces = Nothing
    , _ijFailedPieces = Nothing
    , _ijDefinition = Nothing
    , _ijTotalProcessed = Nothing
    , _ijFailures = Nothing
    , _ijTotalPieces = Nothing
    , _ijApplicationId = Nothing
    , _ijId = Nothing
    , _ijCreationDate = Nothing
    , _ijType = Nothing
    , _ijCompletionDate = Nothing
    , _ijJobStatus = Nothing
    , _ijTotalFailures = Nothing
    }


-- | The number of pieces that have successfully imported as of the time of the request.
ijCompletedPieces :: Lens' ImportJobResponse (Maybe Int)
ijCompletedPieces = lens _ijCompletedPieces (\ s a -> s{_ijCompletedPieces = a})

-- | The number of pieces that have failed to import as of the time of the request.
ijFailedPieces :: Lens' ImportJobResponse (Maybe Int)
ijFailedPieces = lens _ijFailedPieces (\ s a -> s{_ijFailedPieces = a})

-- | The import job settings.
ijDefinition :: Lens' ImportJobResponse (Maybe ImportJobResource)
ijDefinition = lens _ijDefinition (\ s a -> s{_ijDefinition = a})

-- | The number of endpoints that were processed by the import job.
ijTotalProcessed :: Lens' ImportJobResponse (Maybe Int)
ijTotalProcessed = lens _ijTotalProcessed (\ s a -> s{_ijTotalProcessed = a})

-- | Provides up to 100 of the first failed entries for the job, if any exist.
ijFailures :: Lens' ImportJobResponse [Text]
ijFailures = lens _ijFailures (\ s a -> s{_ijFailures = a}) . _Default . _Coerce

-- | The total number of pieces that must be imported to finish the job. Each piece is an approximately equal portion of the endpoints to import.
ijTotalPieces :: Lens' ImportJobResponse (Maybe Int)
ijTotalPieces = lens _ijTotalPieces (\ s a -> s{_ijTotalPieces = a})

-- | The unique ID of the application to which the import job applies.
ijApplicationId :: Lens' ImportJobResponse (Maybe Text)
ijApplicationId = lens _ijApplicationId (\ s a -> s{_ijApplicationId = a})

-- | The unique ID of the import job.
ijId :: Lens' ImportJobResponse (Maybe Text)
ijId = lens _ijId (\ s a -> s{_ijId = a})

-- | The date the import job was created in ISO 8601 format.
ijCreationDate :: Lens' ImportJobResponse (Maybe Text)
ijCreationDate = lens _ijCreationDate (\ s a -> s{_ijCreationDate = a})

-- | The job type. Will be Import.
ijType :: Lens' ImportJobResponse (Maybe Text)
ijType = lens _ijType (\ s a -> s{_ijType = a})

-- | The date the import job completed in ISO 8601 format.
ijCompletionDate :: Lens' ImportJobResponse (Maybe Text)
ijCompletionDate = lens _ijCompletionDate (\ s a -> s{_ijCompletionDate = a})

-- | The status of the import job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed to import.
ijJobStatus :: Lens' ImportJobResponse (Maybe JobStatus)
ijJobStatus = lens _ijJobStatus (\ s a -> s{_ijJobStatus = a})

-- | The number of endpoints that failed to import; for example, because of syntax errors.
ijTotalFailures :: Lens' ImportJobResponse (Maybe Int)
ijTotalFailures = lens _ijTotalFailures (\ s a -> s{_ijTotalFailures = a})

instance FromJSON ImportJobResponse where
        parseJSON
          = withObject "ImportJobResponse"
              (\ x ->
                 ImportJobResponse' <$>
                   (x .:? "CompletedPieces") <*> (x .:? "FailedPieces")
                     <*> (x .:? "Definition")
                     <*> (x .:? "TotalProcessed")
                     <*> (x .:? "Failures" .!= mempty)
                     <*> (x .:? "TotalPieces")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "JobStatus")
                     <*> (x .:? "TotalFailures"))

instance Hashable ImportJobResponse where

instance NFData ImportJobResponse where

-- | Import job list.
--
-- /See:/ 'importJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
  { _ijNextToken :: !(Maybe Text)
  , _ijItem      :: !(Maybe [ImportJobResponse])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'ijItem' - A list of import jobs for the application.
importJobsResponse
    :: ImportJobsResponse
importJobsResponse =
  ImportJobsResponse' {_ijNextToken = Nothing, _ijItem = Nothing}


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
ijNextToken :: Lens' ImportJobsResponse (Maybe Text)
ijNextToken = lens _ijNextToken (\ s a -> s{_ijNextToken = a})

-- | A list of import jobs for the application.
ijItem :: Lens' ImportJobsResponse [ImportJobResponse]
ijItem = lens _ijItem (\ s a -> s{_ijItem = a}) . _Default . _Coerce

instance FromJSON ImportJobsResponse where
        parseJSON
          = withObject "ImportJobsResponse"
              (\ x ->
                 ImportJobsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable ImportJobsResponse where

instance NFData ImportJobsResponse where

-- | /See:/ 'message' smart constructor.
data Message = Message'
  { _mSilentPush        :: !(Maybe Bool)
  , _mImageIconURL      :: !(Maybe Text)
  , _mRawContent        :: !(Maybe Text)
  , _mBody              :: !(Maybe Text)
  , _mImageSmallIconURL :: !(Maybe Text)
  , _mJSONBody          :: !(Maybe Text)
  , _mURL               :: !(Maybe Text)
  , _mAction            :: !(Maybe Action)
  , _mImageURL          :: !(Maybe Text)
  , _mMediaURL          :: !(Maybe Text)
  , _mTitle             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'mImageIconURL' - The URL that points to the icon image for the push notification icon, for example, the app icon.
--
-- * 'mRawContent' - The Raw JSON formatted string to be used as the payload. This value overrides the message.
--
-- * 'mBody' - The message body. Can include up to 140 characters.
--
-- * 'mImageSmallIconURL' - The URL that points to the small icon image for the push notification icon, for example, the app icon.
--
-- * 'mJSONBody' - The JSON payload used for a silent push.
--
-- * 'mURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'mAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify.
--
-- * 'mImageURL' - The URL that points to an image used in the push notification.
--
-- * 'mMediaURL' - The URL that points to the media resource, for example a .mp4 or .gif file.
--
-- * 'mTitle' - The message title that displays above the message on the user's device.
message
    :: Message
message =
  Message'
    { _mSilentPush = Nothing
    , _mImageIconURL = Nothing
    , _mRawContent = Nothing
    , _mBody = Nothing
    , _mImageSmallIconURL = Nothing
    , _mJSONBody = Nothing
    , _mURL = Nothing
    , _mAction = Nothing
    , _mImageURL = Nothing
    , _mMediaURL = Nothing
    , _mTitle = Nothing
    }


-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
mSilentPush :: Lens' Message (Maybe Bool)
mSilentPush = lens _mSilentPush (\ s a -> s{_mSilentPush = a})

-- | The URL that points to the icon image for the push notification icon, for example, the app icon.
mImageIconURL :: Lens' Message (Maybe Text)
mImageIconURL = lens _mImageIconURL (\ s a -> s{_mImageIconURL = a})

-- | The Raw JSON formatted string to be used as the payload. This value overrides the message.
mRawContent :: Lens' Message (Maybe Text)
mRawContent = lens _mRawContent (\ s a -> s{_mRawContent = a})

-- | The message body. Can include up to 140 characters.
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\ s a -> s{_mBody = a})

-- | The URL that points to the small icon image for the push notification icon, for example, the app icon.
mImageSmallIconURL :: Lens' Message (Maybe Text)
mImageSmallIconURL = lens _mImageSmallIconURL (\ s a -> s{_mImageSmallIconURL = a})

-- | The JSON payload used for a silent push.
mJSONBody :: Lens' Message (Maybe Text)
mJSONBody = lens _mJSONBody (\ s a -> s{_mJSONBody = a})

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
mURL :: Lens' Message (Maybe Text)
mURL = lens _mURL (\ s a -> s{_mURL = a})

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP - Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK - Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL - The default mobile browser on the user's device launches and opens a web page at the URL you specify.
mAction :: Lens' Message (Maybe Action)
mAction = lens _mAction (\ s a -> s{_mAction = a})

-- | The URL that points to an image used in the push notification.
mImageURL :: Lens' Message (Maybe Text)
mImageURL = lens _mImageURL (\ s a -> s{_mImageURL = a})

-- | The URL that points to the media resource, for example a .mp4 or .gif file.
mMediaURL :: Lens' Message (Maybe Text)
mMediaURL = lens _mMediaURL (\ s a -> s{_mMediaURL = a})

-- | The message title that displays above the message on the user's device.
mTitle :: Lens' Message (Maybe Text)
mTitle = lens _mTitle (\ s a -> s{_mTitle = a})

instance FromJSON Message where
        parseJSON
          = withObject "Message"
              (\ x ->
                 Message' <$>
                   (x .:? "SilentPush") <*> (x .:? "ImageIconUrl") <*>
                     (x .:? "RawContent")
                     <*> (x .:? "Body")
                     <*> (x .:? "ImageSmallIconUrl")
                     <*> (x .:? "JsonBody")
                     <*> (x .:? "Url")
                     <*> (x .:? "Action")
                     <*> (x .:? "ImageUrl")
                     <*> (x .:? "MediaUrl")
                     <*> (x .:? "Title"))

instance Hashable Message where

instance NFData Message where

instance ToJSON Message where
        toJSON Message'{..}
          = object
              (catMaybes
                 [("SilentPush" .=) <$> _mSilentPush,
                  ("ImageIconUrl" .=) <$> _mImageIconURL,
                  ("RawContent" .=) <$> _mRawContent,
                  ("Body" .=) <$> _mBody,
                  ("ImageSmallIconUrl" .=) <$> _mImageSmallIconURL,
                  ("JsonBody" .=) <$> _mJSONBody, ("Url" .=) <$> _mURL,
                  ("Action" .=) <$> _mAction,
                  ("ImageUrl" .=) <$> _mImageURL,
                  ("MediaUrl" .=) <$> _mMediaURL,
                  ("Title" .=) <$> _mTitle])

-- | Simple message object.
--
-- /See:/ 'messageBody' smart constructor.
data MessageBody = MessageBody'
  { _mbRequestId :: !(Maybe Text)
  , _mbMessage   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageBody' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbRequestId' - The unique message body ID.
--
-- * 'mbMessage' - The error message returned from the API.
messageBody
    :: MessageBody
messageBody = MessageBody' {_mbRequestId = Nothing, _mbMessage = Nothing}


-- | The unique message body ID.
mbRequestId :: Lens' MessageBody (Maybe Text)
mbRequestId = lens _mbRequestId (\ s a -> s{_mbRequestId = a})

-- | The error message returned from the API.
mbMessage :: Lens' MessageBody (Maybe Text)
mbMessage = lens _mbMessage (\ s a -> s{_mbMessage = a})

instance FromJSON MessageBody where
        parseJSON
          = withObject "MessageBody"
              (\ x ->
                 MessageBody' <$>
                   (x .:? "RequestID") <*> (x .:? "Message"))

instance Hashable MessageBody where

instance NFData MessageBody where

-- | Message configuration for a campaign.
--
-- /See:/ 'messageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
  { _mcAPNSMessage    :: !(Maybe Message)
  , _mcGCMMessage     :: !(Maybe Message)
  , _mcDefaultMessage :: !(Maybe Message)
  , _mcADMMessage     :: !(Maybe Message)
  , _mcSMSMessage     :: !(Maybe CampaignSmsMessage)
  , _mcEmailMessage   :: !(Maybe CampaignEmailMessage)
  , _mcBaiduMessage   :: !(Maybe Message)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcAPNSMessage' - The message that the campaign delivers to APNS channels. Overrides the default message.
--
-- * 'mcGCMMessage' - The message that the campaign delivers to GCM channels. Overrides the default message.
--
-- * 'mcDefaultMessage' - The default message for all channels.
--
-- * 'mcADMMessage' - The message that the campaign delivers to ADM channels. Overrides the default message.
--
-- * 'mcSMSMessage' - The SMS message configuration.
--
-- * 'mcEmailMessage' - The email message configuration.
--
-- * 'mcBaiduMessage' - The message that the campaign delivers to Baidu channels. Overrides the default message.
messageConfiguration
    :: MessageConfiguration
messageConfiguration =
  MessageConfiguration'
    { _mcAPNSMessage = Nothing
    , _mcGCMMessage = Nothing
    , _mcDefaultMessage = Nothing
    , _mcADMMessage = Nothing
    , _mcSMSMessage = Nothing
    , _mcEmailMessage = Nothing
    , _mcBaiduMessage = Nothing
    }


-- | The message that the campaign delivers to APNS channels. Overrides the default message.
mcAPNSMessage :: Lens' MessageConfiguration (Maybe Message)
mcAPNSMessage = lens _mcAPNSMessage (\ s a -> s{_mcAPNSMessage = a})

-- | The message that the campaign delivers to GCM channels. Overrides the default message.
mcGCMMessage :: Lens' MessageConfiguration (Maybe Message)
mcGCMMessage = lens _mcGCMMessage (\ s a -> s{_mcGCMMessage = a})

-- | The default message for all channels.
mcDefaultMessage :: Lens' MessageConfiguration (Maybe Message)
mcDefaultMessage = lens _mcDefaultMessage (\ s a -> s{_mcDefaultMessage = a})

-- | The message that the campaign delivers to ADM channels. Overrides the default message.
mcADMMessage :: Lens' MessageConfiguration (Maybe Message)
mcADMMessage = lens _mcADMMessage (\ s a -> s{_mcADMMessage = a})

-- | The SMS message configuration.
mcSMSMessage :: Lens' MessageConfiguration (Maybe CampaignSmsMessage)
mcSMSMessage = lens _mcSMSMessage (\ s a -> s{_mcSMSMessage = a})

-- | The email message configuration.
mcEmailMessage :: Lens' MessageConfiguration (Maybe CampaignEmailMessage)
mcEmailMessage = lens _mcEmailMessage (\ s a -> s{_mcEmailMessage = a})

-- | The message that the campaign delivers to Baidu channels. Overrides the default message.
mcBaiduMessage :: Lens' MessageConfiguration (Maybe Message)
mcBaiduMessage = lens _mcBaiduMessage (\ s a -> s{_mcBaiduMessage = a})

instance FromJSON MessageConfiguration where
        parseJSON
          = withObject "MessageConfiguration"
              (\ x ->
                 MessageConfiguration' <$>
                   (x .:? "APNSMessage") <*> (x .:? "GCMMessage") <*>
                     (x .:? "DefaultMessage")
                     <*> (x .:? "ADMMessage")
                     <*> (x .:? "SMSMessage")
                     <*> (x .:? "EmailMessage")
                     <*> (x .:? "BaiduMessage"))

instance Hashable MessageConfiguration where

instance NFData MessageConfiguration where

instance ToJSON MessageConfiguration where
        toJSON MessageConfiguration'{..}
          = object
              (catMaybes
                 [("APNSMessage" .=) <$> _mcAPNSMessage,
                  ("GCMMessage" .=) <$> _mcGCMMessage,
                  ("DefaultMessage" .=) <$> _mcDefaultMessage,
                  ("ADMMessage" .=) <$> _mcADMMessage,
                  ("SMSMessage" .=) <$> _mcSMSMessage,
                  ("EmailMessage" .=) <$> _mcEmailMessage,
                  ("BaiduMessage" .=) <$> _mcBaiduMessage])

-- | Send message request.
--
-- /See:/ 'messageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { _mrContext              :: !(Maybe (Map Text Text))
  , _mrAddresses            :: !(Maybe (Map Text AddressConfiguration))
  , _mrEndpoints            :: !(Maybe (Map Text EndpointSendConfiguration))
  , _mrMessageConfiguration :: !(Maybe DirectMessageConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrContext' - A map of custom attributes to attributes to be attached to the message. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
--
-- * 'mrAddresses' - A map of destination addresses, with the address as the key(Email address, phone number or push token) and the Address Configuration as the value.
--
-- * 'mrEndpoints' - A map of destination addresses, with the address as the key(Email address, phone number or push token) and the Address Configuration as the value.
--
-- * 'mrMessageConfiguration' - Message configuration.
messageRequest
    :: MessageRequest
messageRequest =
  MessageRequest'
    { _mrContext = Nothing
    , _mrAddresses = Nothing
    , _mrEndpoints = Nothing
    , _mrMessageConfiguration = Nothing
    }


-- | A map of custom attributes to attributes to be attached to the message. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
mrContext :: Lens' MessageRequest (HashMap Text Text)
mrContext = lens _mrContext (\ s a -> s{_mrContext = a}) . _Default . _Map

-- | A map of destination addresses, with the address as the key(Email address, phone number or push token) and the Address Configuration as the value.
mrAddresses :: Lens' MessageRequest (HashMap Text AddressConfiguration)
mrAddresses = lens _mrAddresses (\ s a -> s{_mrAddresses = a}) . _Default . _Map

-- | A map of destination addresses, with the address as the key(Email address, phone number or push token) and the Address Configuration as the value.
mrEndpoints :: Lens' MessageRequest (HashMap Text EndpointSendConfiguration)
mrEndpoints = lens _mrEndpoints (\ s a -> s{_mrEndpoints = a}) . _Default . _Map

-- | Message configuration.
mrMessageConfiguration :: Lens' MessageRequest (Maybe DirectMessageConfiguration)
mrMessageConfiguration = lens _mrMessageConfiguration (\ s a -> s{_mrMessageConfiguration = a})

instance Hashable MessageRequest where

instance NFData MessageRequest where

instance ToJSON MessageRequest where
        toJSON MessageRequest'{..}
          = object
              (catMaybes
                 [("Context" .=) <$> _mrContext,
                  ("Addresses" .=) <$> _mrAddresses,
                  ("Endpoints" .=) <$> _mrEndpoints,
                  ("MessageConfiguration" .=) <$>
                    _mrMessageConfiguration])

-- | Send message response.
--
-- /See:/ 'messageResponse' smart constructor.
data MessageResponse = MessageResponse'
  { _mRequestId      :: !(Maybe Text)
  , _mResult         :: !(Maybe (Map Text MessageResult))
  , _mApplicationId  :: !(Maybe Text)
  , _mEndpointResult :: !(Maybe (Map Text EndpointMessageResult))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mRequestId' - Original request Id for which this message was delivered.
--
-- * 'mResult' - A map containing a multi part response for each address, with the address as the key(Email address, phone number or push token) and the result as the value.
--
-- * 'mApplicationId' - Application id of the message.
--
-- * 'mEndpointResult' - A map containing a multi part response for each address, with the endpointId as the key and the result as the value.
messageResponse
    :: MessageResponse
messageResponse =
  MessageResponse'
    { _mRequestId = Nothing
    , _mResult = Nothing
    , _mApplicationId = Nothing
    , _mEndpointResult = Nothing
    }


-- | Original request Id for which this message was delivered.
mRequestId :: Lens' MessageResponse (Maybe Text)
mRequestId = lens _mRequestId (\ s a -> s{_mRequestId = a})

-- | A map containing a multi part response for each address, with the address as the key(Email address, phone number or push token) and the result as the value.
mResult :: Lens' MessageResponse (HashMap Text MessageResult)
mResult = lens _mResult (\ s a -> s{_mResult = a}) . _Default . _Map

-- | Application id of the message.
mApplicationId :: Lens' MessageResponse (Maybe Text)
mApplicationId = lens _mApplicationId (\ s a -> s{_mApplicationId = a})

-- | A map containing a multi part response for each address, with the endpointId as the key and the result as the value.
mEndpointResult :: Lens' MessageResponse (HashMap Text EndpointMessageResult)
mEndpointResult = lens _mEndpointResult (\ s a -> s{_mEndpointResult = a}) . _Default . _Map

instance FromJSON MessageResponse where
        parseJSON
          = withObject "MessageResponse"
              (\ x ->
                 MessageResponse' <$>
                   (x .:? "RequestId") <*> (x .:? "Result" .!= mempty)
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "EndpointResult" .!= mempty))

instance Hashable MessageResponse where

instance NFData MessageResponse where

-- | The result from sending a message to an address.
--
-- /See:/ 'messageResult' smart constructor.
data MessageResult = MessageResult'
  { _mrDeliveryStatus :: !(Maybe DeliveryStatus)
  , _mrStatusMessage  :: !(Maybe Text)
  , _mrUpdatedToken   :: !(Maybe Text)
  , _mrStatusCode     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrDeliveryStatus' - Delivery status of message.
--
-- * 'mrStatusMessage' - Status message for message delivery.
--
-- * 'mrUpdatedToken' - If token was updated as part of delivery. (This is GCM Specific)
--
-- * 'mrStatusCode' - Downstream service status code.
messageResult
    :: MessageResult
messageResult =
  MessageResult'
    { _mrDeliveryStatus = Nothing
    , _mrStatusMessage = Nothing
    , _mrUpdatedToken = Nothing
    , _mrStatusCode = Nothing
    }


-- | Delivery status of message.
mrDeliveryStatus :: Lens' MessageResult (Maybe DeliveryStatus)
mrDeliveryStatus = lens _mrDeliveryStatus (\ s a -> s{_mrDeliveryStatus = a})

-- | Status message for message delivery.
mrStatusMessage :: Lens' MessageResult (Maybe Text)
mrStatusMessage = lens _mrStatusMessage (\ s a -> s{_mrStatusMessage = a})

-- | If token was updated as part of delivery. (This is GCM Specific)
mrUpdatedToken :: Lens' MessageResult (Maybe Text)
mrUpdatedToken = lens _mrUpdatedToken (\ s a -> s{_mrUpdatedToken = a})

-- | Downstream service status code.
mrStatusCode :: Lens' MessageResult (Maybe Int)
mrStatusCode = lens _mrStatusCode (\ s a -> s{_mrStatusCode = a})

instance FromJSON MessageResult where
        parseJSON
          = withObject "MessageResult"
              (\ x ->
                 MessageResult' <$>
                   (x .:? "DeliveryStatus") <*> (x .:? "StatusMessage")
                     <*> (x .:? "UpdatedToken")
                     <*> (x .:? "StatusCode"))

instance Hashable MessageResult where

instance NFData MessageResult where

-- | Quiet Time
--
-- /See:/ 'quietTime' smart constructor.
data QuietTime = QuietTime'
  { _qtStart :: !(Maybe Text)
  , _qtEnd   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QuietTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtStart' - The default start time for quiet time in ISO 8601 format.
--
-- * 'qtEnd' - The default end time for quiet time in ISO 8601 format.
quietTime
    :: QuietTime
quietTime = QuietTime' {_qtStart = Nothing, _qtEnd = Nothing}


-- | The default start time for quiet time in ISO 8601 format.
qtStart :: Lens' QuietTime (Maybe Text)
qtStart = lens _qtStart (\ s a -> s{_qtStart = a})

-- | The default end time for quiet time in ISO 8601 format.
qtEnd :: Lens' QuietTime (Maybe Text)
qtEnd = lens _qtEnd (\ s a -> s{_qtEnd = a})

instance FromJSON QuietTime where
        parseJSON
          = withObject "QuietTime"
              (\ x ->
                 QuietTime' <$> (x .:? "Start") <*> (x .:? "End"))

instance Hashable QuietTime where

instance NFData QuietTime where

instance ToJSON QuietTime where
        toJSON QuietTime'{..}
          = object
              (catMaybes
                 [("Start" .=) <$> _qtStart, ("End" .=) <$> _qtEnd])

-- | Define how a segment based on recency of use.
--
-- /See:/ 'recencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
  { _rdRecencyType :: !(Maybe RecencyType)
  , _rdDuration    :: !(Maybe Duration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecencyDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdRecencyType' - The recency dimension type: ACTIVE - Users who have used your app within the specified duration are included in the segment. INACTIVE - Users who have not used your app within the specified duration are included in the segment.
--
-- * 'rdDuration' - The length of time during which users have been active or inactive with your app. Valid values: HR_24, DAY_7, DAY_14, DAY_30
recencyDimension
    :: RecencyDimension
recencyDimension =
  RecencyDimension' {_rdRecencyType = Nothing, _rdDuration = Nothing}


-- | The recency dimension type: ACTIVE - Users who have used your app within the specified duration are included in the segment. INACTIVE - Users who have not used your app within the specified duration are included in the segment.
rdRecencyType :: Lens' RecencyDimension (Maybe RecencyType)
rdRecencyType = lens _rdRecencyType (\ s a -> s{_rdRecencyType = a})

-- | The length of time during which users have been active or inactive with your app. Valid values: HR_24, DAY_7, DAY_14, DAY_30
rdDuration :: Lens' RecencyDimension (Maybe Duration)
rdDuration = lens _rdDuration (\ s a -> s{_rdDuration = a})

instance FromJSON RecencyDimension where
        parseJSON
          = withObject "RecencyDimension"
              (\ x ->
                 RecencyDimension' <$>
                   (x .:? "RecencyType") <*> (x .:? "Duration"))

instance Hashable RecencyDimension where

instance NFData RecencyDimension where

instance ToJSON RecencyDimension where
        toJSON RecencyDimension'{..}
          = object
              (catMaybes
                 [("RecencyType" .=) <$> _rdRecencyType,
                  ("Duration" .=) <$> _rdDuration])

-- | SMS Channel Request
--
-- /See:/ 'sMSChannelRequest' smart constructor.
data SMSChannelRequest = SMSChannelRequest'
  { _smscrShortCode :: !(Maybe Text)
  , _smscrEnabled   :: !(Maybe Bool)
  , _smscrSenderId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SMSChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smscrShortCode' - ShortCode registered with phone provider.
--
-- * 'smscrEnabled' - If the channel is enabled for sending messages.
--
-- * 'smscrSenderId' - Sender identifier of your messages.
sMSChannelRequest
    :: SMSChannelRequest
sMSChannelRequest =
  SMSChannelRequest'
    { _smscrShortCode = Nothing
    , _smscrEnabled = Nothing
    , _smscrSenderId = Nothing
    }


-- | ShortCode registered with phone provider.
smscrShortCode :: Lens' SMSChannelRequest (Maybe Text)
smscrShortCode = lens _smscrShortCode (\ s a -> s{_smscrShortCode = a})

-- | If the channel is enabled for sending messages.
smscrEnabled :: Lens' SMSChannelRequest (Maybe Bool)
smscrEnabled = lens _smscrEnabled (\ s a -> s{_smscrEnabled = a})

-- | Sender identifier of your messages.
smscrSenderId :: Lens' SMSChannelRequest (Maybe Text)
smscrSenderId = lens _smscrSenderId (\ s a -> s{_smscrSenderId = a})

instance Hashable SMSChannelRequest where

instance NFData SMSChannelRequest where

instance ToJSON SMSChannelRequest where
        toJSON SMSChannelRequest'{..}
          = object
              (catMaybes
                 [("ShortCode" .=) <$> _smscrShortCode,
                  ("Enabled" .=) <$> _smscrEnabled,
                  ("SenderId" .=) <$> _smscrSenderId])

-- | SMS Channel Response.
--
-- /See:/ 'sMSChannelResponse' smart constructor.
data SMSChannelResponse = SMSChannelResponse'
  { _smscPlatform         :: !(Maybe Text)
  , _smscShortCode        :: !(Maybe Text)
  , _smscLastModifiedDate :: !(Maybe Text)
  , _smscEnabled          :: !(Maybe Bool)
  , _smscSenderId         :: !(Maybe Text)
  , _smscIsArchived       :: !(Maybe Bool)
  , _smscApplicationId    :: !(Maybe Text)
  , _smscVersion          :: !(Maybe Int)
  , _smscId               :: !(Maybe Text)
  , _smscCreationDate     :: !(Maybe Text)
  , _smscLastModifiedBy   :: !(Maybe Text)
  , _smscHasCredential    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SMSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smscPlatform' - Platform type. Will be "SMS"
--
-- * 'smscShortCode' - The short code registered with the phone provider.
--
-- * 'smscLastModifiedDate' - Last date this was updated
--
-- * 'smscEnabled' - If the channel is enabled for sending messages.
--
-- * 'smscSenderId' - Sender identifier of your messages.
--
-- * 'smscIsArchived' - Is this channel archived
--
-- * 'smscApplicationId' - The unique ID of the application to which the SMS channel belongs.
--
-- * 'smscVersion' - Version of channel
--
-- * 'smscId' - Channel ID. Not used, only for backwards compatibility.
--
-- * 'smscCreationDate' - The date that the settings were last updated in ISO 8601 format.
--
-- * 'smscLastModifiedBy' - Who last updated this entry
--
-- * 'smscHasCredential' - If the channel is registered with a credential for authentication.
sMSChannelResponse
    :: SMSChannelResponse
sMSChannelResponse =
  SMSChannelResponse'
    { _smscPlatform = Nothing
    , _smscShortCode = Nothing
    , _smscLastModifiedDate = Nothing
    , _smscEnabled = Nothing
    , _smscSenderId = Nothing
    , _smscIsArchived = Nothing
    , _smscApplicationId = Nothing
    , _smscVersion = Nothing
    , _smscId = Nothing
    , _smscCreationDate = Nothing
    , _smscLastModifiedBy = Nothing
    , _smscHasCredential = Nothing
    }


-- | Platform type. Will be "SMS"
smscPlatform :: Lens' SMSChannelResponse (Maybe Text)
smscPlatform = lens _smscPlatform (\ s a -> s{_smscPlatform = a})

-- | The short code registered with the phone provider.
smscShortCode :: Lens' SMSChannelResponse (Maybe Text)
smscShortCode = lens _smscShortCode (\ s a -> s{_smscShortCode = a})

-- | Last date this was updated
smscLastModifiedDate :: Lens' SMSChannelResponse (Maybe Text)
smscLastModifiedDate = lens _smscLastModifiedDate (\ s a -> s{_smscLastModifiedDate = a})

-- | If the channel is enabled for sending messages.
smscEnabled :: Lens' SMSChannelResponse (Maybe Bool)
smscEnabled = lens _smscEnabled (\ s a -> s{_smscEnabled = a})

-- | Sender identifier of your messages.
smscSenderId :: Lens' SMSChannelResponse (Maybe Text)
smscSenderId = lens _smscSenderId (\ s a -> s{_smscSenderId = a})

-- | Is this channel archived
smscIsArchived :: Lens' SMSChannelResponse (Maybe Bool)
smscIsArchived = lens _smscIsArchived (\ s a -> s{_smscIsArchived = a})

-- | The unique ID of the application to which the SMS channel belongs.
smscApplicationId :: Lens' SMSChannelResponse (Maybe Text)
smscApplicationId = lens _smscApplicationId (\ s a -> s{_smscApplicationId = a})

-- | Version of channel
smscVersion :: Lens' SMSChannelResponse (Maybe Int)
smscVersion = lens _smscVersion (\ s a -> s{_smscVersion = a})

-- | Channel ID. Not used, only for backwards compatibility.
smscId :: Lens' SMSChannelResponse (Maybe Text)
smscId = lens _smscId (\ s a -> s{_smscId = a})

-- | The date that the settings were last updated in ISO 8601 format.
smscCreationDate :: Lens' SMSChannelResponse (Maybe Text)
smscCreationDate = lens _smscCreationDate (\ s a -> s{_smscCreationDate = a})

-- | Who last updated this entry
smscLastModifiedBy :: Lens' SMSChannelResponse (Maybe Text)
smscLastModifiedBy = lens _smscLastModifiedBy (\ s a -> s{_smscLastModifiedBy = a})

-- | If the channel is registered with a credential for authentication.
smscHasCredential :: Lens' SMSChannelResponse (Maybe Bool)
smscHasCredential = lens _smscHasCredential (\ s a -> s{_smscHasCredential = a})

instance FromJSON SMSChannelResponse where
        parseJSON
          = withObject "SMSChannelResponse"
              (\ x ->
                 SMSChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "ShortCode") <*>
                     (x .:? "LastModifiedDate")
                     <*> (x .:? "Enabled")
                     <*> (x .:? "SenderId")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy")
                     <*> (x .:? "HasCredential"))

instance Hashable SMSChannelResponse where

instance NFData SMSChannelResponse where

-- | SMS Message.
--
-- /See:/ 'sMSMessage' smart constructor.
data SMSMessage = SMSMessage'
  { _smsmSubstitutions     :: !(Maybe (Map Text [Text]))
  , _smsmOriginationNumber :: !(Maybe Text)
  , _smsmBody              :: !(Maybe Text)
  , _smsmMessageType       :: !(Maybe MessageType)
  , _smsmSenderId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SMSMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsmSubstitutions' - Default message substitutions. Can be overridden by individual address substitutions.
--
-- * 'smsmOriginationNumber' - The phone number that the SMS message originates from. Specify one of the dedicated long codes or short codes that you requested from AWS Support and that is assigned to your account. If this attribute is not specified, Amazon Pinpoint randomly assigns a long code.
--
-- * 'smsmBody' - The message body of the notification, the email body or the text message.
--
-- * 'smsmMessageType' - Is this a transaction priority message or lower priority.
--
-- * 'smsmSenderId' - The sender ID that is shown as the message sender on the recipient's device. Support for sender IDs varies by country or region.
sMSMessage
    :: SMSMessage
sMSMessage =
  SMSMessage'
    { _smsmSubstitutions = Nothing
    , _smsmOriginationNumber = Nothing
    , _smsmBody = Nothing
    , _smsmMessageType = Nothing
    , _smsmSenderId = Nothing
    }


-- | Default message substitutions. Can be overridden by individual address substitutions.
smsmSubstitutions :: Lens' SMSMessage (HashMap Text [Text])
smsmSubstitutions = lens _smsmSubstitutions (\ s a -> s{_smsmSubstitutions = a}) . _Default . _Map

-- | The phone number that the SMS message originates from. Specify one of the dedicated long codes or short codes that you requested from AWS Support and that is assigned to your account. If this attribute is not specified, Amazon Pinpoint randomly assigns a long code.
smsmOriginationNumber :: Lens' SMSMessage (Maybe Text)
smsmOriginationNumber = lens _smsmOriginationNumber (\ s a -> s{_smsmOriginationNumber = a})

-- | The message body of the notification, the email body or the text message.
smsmBody :: Lens' SMSMessage (Maybe Text)
smsmBody = lens _smsmBody (\ s a -> s{_smsmBody = a})

-- | Is this a transaction priority message or lower priority.
smsmMessageType :: Lens' SMSMessage (Maybe MessageType)
smsmMessageType = lens _smsmMessageType (\ s a -> s{_smsmMessageType = a})

-- | The sender ID that is shown as the message sender on the recipient's device. Support for sender IDs varies by country or region.
smsmSenderId :: Lens' SMSMessage (Maybe Text)
smsmSenderId = lens _smsmSenderId (\ s a -> s{_smsmSenderId = a})

instance Hashable SMSMessage where

instance NFData SMSMessage where

instance ToJSON SMSMessage where
        toJSON SMSMessage'{..}
          = object
              (catMaybes
                 [("Substitutions" .=) <$> _smsmSubstitutions,
                  ("OriginationNumber" .=) <$> _smsmOriginationNumber,
                  ("Body" .=) <$> _smsmBody,
                  ("MessageType" .=) <$> _smsmMessageType,
                  ("SenderId" .=) <$> _smsmSenderId])

-- | Shcedule that defines when a campaign is run.
--
-- /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
  { _sFrequency   :: !(Maybe Frequency)
  , _sStartTime   :: !(Maybe Text)
  , _sQuietTime   :: !(Maybe QuietTime)
  , _sIsLocalTime :: !(Maybe Bool)
  , _sEndTime     :: !(Maybe Text)
  , _sTimezone    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFrequency' - How often the campaign delivers messages. Valid values: ONCE, HOURLY, DAILY, WEEKLY, MONTHLY
--
-- * 'sStartTime' - The scheduled time that the campaign begins in ISO 8601 format.
--
-- * 'sQuietTime' - The time during which the campaign sends no messages.
--
-- * 'sIsLocalTime' - Indicates whether the campaign schedule takes effect according to each user's local time.
--
-- * 'sEndTime' - The scheduled time that the campaign ends in ISO 8601 format.
--
-- * 'sTimezone' - The starting UTC offset for the schedule if the value for isLocalTime is true Valid values:  UTC UTC+01 UTC+02 UTC+03 UTC+03:30 UTC+04 UTC+04:30 UTC+05 UTC+05:30 UTC+05:45 UTC+06 UTC+06:30 UTC+07 UTC+08 UTC+09 UTC+09:30 UTC+10 UTC+10:30 UTC+11 UTC+12 UTC+13 UTC-02 UTC-03 UTC-04 UTC-05 UTC-06 UTC-07 UTC-08 UTC-09 UTC-10 UTC-11
schedule
    :: Schedule
schedule =
  Schedule'
    { _sFrequency = Nothing
    , _sStartTime = Nothing
    , _sQuietTime = Nothing
    , _sIsLocalTime = Nothing
    , _sEndTime = Nothing
    , _sTimezone = Nothing
    }


-- | How often the campaign delivers messages. Valid values: ONCE, HOURLY, DAILY, WEEKLY, MONTHLY
sFrequency :: Lens' Schedule (Maybe Frequency)
sFrequency = lens _sFrequency (\ s a -> s{_sFrequency = a})

-- | The scheduled time that the campaign begins in ISO 8601 format.
sStartTime :: Lens' Schedule (Maybe Text)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a})

-- | The time during which the campaign sends no messages.
sQuietTime :: Lens' Schedule (Maybe QuietTime)
sQuietTime = lens _sQuietTime (\ s a -> s{_sQuietTime = a})

-- | Indicates whether the campaign schedule takes effect according to each user's local time.
sIsLocalTime :: Lens' Schedule (Maybe Bool)
sIsLocalTime = lens _sIsLocalTime (\ s a -> s{_sIsLocalTime = a})

-- | The scheduled time that the campaign ends in ISO 8601 format.
sEndTime :: Lens' Schedule (Maybe Text)
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a})

-- | The starting UTC offset for the schedule if the value for isLocalTime is true Valid values:  UTC UTC+01 UTC+02 UTC+03 UTC+03:30 UTC+04 UTC+04:30 UTC+05 UTC+05:30 UTC+05:45 UTC+06 UTC+06:30 UTC+07 UTC+08 UTC+09 UTC+09:30 UTC+10 UTC+10:30 UTC+11 UTC+12 UTC+13 UTC-02 UTC-03 UTC-04 UTC-05 UTC-06 UTC-07 UTC-08 UTC-09 UTC-10 UTC-11
sTimezone :: Lens' Schedule (Maybe Text)
sTimezone = lens _sTimezone (\ s a -> s{_sTimezone = a})

instance FromJSON Schedule where
        parseJSON
          = withObject "Schedule"
              (\ x ->
                 Schedule' <$>
                   (x .:? "Frequency") <*> (x .:? "StartTime") <*>
                     (x .:? "QuietTime")
                     <*> (x .:? "IsLocalTime")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "Timezone"))

instance Hashable Schedule where

instance NFData Schedule where

instance ToJSON Schedule where
        toJSON Schedule'{..}
          = object
              (catMaybes
                 [("Frequency" .=) <$> _sFrequency,
                  ("StartTime" .=) <$> _sStartTime,
                  ("QuietTime" .=) <$> _sQuietTime,
                  ("IsLocalTime" .=) <$> _sIsLocalTime,
                  ("EndTime" .=) <$> _sEndTime,
                  ("Timezone" .=) <$> _sTimezone])

-- | Segment behavior dimensions
--
-- /See:/ 'segmentBehaviors' smart constructor.
newtype SegmentBehaviors = SegmentBehaviors'
  { _sbRecency :: Maybe RecencyDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbRecency' - The recency of use.
segmentBehaviors
    :: SegmentBehaviors
segmentBehaviors = SegmentBehaviors' {_sbRecency = Nothing}


-- | The recency of use.
sbRecency :: Lens' SegmentBehaviors (Maybe RecencyDimension)
sbRecency = lens _sbRecency (\ s a -> s{_sbRecency = a})

instance FromJSON SegmentBehaviors where
        parseJSON
          = withObject "SegmentBehaviors"
              (\ x -> SegmentBehaviors' <$> (x .:? "Recency"))

instance Hashable SegmentBehaviors where

instance NFData SegmentBehaviors where

instance ToJSON SegmentBehaviors where
        toJSON SegmentBehaviors'{..}
          = object (catMaybes [("Recency" .=) <$> _sbRecency])

-- | Segment demographic dimensions
--
-- /See:/ 'segmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
  { _sdPlatform   :: !(Maybe SetDimension)
  , _sdAppVersion :: !(Maybe SetDimension)
  , _sdChannel    :: !(Maybe SetDimension)
  , _sdModel      :: !(Maybe SetDimension)
  , _sdMake       :: !(Maybe SetDimension)
  , _sdDeviceType :: !(Maybe SetDimension)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentDemographics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdPlatform' - The device platform criteria for the segment.
--
-- * 'sdAppVersion' - The app version criteria for the segment.
--
-- * 'sdChannel' - The channel criteria for the segment.
--
-- * 'sdModel' - The device model criteria for the segment.
--
-- * 'sdMake' - The device make criteria for the segment.
--
-- * 'sdDeviceType' - The device type criteria for the segment.
segmentDemographics
    :: SegmentDemographics
segmentDemographics =
  SegmentDemographics'
    { _sdPlatform = Nothing
    , _sdAppVersion = Nothing
    , _sdChannel = Nothing
    , _sdModel = Nothing
    , _sdMake = Nothing
    , _sdDeviceType = Nothing
    }


-- | The device platform criteria for the segment.
sdPlatform :: Lens' SegmentDemographics (Maybe SetDimension)
sdPlatform = lens _sdPlatform (\ s a -> s{_sdPlatform = a})

-- | The app version criteria for the segment.
sdAppVersion :: Lens' SegmentDemographics (Maybe SetDimension)
sdAppVersion = lens _sdAppVersion (\ s a -> s{_sdAppVersion = a})

-- | The channel criteria for the segment.
sdChannel :: Lens' SegmentDemographics (Maybe SetDimension)
sdChannel = lens _sdChannel (\ s a -> s{_sdChannel = a})

-- | The device model criteria for the segment.
sdModel :: Lens' SegmentDemographics (Maybe SetDimension)
sdModel = lens _sdModel (\ s a -> s{_sdModel = a})

-- | The device make criteria for the segment.
sdMake :: Lens' SegmentDemographics (Maybe SetDimension)
sdMake = lens _sdMake (\ s a -> s{_sdMake = a})

-- | The device type criteria for the segment.
sdDeviceType :: Lens' SegmentDemographics (Maybe SetDimension)
sdDeviceType = lens _sdDeviceType (\ s a -> s{_sdDeviceType = a})

instance FromJSON SegmentDemographics where
        parseJSON
          = withObject "SegmentDemographics"
              (\ x ->
                 SegmentDemographics' <$>
                   (x .:? "Platform") <*> (x .:? "AppVersion") <*>
                     (x .:? "Channel")
                     <*> (x .:? "Model")
                     <*> (x .:? "Make")
                     <*> (x .:? "DeviceType"))

instance Hashable SegmentDemographics where

instance NFData SegmentDemographics where

instance ToJSON SegmentDemographics where
        toJSON SegmentDemographics'{..}
          = object
              (catMaybes
                 [("Platform" .=) <$> _sdPlatform,
                  ("AppVersion" .=) <$> _sdAppVersion,
                  ("Channel" .=) <$> _sdChannel,
                  ("Model" .=) <$> _sdModel, ("Make" .=) <$> _sdMake,
                  ("DeviceType" .=) <$> _sdDeviceType])

-- | Segment dimensions
--
-- /See:/ 'segmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { _sdLocation       :: !(Maybe SegmentLocation)
  , _sdDemographic    :: !(Maybe SegmentDemographics)
  , _sdUserAttributes :: !(Maybe (Map Text AttributeDimension))
  , _sdBehavior       :: !(Maybe SegmentBehaviors)
  , _sdAttributes     :: !(Maybe (Map Text AttributeDimension))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentDimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLocation' - The segment location attributes.
--
-- * 'sdDemographic' - The segment demographics attributes.
--
-- * 'sdUserAttributes' - Custom segment user attributes.
--
-- * 'sdBehavior' - The segment behaviors attributes.
--
-- * 'sdAttributes' - Custom segment attributes.
segmentDimensions
    :: SegmentDimensions
segmentDimensions =
  SegmentDimensions'
    { _sdLocation = Nothing
    , _sdDemographic = Nothing
    , _sdUserAttributes = Nothing
    , _sdBehavior = Nothing
    , _sdAttributes = Nothing
    }


-- | The segment location attributes.
sdLocation :: Lens' SegmentDimensions (Maybe SegmentLocation)
sdLocation = lens _sdLocation (\ s a -> s{_sdLocation = a})

-- | The segment demographics attributes.
sdDemographic :: Lens' SegmentDimensions (Maybe SegmentDemographics)
sdDemographic = lens _sdDemographic (\ s a -> s{_sdDemographic = a})

-- | Custom segment user attributes.
sdUserAttributes :: Lens' SegmentDimensions (HashMap Text AttributeDimension)
sdUserAttributes = lens _sdUserAttributes (\ s a -> s{_sdUserAttributes = a}) . _Default . _Map

-- | The segment behaviors attributes.
sdBehavior :: Lens' SegmentDimensions (Maybe SegmentBehaviors)
sdBehavior = lens _sdBehavior (\ s a -> s{_sdBehavior = a})

-- | Custom segment attributes.
sdAttributes :: Lens' SegmentDimensions (HashMap Text AttributeDimension)
sdAttributes = lens _sdAttributes (\ s a -> s{_sdAttributes = a}) . _Default . _Map

instance FromJSON SegmentDimensions where
        parseJSON
          = withObject "SegmentDimensions"
              (\ x ->
                 SegmentDimensions' <$>
                   (x .:? "Location") <*> (x .:? "Demographic") <*>
                     (x .:? "UserAttributes" .!= mempty)
                     <*> (x .:? "Behavior")
                     <*> (x .:? "Attributes" .!= mempty))

instance Hashable SegmentDimensions where

instance NFData SegmentDimensions where

instance ToJSON SegmentDimensions where
        toJSON SegmentDimensions'{..}
          = object
              (catMaybes
                 [("Location" .=) <$> _sdLocation,
                  ("Demographic" .=) <$> _sdDemographic,
                  ("UserAttributes" .=) <$> _sdUserAttributes,
                  ("Behavior" .=) <$> _sdBehavior,
                  ("Attributes" .=) <$> _sdAttributes])

-- | Segment import definition.
--
-- /See:/ 'segmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
  { _sirSize          :: !(Maybe Int)
  , _sirFormat        :: !(Maybe DefinitionFormat)
  , _sirChannelCounts :: !(Maybe (Map Text Int))
  , _sirExternalId    :: !(Maybe Text)
  , _sirS3URL         :: !(Maybe Text)
  , _sirRoleARN       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentImportResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirSize' - The number of endpoints that were successfully imported to create this segment.
--
-- * 'sirFormat' - The format of the endpoint files that were imported to create this segment. Valid values: CSV, JSON
--
-- * 'sirChannelCounts' - Channel type counts
--
-- * 'sirExternalId' - DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
--
-- * 'sirS3URL' - A URL that points to the Amazon S3 location from which the endpoints for this segment were imported.
--
-- * 'sirRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the endpoints in Amazon S3.
segmentImportResource
    :: SegmentImportResource
segmentImportResource =
  SegmentImportResource'
    { _sirSize = Nothing
    , _sirFormat = Nothing
    , _sirChannelCounts = Nothing
    , _sirExternalId = Nothing
    , _sirS3URL = Nothing
    , _sirRoleARN = Nothing
    }


-- | The number of endpoints that were successfully imported to create this segment.
sirSize :: Lens' SegmentImportResource (Maybe Int)
sirSize = lens _sirSize (\ s a -> s{_sirSize = a})

-- | The format of the endpoint files that were imported to create this segment. Valid values: CSV, JSON
sirFormat :: Lens' SegmentImportResource (Maybe DefinitionFormat)
sirFormat = lens _sirFormat (\ s a -> s{_sirFormat = a})

-- | Channel type counts
sirChannelCounts :: Lens' SegmentImportResource (HashMap Text Int)
sirChannelCounts = lens _sirChannelCounts (\ s a -> s{_sirChannelCounts = a}) . _Default . _Map

-- | DEPRECATED. Your AWS account ID, which you assigned to the ExternalID key in an IAM trust policy. Used by Amazon Pinpoint to assume an IAM role. This requirement is removed, and external IDs are not recommended for IAM roles assumed by Amazon Pinpoint.
sirExternalId :: Lens' SegmentImportResource (Maybe Text)
sirExternalId = lens _sirExternalId (\ s a -> s{_sirExternalId = a})

-- | A URL that points to the Amazon S3 location from which the endpoints for this segment were imported.
sirS3URL :: Lens' SegmentImportResource (Maybe Text)
sirS3URL = lens _sirS3URL (\ s a -> s{_sirS3URL = a})

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the endpoints in Amazon S3.
sirRoleARN :: Lens' SegmentImportResource (Maybe Text)
sirRoleARN = lens _sirRoleARN (\ s a -> s{_sirRoleARN = a})

instance FromJSON SegmentImportResource where
        parseJSON
          = withObject "SegmentImportResource"
              (\ x ->
                 SegmentImportResource' <$>
                   (x .:? "Size") <*> (x .:? "Format") <*>
                     (x .:? "ChannelCounts" .!= mempty)
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "S3Url")
                     <*> (x .:? "RoleArn"))

instance Hashable SegmentImportResource where

instance NFData SegmentImportResource where

-- | Segment location dimensions
--
-- /See:/ 'segmentLocation' smart constructor.
newtype SegmentLocation = SegmentLocation'
  { _slCountry :: Maybe SetDimension
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCountry' - The country filter according to ISO 3166-1 Alpha-2 codes.
segmentLocation
    :: SegmentLocation
segmentLocation = SegmentLocation' {_slCountry = Nothing}


-- | The country filter according to ISO 3166-1 Alpha-2 codes.
slCountry :: Lens' SegmentLocation (Maybe SetDimension)
slCountry = lens _slCountry (\ s a -> s{_slCountry = a})

instance FromJSON SegmentLocation where
        parseJSON
          = withObject "SegmentLocation"
              (\ x -> SegmentLocation' <$> (x .:? "Country"))

instance Hashable SegmentLocation where

instance NFData SegmentLocation where

instance ToJSON SegmentLocation where
        toJSON SegmentLocation'{..}
          = object (catMaybes [("Country" .=) <$> _slCountry])

-- | Segment definition.
--
-- /See:/ 'segmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
  { _sLastModifiedDate :: !(Maybe Text)
  , _sSegmentType      :: !(Maybe SegmentType)
  , _sApplicationId    :: !(Maybe Text)
  , _sName             :: !(Maybe Text)
  , _sVersion          :: !(Maybe Int)
  , _sId               :: !(Maybe Text)
  , _sCreationDate     :: !(Maybe Text)
  , _sImportDefinition :: !(Maybe SegmentImportResource)
  , _sDimensions       :: !(Maybe SegmentDimensions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sLastModifiedDate' - The date the segment was last updated in ISO 8601 format.
--
-- * 'sSegmentType' - The segment type: DIMENSIONAL - A dynamic segment built from selection criteria based on endpoint data reported by your app. You create this type of segment by using the segment builder in the Amazon Pinpoint console or by making a POST request to the segments resource. IMPORT - A static segment built from an imported set of endpoint definitions. You create this type of segment by importing a segment in the Amazon Pinpoint console or by making a POST request to the jobs/import resource.
--
-- * 'sApplicationId' - The ID of the application to which the segment applies.
--
-- * 'sName' - The name of segment
--
-- * 'sVersion' - The segment version number.
--
-- * 'sId' - The unique segment ID.
--
-- * 'sCreationDate' - The date the segment was created in ISO 8601 format.
--
-- * 'sImportDefinition' - The import job settings.
--
-- * 'sDimensions' - The segment dimensions attributes.
segmentResponse
    :: SegmentResponse
segmentResponse =
  SegmentResponse'
    { _sLastModifiedDate = Nothing
    , _sSegmentType = Nothing
    , _sApplicationId = Nothing
    , _sName = Nothing
    , _sVersion = Nothing
    , _sId = Nothing
    , _sCreationDate = Nothing
    , _sImportDefinition = Nothing
    , _sDimensions = Nothing
    }


-- | The date the segment was last updated in ISO 8601 format.
sLastModifiedDate :: Lens' SegmentResponse (Maybe Text)
sLastModifiedDate = lens _sLastModifiedDate (\ s a -> s{_sLastModifiedDate = a})

-- | The segment type: DIMENSIONAL - A dynamic segment built from selection criteria based on endpoint data reported by your app. You create this type of segment by using the segment builder in the Amazon Pinpoint console or by making a POST request to the segments resource. IMPORT - A static segment built from an imported set of endpoint definitions. You create this type of segment by importing a segment in the Amazon Pinpoint console or by making a POST request to the jobs/import resource.
sSegmentType :: Lens' SegmentResponse (Maybe SegmentType)
sSegmentType = lens _sSegmentType (\ s a -> s{_sSegmentType = a})

-- | The ID of the application to which the segment applies.
sApplicationId :: Lens' SegmentResponse (Maybe Text)
sApplicationId = lens _sApplicationId (\ s a -> s{_sApplicationId = a})

-- | The name of segment
sName :: Lens' SegmentResponse (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The segment version number.
sVersion :: Lens' SegmentResponse (Maybe Int)
sVersion = lens _sVersion (\ s a -> s{_sVersion = a})

-- | The unique segment ID.
sId :: Lens' SegmentResponse (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a})

-- | The date the segment was created in ISO 8601 format.
sCreationDate :: Lens' SegmentResponse (Maybe Text)
sCreationDate = lens _sCreationDate (\ s a -> s{_sCreationDate = a})

-- | The import job settings.
sImportDefinition :: Lens' SegmentResponse (Maybe SegmentImportResource)
sImportDefinition = lens _sImportDefinition (\ s a -> s{_sImportDefinition = a})

-- | The segment dimensions attributes.
sDimensions :: Lens' SegmentResponse (Maybe SegmentDimensions)
sDimensions = lens _sDimensions (\ s a -> s{_sDimensions = a})

instance FromJSON SegmentResponse where
        parseJSON
          = withObject "SegmentResponse"
              (\ x ->
                 SegmentResponse' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "SegmentType")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ImportDefinition")
                     <*> (x .:? "Dimensions"))

instance Hashable SegmentResponse where

instance NFData SegmentResponse where

-- | Segments in your account.
--
-- /See:/ 'segmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
  { _sNextToken :: !(Maybe Text)
  , _sItem      :: !(Maybe [SegmentResponse])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNextToken' - An identifier used to retrieve the next page of results. The token is null if no additional pages exist.
--
-- * 'sItem' - The list of segments.
segmentsResponse
    :: SegmentsResponse
segmentsResponse = SegmentsResponse' {_sNextToken = Nothing, _sItem = Nothing}


-- | An identifier used to retrieve the next page of results. The token is null if no additional pages exist.
sNextToken :: Lens' SegmentsResponse (Maybe Text)
sNextToken = lens _sNextToken (\ s a -> s{_sNextToken = a})

-- | The list of segments.
sItem :: Lens' SegmentsResponse [SegmentResponse]
sItem = lens _sItem (\ s a -> s{_sItem = a}) . _Default . _Coerce

instance FromJSON SegmentsResponse where
        parseJSON
          = withObject "SegmentsResponse"
              (\ x ->
                 SegmentsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable SegmentsResponse where

instance NFData SegmentsResponse where

-- | Send message request.
--
-- /See:/ 'sendUsersMessageRequest' smart constructor.
data SendUsersMessageRequest = SendUsersMessageRequest'
  { _sumrContext              :: !(Maybe (Map Text Text))
  , _sumrUsers                :: !(Maybe (Map Text EndpointSendConfiguration))
  , _sumrMessageConfiguration :: !(Maybe DirectMessageConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendUsersMessageRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumrContext' - A map of custom attributes to attributes to be attached to the message. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
--
-- * 'sumrUsers' - A map of destination endpoints, with the EndpointId as the key Endpoint Message Configuration as the value.
--
-- * 'sumrMessageConfiguration' - Message configuration.
sendUsersMessageRequest
    :: SendUsersMessageRequest
sendUsersMessageRequest =
  SendUsersMessageRequest'
    { _sumrContext = Nothing
    , _sumrUsers = Nothing
    , _sumrMessageConfiguration = Nothing
    }


-- | A map of custom attributes to attributes to be attached to the message. This payload is added to the push notification's 'data.pinpoint' object or added to the email/sms delivery receipt event attributes.
sumrContext :: Lens' SendUsersMessageRequest (HashMap Text Text)
sumrContext = lens _sumrContext (\ s a -> s{_sumrContext = a}) . _Default . _Map

-- | A map of destination endpoints, with the EndpointId as the key Endpoint Message Configuration as the value.
sumrUsers :: Lens' SendUsersMessageRequest (HashMap Text EndpointSendConfiguration)
sumrUsers = lens _sumrUsers (\ s a -> s{_sumrUsers = a}) . _Default . _Map

-- | Message configuration.
sumrMessageConfiguration :: Lens' SendUsersMessageRequest (Maybe DirectMessageConfiguration)
sumrMessageConfiguration = lens _sumrMessageConfiguration (\ s a -> s{_sumrMessageConfiguration = a})

instance Hashable SendUsersMessageRequest where

instance NFData SendUsersMessageRequest where

instance ToJSON SendUsersMessageRequest where
        toJSON SendUsersMessageRequest'{..}
          = object
              (catMaybes
                 [("Context" .=) <$> _sumrContext,
                  ("Users" .=) <$> _sumrUsers,
                  ("MessageConfiguration" .=) <$>
                    _sumrMessageConfiguration])

-- | User send message response.
--
-- /See:/ 'sendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { _sumRequestId     :: !(Maybe Text)
  , _sumResult        :: !(Maybe (Map Text (Map Text EndpointMessageResult)))
  , _sumApplicationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendUsersMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumRequestId' - Original request Id for which this message was delivered.
--
-- * 'sumResult' - A map containing of UserId to Map of EndpointId to Endpoint Message Result.
--
-- * 'sumApplicationId' - Application id of the message.
sendUsersMessageResponse
    :: SendUsersMessageResponse
sendUsersMessageResponse =
  SendUsersMessageResponse'
    {_sumRequestId = Nothing, _sumResult = Nothing, _sumApplicationId = Nothing}


-- | Original request Id for which this message was delivered.
sumRequestId :: Lens' SendUsersMessageResponse (Maybe Text)
sumRequestId = lens _sumRequestId (\ s a -> s{_sumRequestId = a})

-- | A map containing of UserId to Map of EndpointId to Endpoint Message Result.
sumResult :: Lens' SendUsersMessageResponse (HashMap Text (HashMap Text EndpointMessageResult))
sumResult = lens _sumResult (\ s a -> s{_sumResult = a}) . _Default . _Map

-- | Application id of the message.
sumApplicationId :: Lens' SendUsersMessageResponse (Maybe Text)
sumApplicationId = lens _sumApplicationId (\ s a -> s{_sumApplicationId = a})

instance FromJSON SendUsersMessageResponse where
        parseJSON
          = withObject "SendUsersMessageResponse"
              (\ x ->
                 SendUsersMessageResponse' <$>
                   (x .:? "RequestId") <*> (x .:? "Result" .!= mempty)
                     <*> (x .:? "ApplicationId"))

instance Hashable SendUsersMessageResponse where

instance NFData SendUsersMessageResponse where

-- | Dimension specification of a segment.
--
-- /See:/ 'setDimension' smart constructor.
data SetDimension = SetDimension'
  { _sdValues        :: !(Maybe [Text])
  , _sdDimensionType :: !(Maybe DimensionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdValues' - The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
--
-- * 'sdDimensionType' - The type of dimension: INCLUSIVE - Endpoints that match the criteria are included in the segment. EXCLUSIVE - Endpoints that match the criteria are excluded from the segment.
setDimension
    :: SetDimension
setDimension = SetDimension' {_sdValues = Nothing, _sdDimensionType = Nothing}


-- | The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
sdValues :: Lens' SetDimension [Text]
sdValues = lens _sdValues (\ s a -> s{_sdValues = a}) . _Default . _Coerce

-- | The type of dimension: INCLUSIVE - Endpoints that match the criteria are included in the segment. EXCLUSIVE - Endpoints that match the criteria are excluded from the segment.
sdDimensionType :: Lens' SetDimension (Maybe DimensionType)
sdDimensionType = lens _sdDimensionType (\ s a -> s{_sdDimensionType = a})

instance FromJSON SetDimension where
        parseJSON
          = withObject "SetDimension"
              (\ x ->
                 SetDimension' <$>
                   (x .:? "Values" .!= mempty) <*>
                     (x .:? "DimensionType"))

instance Hashable SetDimension where

instance NFData SetDimension where

instance ToJSON SetDimension where
        toJSON SetDimension'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _sdValues,
                  ("DimensionType" .=) <$> _sdDimensionType])

-- | Treatment resource
--
-- /See:/ 'treatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
  { _trState                :: !(Maybe CampaignState)
  , _trSchedule             :: !(Maybe Schedule)
  , _trTreatmentName        :: !(Maybe Text)
  , _trSizePercent          :: !(Maybe Int)
  , _trTreatmentDescription :: !(Maybe Text)
  , _trId                   :: !(Maybe Text)
  , _trMessageConfiguration :: !(Maybe MessageConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TreatmentResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trState' - The treatment status.
--
-- * 'trSchedule' - The campaign schedule.
--
-- * 'trTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'trSizePercent' - The allocated percentage of users for this treatment.
--
-- * 'trTreatmentDescription' - A custom description for the treatment.
--
-- * 'trId' - The unique treatment ID.
--
-- * 'trMessageConfiguration' - The message configuration settings.
treatmentResource
    :: TreatmentResource
treatmentResource =
  TreatmentResource'
    { _trState = Nothing
    , _trSchedule = Nothing
    , _trTreatmentName = Nothing
    , _trSizePercent = Nothing
    , _trTreatmentDescription = Nothing
    , _trId = Nothing
    , _trMessageConfiguration = Nothing
    }


-- | The treatment status.
trState :: Lens' TreatmentResource (Maybe CampaignState)
trState = lens _trState (\ s a -> s{_trState = a})

-- | The campaign schedule.
trSchedule :: Lens' TreatmentResource (Maybe Schedule)
trSchedule = lens _trSchedule (\ s a -> s{_trSchedule = a})

-- | The custom name of a variation of the campaign used for A/B testing.
trTreatmentName :: Lens' TreatmentResource (Maybe Text)
trTreatmentName = lens _trTreatmentName (\ s a -> s{_trTreatmentName = a})

-- | The allocated percentage of users for this treatment.
trSizePercent :: Lens' TreatmentResource (Maybe Int)
trSizePercent = lens _trSizePercent (\ s a -> s{_trSizePercent = a})

-- | A custom description for the treatment.
trTreatmentDescription :: Lens' TreatmentResource (Maybe Text)
trTreatmentDescription = lens _trTreatmentDescription (\ s a -> s{_trTreatmentDescription = a})

-- | The unique treatment ID.
trId :: Lens' TreatmentResource (Maybe Text)
trId = lens _trId (\ s a -> s{_trId = a})

-- | The message configuration settings.
trMessageConfiguration :: Lens' TreatmentResource (Maybe MessageConfiguration)
trMessageConfiguration = lens _trMessageConfiguration (\ s a -> s{_trMessageConfiguration = a})

instance FromJSON TreatmentResource where
        parseJSON
          = withObject "TreatmentResource"
              (\ x ->
                 TreatmentResource' <$>
                   (x .:? "State") <*> (x .:? "Schedule") <*>
                     (x .:? "TreatmentName")
                     <*> (x .:? "SizePercent")
                     <*> (x .:? "TreatmentDescription")
                     <*> (x .:? "Id")
                     <*> (x .:? "MessageConfiguration"))

instance Hashable TreatmentResource where

instance NFData TreatmentResource where

-- | Creating application setting request
--
-- /See:/ 'writeApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
  { _wasrLimits       :: !(Maybe CampaignLimits)
  , _wasrQuietTime    :: !(Maybe QuietTime)
  , _wasrCampaignHook :: !(Maybe CampaignHook)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteApplicationSettingsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wasrLimits' - The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
--
-- * 'wasrQuietTime' - The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
--
-- * 'wasrCampaignHook' - Default campaign hook information.
writeApplicationSettingsRequest
    :: WriteApplicationSettingsRequest
writeApplicationSettingsRequest =
  WriteApplicationSettingsRequest'
    { _wasrLimits = Nothing
    , _wasrQuietTime = Nothing
    , _wasrCampaignHook = Nothing
    }


-- | The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
wasrLimits :: Lens' WriteApplicationSettingsRequest (Maybe CampaignLimits)
wasrLimits = lens _wasrLimits (\ s a -> s{_wasrLimits = a})

-- | The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
wasrQuietTime :: Lens' WriteApplicationSettingsRequest (Maybe QuietTime)
wasrQuietTime = lens _wasrQuietTime (\ s a -> s{_wasrQuietTime = a})

-- | Default campaign hook information.
wasrCampaignHook :: Lens' WriteApplicationSettingsRequest (Maybe CampaignHook)
wasrCampaignHook = lens _wasrCampaignHook (\ s a -> s{_wasrCampaignHook = a})

instance Hashable WriteApplicationSettingsRequest
         where

instance NFData WriteApplicationSettingsRequest where

instance ToJSON WriteApplicationSettingsRequest where
        toJSON WriteApplicationSettingsRequest'{..}
          = object
              (catMaybes
                 [("Limits" .=) <$> _wasrLimits,
                  ("QuietTime" .=) <$> _wasrQuietTime,
                  ("CampaignHook" .=) <$> _wasrCampaignHook])

-- | Used to create a campaign.
--
-- /See:/ 'writeCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
  { _wcrSchedule             :: !(Maybe Schedule)
  , _wcrHook                 :: !(Maybe CampaignHook)
  , _wcrTreatmentName        :: !(Maybe Text)
  , _wcrLimits               :: !(Maybe CampaignLimits)
  , _wcrIsPaused             :: !(Maybe Bool)
  , _wcrName                 :: !(Maybe Text)
  , _wcrHoldoutPercent       :: !(Maybe Int)
  , _wcrTreatmentDescription :: !(Maybe Text)
  , _wcrMessageConfiguration :: !(Maybe MessageConfiguration)
  , _wcrDescription          :: !(Maybe Text)
  , _wcrSegmentId            :: !(Maybe Text)
  , _wcrAdditionalTreatments :: !(Maybe [WriteTreatmentResource])
  , _wcrSegmentVersion       :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteCampaignRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcrSchedule' - The campaign schedule.
--
-- * 'wcrHook' - Campaign hook information.
--
-- * 'wcrTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'wcrLimits' - The campaign limits settings.
--
-- * 'wcrIsPaused' - Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
--
-- * 'wcrName' - The custom name of the campaign.
--
-- * 'wcrHoldoutPercent' - The allocated percentage of end users who will not receive messages from this campaign.
--
-- * 'wcrTreatmentDescription' - A custom description for the treatment.
--
-- * 'wcrMessageConfiguration' - The message configuration settings.
--
-- * 'wcrDescription' - A description of the campaign.
--
-- * 'wcrSegmentId' - The ID of the segment to which the campaign sends messages.
--
-- * 'wcrAdditionalTreatments' - Treatments that are defined in addition to the default treatment.
--
-- * 'wcrSegmentVersion' - The version of the segment to which the campaign sends messages.
writeCampaignRequest
    :: WriteCampaignRequest
writeCampaignRequest =
  WriteCampaignRequest'
    { _wcrSchedule = Nothing
    , _wcrHook = Nothing
    , _wcrTreatmentName = Nothing
    , _wcrLimits = Nothing
    , _wcrIsPaused = Nothing
    , _wcrName = Nothing
    , _wcrHoldoutPercent = Nothing
    , _wcrTreatmentDescription = Nothing
    , _wcrMessageConfiguration = Nothing
    , _wcrDescription = Nothing
    , _wcrSegmentId = Nothing
    , _wcrAdditionalTreatments = Nothing
    , _wcrSegmentVersion = Nothing
    }


-- | The campaign schedule.
wcrSchedule :: Lens' WriteCampaignRequest (Maybe Schedule)
wcrSchedule = lens _wcrSchedule (\ s a -> s{_wcrSchedule = a})

-- | Campaign hook information.
wcrHook :: Lens' WriteCampaignRequest (Maybe CampaignHook)
wcrHook = lens _wcrHook (\ s a -> s{_wcrHook = a})

-- | The custom name of a variation of the campaign used for A/B testing.
wcrTreatmentName :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentName = lens _wcrTreatmentName (\ s a -> s{_wcrTreatmentName = a})

-- | The campaign limits settings.
wcrLimits :: Lens' WriteCampaignRequest (Maybe CampaignLimits)
wcrLimits = lens _wcrLimits (\ s a -> s{_wcrLimits = a})

-- | Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
wcrIsPaused :: Lens' WriteCampaignRequest (Maybe Bool)
wcrIsPaused = lens _wcrIsPaused (\ s a -> s{_wcrIsPaused = a})

-- | The custom name of the campaign.
wcrName :: Lens' WriteCampaignRequest (Maybe Text)
wcrName = lens _wcrName (\ s a -> s{_wcrName = a})

-- | The allocated percentage of end users who will not receive messages from this campaign.
wcrHoldoutPercent :: Lens' WriteCampaignRequest (Maybe Int)
wcrHoldoutPercent = lens _wcrHoldoutPercent (\ s a -> s{_wcrHoldoutPercent = a})

-- | A custom description for the treatment.
wcrTreatmentDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentDescription = lens _wcrTreatmentDescription (\ s a -> s{_wcrTreatmentDescription = a})

-- | The message configuration settings.
wcrMessageConfiguration :: Lens' WriteCampaignRequest (Maybe MessageConfiguration)
wcrMessageConfiguration = lens _wcrMessageConfiguration (\ s a -> s{_wcrMessageConfiguration = a})

-- | A description of the campaign.
wcrDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrDescription = lens _wcrDescription (\ s a -> s{_wcrDescription = a})

-- | The ID of the segment to which the campaign sends messages.
wcrSegmentId :: Lens' WriteCampaignRequest (Maybe Text)
wcrSegmentId = lens _wcrSegmentId (\ s a -> s{_wcrSegmentId = a})

-- | Treatments that are defined in addition to the default treatment.
wcrAdditionalTreatments :: Lens' WriteCampaignRequest [WriteTreatmentResource]
wcrAdditionalTreatments = lens _wcrAdditionalTreatments (\ s a -> s{_wcrAdditionalTreatments = a}) . _Default . _Coerce

-- | The version of the segment to which the campaign sends messages.
wcrSegmentVersion :: Lens' WriteCampaignRequest (Maybe Int)
wcrSegmentVersion = lens _wcrSegmentVersion (\ s a -> s{_wcrSegmentVersion = a})

instance Hashable WriteCampaignRequest where

instance NFData WriteCampaignRequest where

instance ToJSON WriteCampaignRequest where
        toJSON WriteCampaignRequest'{..}
          = object
              (catMaybes
                 [("Schedule" .=) <$> _wcrSchedule,
                  ("Hook" .=) <$> _wcrHook,
                  ("TreatmentName" .=) <$> _wcrTreatmentName,
                  ("Limits" .=) <$> _wcrLimits,
                  ("IsPaused" .=) <$> _wcrIsPaused,
                  ("Name" .=) <$> _wcrName,
                  ("HoldoutPercent" .=) <$> _wcrHoldoutPercent,
                  ("TreatmentDescription" .=) <$>
                    _wcrTreatmentDescription,
                  ("MessageConfiguration" .=) <$>
                    _wcrMessageConfiguration,
                  ("Description" .=) <$> _wcrDescription,
                  ("SegmentId" .=) <$> _wcrSegmentId,
                  ("AdditionalTreatments" .=) <$>
                    _wcrAdditionalTreatments,
                  ("SegmentVersion" .=) <$> _wcrSegmentVersion])

-- | Request to save an EventStream.
--
-- /See:/ 'writeEventStream' smart constructor.
data WriteEventStream = WriteEventStream'
  { _wesDestinationStreamARN :: !(Maybe Text)
  , _wesRoleARN              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wesDestinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis stream or Firehose delivery stream to which you want to publish events.  Firehose ARN: arn:aws:firehose:REGION:ACCOUNT_ID:deliverystream/STREAM_NAME  Kinesis ARN: arn:aws:kinesis:REGION:ACCOUNT_ID:stream/STREAM_NAME
--
-- * 'wesRoleARN' - The IAM role that authorizes Amazon Pinpoint to publish events to the stream in your account.
writeEventStream
    :: WriteEventStream
writeEventStream =
  WriteEventStream' {_wesDestinationStreamARN = Nothing, _wesRoleARN = Nothing}


-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream or Firehose delivery stream to which you want to publish events.  Firehose ARN: arn:aws:firehose:REGION:ACCOUNT_ID:deliverystream/STREAM_NAME  Kinesis ARN: arn:aws:kinesis:REGION:ACCOUNT_ID:stream/STREAM_NAME
wesDestinationStreamARN :: Lens' WriteEventStream (Maybe Text)
wesDestinationStreamARN = lens _wesDestinationStreamARN (\ s a -> s{_wesDestinationStreamARN = a})

-- | The IAM role that authorizes Amazon Pinpoint to publish events to the stream in your account.
wesRoleARN :: Lens' WriteEventStream (Maybe Text)
wesRoleARN = lens _wesRoleARN (\ s a -> s{_wesRoleARN = a})

instance Hashable WriteEventStream where

instance NFData WriteEventStream where

instance ToJSON WriteEventStream where
        toJSON WriteEventStream'{..}
          = object
              (catMaybes
                 [("DestinationStreamArn" .=) <$>
                    _wesDestinationStreamARN,
                  ("RoleArn" .=) <$> _wesRoleARN])

-- | Segment definition.
--
-- /See:/ 'writeSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
  { _wsrName       :: !(Maybe Text)
  , _wsrDimensions :: !(Maybe SegmentDimensions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteSegmentRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsrName' - The name of segment
--
-- * 'wsrDimensions' - The segment dimensions attributes.
writeSegmentRequest
    :: WriteSegmentRequest
writeSegmentRequest =
  WriteSegmentRequest' {_wsrName = Nothing, _wsrDimensions = Nothing}


-- | The name of segment
wsrName :: Lens' WriteSegmentRequest (Maybe Text)
wsrName = lens _wsrName (\ s a -> s{_wsrName = a})

-- | The segment dimensions attributes.
wsrDimensions :: Lens' WriteSegmentRequest (Maybe SegmentDimensions)
wsrDimensions = lens _wsrDimensions (\ s a -> s{_wsrDimensions = a})

instance Hashable WriteSegmentRequest where

instance NFData WriteSegmentRequest where

instance ToJSON WriteSegmentRequest where
        toJSON WriteSegmentRequest'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _wsrName,
                  ("Dimensions" .=) <$> _wsrDimensions])

-- | Used to create a campaign treatment.
--
-- /See:/ 'writeTreatmentResource' smart constructor.
data WriteTreatmentResource = WriteTreatmentResource'
  { _wtrSchedule             :: !(Maybe Schedule)
  , _wtrTreatmentName        :: !(Maybe Text)
  , _wtrSizePercent          :: !(Maybe Int)
  , _wtrTreatmentDescription :: !(Maybe Text)
  , _wtrMessageConfiguration :: !(Maybe MessageConfiguration)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WriteTreatmentResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtrSchedule' - The campaign schedule.
--
-- * 'wtrTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'wtrSizePercent' - The allocated percentage of users for this treatment.
--
-- * 'wtrTreatmentDescription' - A custom description for the treatment.
--
-- * 'wtrMessageConfiguration' - The message configuration settings.
writeTreatmentResource
    :: WriteTreatmentResource
writeTreatmentResource =
  WriteTreatmentResource'
    { _wtrSchedule = Nothing
    , _wtrTreatmentName = Nothing
    , _wtrSizePercent = Nothing
    , _wtrTreatmentDescription = Nothing
    , _wtrMessageConfiguration = Nothing
    }


-- | The campaign schedule.
wtrSchedule :: Lens' WriteTreatmentResource (Maybe Schedule)
wtrSchedule = lens _wtrSchedule (\ s a -> s{_wtrSchedule = a})

-- | The custom name of a variation of the campaign used for A/B testing.
wtrTreatmentName :: Lens' WriteTreatmentResource (Maybe Text)
wtrTreatmentName = lens _wtrTreatmentName (\ s a -> s{_wtrTreatmentName = a})

-- | The allocated percentage of users for this treatment.
wtrSizePercent :: Lens' WriteTreatmentResource (Maybe Int)
wtrSizePercent = lens _wtrSizePercent (\ s a -> s{_wtrSizePercent = a})

-- | A custom description for the treatment.
wtrTreatmentDescription :: Lens' WriteTreatmentResource (Maybe Text)
wtrTreatmentDescription = lens _wtrTreatmentDescription (\ s a -> s{_wtrTreatmentDescription = a})

-- | The message configuration settings.
wtrMessageConfiguration :: Lens' WriteTreatmentResource (Maybe MessageConfiguration)
wtrMessageConfiguration = lens _wtrMessageConfiguration (\ s a -> s{_wtrMessageConfiguration = a})

instance Hashable WriteTreatmentResource where

instance NFData WriteTreatmentResource where

instance ToJSON WriteTreatmentResource where
        toJSON WriteTreatmentResource'{..}
          = object
              (catMaybes
                 [("Schedule" .=) <$> _wtrSchedule,
                  ("TreatmentName" .=) <$> _wtrTreatmentName,
                  ("SizePercent" .=) <$> _wtrSizePercent,
                  ("TreatmentDescription" .=) <$>
                    _wtrTreatmentDescription,
                  ("MessageConfiguration" .=) <$>
                    _wtrMessageConfiguration])
