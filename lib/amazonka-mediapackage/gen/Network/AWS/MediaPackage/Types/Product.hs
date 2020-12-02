{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.Product where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.Sum
import Network.AWS.Prelude

-- | A Channel resource configuration.
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cHlsIngest   :: !(Maybe HlsIngest)
  , _cARN         :: !(Maybe Text)
  , _cId          :: !(Maybe Text)
  , _cDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cHlsIngest' - Undocumented member.
--
-- * 'cARN' - The Amazon Resource Name (ARN) assigned to the Channel.
--
-- * 'cId' - The ID of the Channel.
--
-- * 'cDescription' - A short text description of the Channel.
channel
    :: Channel
channel =
  Channel'
    { _cHlsIngest = Nothing
    , _cARN = Nothing
    , _cId = Nothing
    , _cDescription = Nothing
    }


-- | Undocumented member.
cHlsIngest :: Lens' Channel (Maybe HlsIngest)
cHlsIngest = lens _cHlsIngest (\ s a -> s{_cHlsIngest = a})

-- | The Amazon Resource Name (ARN) assigned to the Channel.
cARN :: Lens' Channel (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | The ID of the Channel.
cId :: Lens' Channel (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | A short text description of the Channel.
cDescription :: Lens' Channel (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ x ->
                 Channel' <$>
                   (x .:? "hlsIngest") <*> (x .:? "arn") <*>
                     (x .:? "id")
                     <*> (x .:? "description"))

instance Hashable Channel where

instance NFData Channel where

-- | A Common Media Application Format (CMAF) encryption configuration.
--
-- /See:/ 'cmafEncryption' smart constructor.
data CmafEncryption = CmafEncryption'
  { _ceKeyRotationIntervalSeconds :: !(Maybe Int)
  , _ceSpekeKeyProvider           :: !SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CmafEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceKeyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- * 'ceSpekeKeyProvider' - Undocumented member.
cmafEncryption
    :: SpekeKeyProvider -- ^ 'ceSpekeKeyProvider'
    -> CmafEncryption
cmafEncryption pSpekeKeyProvider_ =
  CmafEncryption'
    { _ceKeyRotationIntervalSeconds = Nothing
    , _ceSpekeKeyProvider = pSpekeKeyProvider_
    }


-- | Time (in seconds) between each encryption key rotation.
ceKeyRotationIntervalSeconds :: Lens' CmafEncryption (Maybe Int)
ceKeyRotationIntervalSeconds = lens _ceKeyRotationIntervalSeconds (\ s a -> s{_ceKeyRotationIntervalSeconds = a})

-- | Undocumented member.
ceSpekeKeyProvider :: Lens' CmafEncryption SpekeKeyProvider
ceSpekeKeyProvider = lens _ceSpekeKeyProvider (\ s a -> s{_ceSpekeKeyProvider = a})

instance FromJSON CmafEncryption where
        parseJSON
          = withObject "CmafEncryption"
              (\ x ->
                 CmafEncryption' <$>
                   (x .:? "keyRotationIntervalSeconds") <*>
                     (x .: "spekeKeyProvider"))

instance Hashable CmafEncryption where

instance NFData CmafEncryption where

instance ToJSON CmafEncryption where
        toJSON CmafEncryption'{..}
          = object
              (catMaybes
                 [("keyRotationIntervalSeconds" .=) <$>
                    _ceKeyRotationIntervalSeconds,
                  Just ("spekeKeyProvider" .= _ceSpekeKeyProvider)])

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'cmafPackage' smart constructor.
data CmafPackage = CmafPackage'
  { _cpHlsManifests           :: !(Maybe [HlsManifest])
  , _cpSegmentDurationSeconds :: !(Maybe Int)
  , _cpStreamSelection        :: !(Maybe StreamSelection)
  , _cpEncryption             :: !(Maybe CmafEncryption)
  , _cpSegmentPrefix          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CmafPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpHlsManifests' - A list of HLS manifest configurations
--
-- * 'cpSegmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
--
-- * 'cpStreamSelection' - Undocumented member.
--
-- * 'cpEncryption' - Undocumented member.
--
-- * 'cpSegmentPrefix' - An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cmafPackage
    :: CmafPackage
cmafPackage =
  CmafPackage'
    { _cpHlsManifests = Nothing
    , _cpSegmentDurationSeconds = Nothing
    , _cpStreamSelection = Nothing
    , _cpEncryption = Nothing
    , _cpSegmentPrefix = Nothing
    }


-- | A list of HLS manifest configurations
cpHlsManifests :: Lens' CmafPackage [HlsManifest]
cpHlsManifests = lens _cpHlsManifests (\ s a -> s{_cpHlsManifests = a}) . _Default . _Coerce

-- | Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
cpSegmentDurationSeconds :: Lens' CmafPackage (Maybe Int)
cpSegmentDurationSeconds = lens _cpSegmentDurationSeconds (\ s a -> s{_cpSegmentDurationSeconds = a})

-- | Undocumented member.
cpStreamSelection :: Lens' CmafPackage (Maybe StreamSelection)
cpStreamSelection = lens _cpStreamSelection (\ s a -> s{_cpStreamSelection = a})

-- | Undocumented member.
cpEncryption :: Lens' CmafPackage (Maybe CmafEncryption)
cpEncryption = lens _cpEncryption (\ s a -> s{_cpEncryption = a})

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cpSegmentPrefix :: Lens' CmafPackage (Maybe Text)
cpSegmentPrefix = lens _cpSegmentPrefix (\ s a -> s{_cpSegmentPrefix = a})

instance FromJSON CmafPackage where
        parseJSON
          = withObject "CmafPackage"
              (\ x ->
                 CmafPackage' <$>
                   (x .:? "hlsManifests" .!= mempty) <*>
                     (x .:? "segmentDurationSeconds")
                     <*> (x .:? "streamSelection")
                     <*> (x .:? "encryption")
                     <*> (x .:? "segmentPrefix"))

instance Hashable CmafPackage where

instance NFData CmafPackage where

-- | A Common Media Application Format (CMAF) packaging configuration.
--
-- /See:/ 'cmafPackageCreateOrUpdateParameters' smart constructor.
data CmafPackageCreateOrUpdateParameters = CmafPackageCreateOrUpdateParameters'
  { _cpcoupHlsManifests :: !(Maybe [HlsManifestCreateOrUpdateParameters])
  , _cpcoupSegmentDurationSeconds :: !(Maybe Int)
  , _cpcoupStreamSelection :: !(Maybe StreamSelection)
  , _cpcoupEncryption :: !(Maybe CmafEncryption)
  , _cpcoupSegmentPrefix :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CmafPackageCreateOrUpdateParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpcoupHlsManifests' - A list of HLS manifest configurations
--
-- * 'cpcoupSegmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
--
-- * 'cpcoupStreamSelection' - Undocumented member.
--
-- * 'cpcoupEncryption' - Undocumented member.
--
-- * 'cpcoupSegmentPrefix' - An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cmafPackageCreateOrUpdateParameters
    :: CmafPackageCreateOrUpdateParameters
cmafPackageCreateOrUpdateParameters =
  CmafPackageCreateOrUpdateParameters'
    { _cpcoupHlsManifests = Nothing
    , _cpcoupSegmentDurationSeconds = Nothing
    , _cpcoupStreamSelection = Nothing
    , _cpcoupEncryption = Nothing
    , _cpcoupSegmentPrefix = Nothing
    }


-- | A list of HLS manifest configurations
cpcoupHlsManifests :: Lens' CmafPackageCreateOrUpdateParameters [HlsManifestCreateOrUpdateParameters]
cpcoupHlsManifests = lens _cpcoupHlsManifests (\ s a -> s{_cpcoupHlsManifests = a}) . _Default . _Coerce

-- | Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
cpcoupSegmentDurationSeconds :: Lens' CmafPackageCreateOrUpdateParameters (Maybe Int)
cpcoupSegmentDurationSeconds = lens _cpcoupSegmentDurationSeconds (\ s a -> s{_cpcoupSegmentDurationSeconds = a})

-- | Undocumented member.
cpcoupStreamSelection :: Lens' CmafPackageCreateOrUpdateParameters (Maybe StreamSelection)
cpcoupStreamSelection = lens _cpcoupStreamSelection (\ s a -> s{_cpcoupStreamSelection = a})

-- | Undocumented member.
cpcoupEncryption :: Lens' CmafPackageCreateOrUpdateParameters (Maybe CmafEncryption)
cpcoupEncryption = lens _cpcoupEncryption (\ s a -> s{_cpcoupEncryption = a})

-- | An optional custom string that is prepended to the name of each segment. If not specified, it defaults to the ChannelId.
cpcoupSegmentPrefix :: Lens' CmafPackageCreateOrUpdateParameters (Maybe Text)
cpcoupSegmentPrefix = lens _cpcoupSegmentPrefix (\ s a -> s{_cpcoupSegmentPrefix = a})

instance Hashable CmafPackageCreateOrUpdateParameters
         where

instance NFData CmafPackageCreateOrUpdateParameters
         where

instance ToJSON CmafPackageCreateOrUpdateParameters
         where
        toJSON CmafPackageCreateOrUpdateParameters'{..}
          = object
              (catMaybes
                 [("hlsManifests" .=) <$> _cpcoupHlsManifests,
                  ("segmentDurationSeconds" .=) <$>
                    _cpcoupSegmentDurationSeconds,
                  ("streamSelection" .=) <$> _cpcoupStreamSelection,
                  ("encryption" .=) <$> _cpcoupEncryption,
                  ("segmentPrefix" .=) <$> _cpcoupSegmentPrefix])

-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
--
-- /See:/ 'dashEncryption' smart constructor.
data DashEncryption = DashEncryption'
  { _deKeyRotationIntervalSeconds :: !(Maybe Int)
  , _deSpekeKeyProvider           :: !SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deKeyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- * 'deSpekeKeyProvider' - Undocumented member.
dashEncryption
    :: SpekeKeyProvider -- ^ 'deSpekeKeyProvider'
    -> DashEncryption
dashEncryption pSpekeKeyProvider_ =
  DashEncryption'
    { _deKeyRotationIntervalSeconds = Nothing
    , _deSpekeKeyProvider = pSpekeKeyProvider_
    }


-- | Time (in seconds) between each encryption key rotation.
deKeyRotationIntervalSeconds :: Lens' DashEncryption (Maybe Int)
deKeyRotationIntervalSeconds = lens _deKeyRotationIntervalSeconds (\ s a -> s{_deKeyRotationIntervalSeconds = a})

-- | Undocumented member.
deSpekeKeyProvider :: Lens' DashEncryption SpekeKeyProvider
deSpekeKeyProvider = lens _deSpekeKeyProvider (\ s a -> s{_deSpekeKeyProvider = a})

instance FromJSON DashEncryption where
        parseJSON
          = withObject "DashEncryption"
              (\ x ->
                 DashEncryption' <$>
                   (x .:? "keyRotationIntervalSeconds") <*>
                     (x .: "spekeKeyProvider"))

instance Hashable DashEncryption where

instance NFData DashEncryption where

instance ToJSON DashEncryption where
        toJSON DashEncryption'{..}
          = object
              (catMaybes
                 [("keyRotationIntervalSeconds" .=) <$>
                    _deKeyRotationIntervalSeconds,
                  Just ("spekeKeyProvider" .= _deSpekeKeyProvider)])

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'dashPackage' smart constructor.
data DashPackage = DashPackage'
  { _dpMinBufferTimeSeconds              :: !(Maybe Int)
  , _dpProfile                           :: !(Maybe Profile)
  , _dpSegmentDurationSeconds            :: !(Maybe Int)
  , _dpStreamSelection                   :: !(Maybe StreamSelection)
  , _dpEncryption                        :: !(Maybe DashEncryption)
  , _dpMinUpdatePeriodSeconds            :: !(Maybe Int)
  , _dpSuggestedPresentationDelaySeconds :: !(Maybe Int)
  , _dpManifestWindowSeconds             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpMinBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before starting the presentation.
--
-- * 'dpProfile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
--
-- * 'dpSegmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
--
-- * 'dpStreamSelection' - Undocumented member.
--
-- * 'dpEncryption' - Undocumented member.
--
-- * 'dpMinUpdatePeriodSeconds' - Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
--
-- * 'dpSuggestedPresentationDelaySeconds' - Duration (in seconds) to delay live content before presentation.
--
-- * 'dpManifestWindowSeconds' - Time window (in seconds) contained in each manifest.
dashPackage
    :: DashPackage
dashPackage =
  DashPackage'
    { _dpMinBufferTimeSeconds = Nothing
    , _dpProfile = Nothing
    , _dpSegmentDurationSeconds = Nothing
    , _dpStreamSelection = Nothing
    , _dpEncryption = Nothing
    , _dpMinUpdatePeriodSeconds = Nothing
    , _dpSuggestedPresentationDelaySeconds = Nothing
    , _dpManifestWindowSeconds = Nothing
    }


-- | Minimum duration (in seconds) that a player will buffer media before starting the presentation.
dpMinBufferTimeSeconds :: Lens' DashPackage (Maybe Int)
dpMinBufferTimeSeconds = lens _dpMinBufferTimeSeconds (\ s a -> s{_dpMinBufferTimeSeconds = a})

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type.  When set to "HBBTV_1_5", HbbTV 1.5 compliant output is enabled.
dpProfile :: Lens' DashPackage (Maybe Profile)
dpProfile = lens _dpProfile (\ s a -> s{_dpProfile = a})

-- | Duration (in seconds) of each segment. Actual segments will be rounded to the nearest multiple of the source segment duration.
dpSegmentDurationSeconds :: Lens' DashPackage (Maybe Int)
dpSegmentDurationSeconds = lens _dpSegmentDurationSeconds (\ s a -> s{_dpSegmentDurationSeconds = a})

-- | Undocumented member.
dpStreamSelection :: Lens' DashPackage (Maybe StreamSelection)
dpStreamSelection = lens _dpStreamSelection (\ s a -> s{_dpStreamSelection = a})

-- | Undocumented member.
dpEncryption :: Lens' DashPackage (Maybe DashEncryption)
dpEncryption = lens _dpEncryption (\ s a -> s{_dpEncryption = a})

-- | Minimum duration (in seconds) between potential changes to the Dynamic Adaptive Streaming over HTTP (DASH) Media Presentation Description (MPD).
dpMinUpdatePeriodSeconds :: Lens' DashPackage (Maybe Int)
dpMinUpdatePeriodSeconds = lens _dpMinUpdatePeriodSeconds (\ s a -> s{_dpMinUpdatePeriodSeconds = a})

-- | Duration (in seconds) to delay live content before presentation.
dpSuggestedPresentationDelaySeconds :: Lens' DashPackage (Maybe Int)
dpSuggestedPresentationDelaySeconds = lens _dpSuggestedPresentationDelaySeconds (\ s a -> s{_dpSuggestedPresentationDelaySeconds = a})

-- | Time window (in seconds) contained in each manifest.
dpManifestWindowSeconds :: Lens' DashPackage (Maybe Int)
dpManifestWindowSeconds = lens _dpManifestWindowSeconds (\ s a -> s{_dpManifestWindowSeconds = a})

instance FromJSON DashPackage where
        parseJSON
          = withObject "DashPackage"
              (\ x ->
                 DashPackage' <$>
                   (x .:? "minBufferTimeSeconds") <*> (x .:? "profile")
                     <*> (x .:? "segmentDurationSeconds")
                     <*> (x .:? "streamSelection")
                     <*> (x .:? "encryption")
                     <*> (x .:? "minUpdatePeriodSeconds")
                     <*> (x .:? "suggestedPresentationDelaySeconds")
                     <*> (x .:? "manifestWindowSeconds"))

instance Hashable DashPackage where

instance NFData DashPackage where

instance ToJSON DashPackage where
        toJSON DashPackage'{..}
          = object
              (catMaybes
                 [("minBufferTimeSeconds" .=) <$>
                    _dpMinBufferTimeSeconds,
                  ("profile" .=) <$> _dpProfile,
                  ("segmentDurationSeconds" .=) <$>
                    _dpSegmentDurationSeconds,
                  ("streamSelection" .=) <$> _dpStreamSelection,
                  ("encryption" .=) <$> _dpEncryption,
                  ("minUpdatePeriodSeconds" .=) <$>
                    _dpMinUpdatePeriodSeconds,
                  ("suggestedPresentationDelaySeconds" .=) <$>
                    _dpSuggestedPresentationDelaySeconds,
                  ("manifestWindowSeconds" .=) <$>
                    _dpManifestWindowSeconds])

-- | An HTTP Live Streaming (HLS) encryption configuration.
--
-- /See:/ 'hlsEncryption' smart constructor.
data HlsEncryption = HlsEncryption'
  { _heEncryptionMethod             :: !(Maybe EncryptionMethod)
  , _heKeyRotationIntervalSeconds   :: !(Maybe Int)
  , _heConstantInitializationVector :: !(Maybe Text)
  , _heRepeatExtXKey                :: !(Maybe Bool)
  , _heSpekeKeyProvider             :: !SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heEncryptionMethod' - The encryption method to use.
--
-- * 'heKeyRotationIntervalSeconds' - Interval (in seconds) between each encryption key rotation.
--
-- * 'heConstantInitializationVector' - A constant initialization vector for encryption (optional). When not specified the initialization vector will be periodically rotated.
--
-- * 'heRepeatExtXKey' - When enabled, the EXT-X-KEY tag will be repeated in output manifests.
--
-- * 'heSpekeKeyProvider' - Undocumented member.
hlsEncryption
    :: SpekeKeyProvider -- ^ 'heSpekeKeyProvider'
    -> HlsEncryption
hlsEncryption pSpekeKeyProvider_ =
  HlsEncryption'
    { _heEncryptionMethod = Nothing
    , _heKeyRotationIntervalSeconds = Nothing
    , _heConstantInitializationVector = Nothing
    , _heRepeatExtXKey = Nothing
    , _heSpekeKeyProvider = pSpekeKeyProvider_
    }


-- | The encryption method to use.
heEncryptionMethod :: Lens' HlsEncryption (Maybe EncryptionMethod)
heEncryptionMethod = lens _heEncryptionMethod (\ s a -> s{_heEncryptionMethod = a})

-- | Interval (in seconds) between each encryption key rotation.
heKeyRotationIntervalSeconds :: Lens' HlsEncryption (Maybe Int)
heKeyRotationIntervalSeconds = lens _heKeyRotationIntervalSeconds (\ s a -> s{_heKeyRotationIntervalSeconds = a})

-- | A constant initialization vector for encryption (optional). When not specified the initialization vector will be periodically rotated.
heConstantInitializationVector :: Lens' HlsEncryption (Maybe Text)
heConstantInitializationVector = lens _heConstantInitializationVector (\ s a -> s{_heConstantInitializationVector = a})

-- | When enabled, the EXT-X-KEY tag will be repeated in output manifests.
heRepeatExtXKey :: Lens' HlsEncryption (Maybe Bool)
heRepeatExtXKey = lens _heRepeatExtXKey (\ s a -> s{_heRepeatExtXKey = a})

-- | Undocumented member.
heSpekeKeyProvider :: Lens' HlsEncryption SpekeKeyProvider
heSpekeKeyProvider = lens _heSpekeKeyProvider (\ s a -> s{_heSpekeKeyProvider = a})

instance FromJSON HlsEncryption where
        parseJSON
          = withObject "HlsEncryption"
              (\ x ->
                 HlsEncryption' <$>
                   (x .:? "encryptionMethod") <*>
                     (x .:? "keyRotationIntervalSeconds")
                     <*> (x .:? "constantInitializationVector")
                     <*> (x .:? "repeatExtXKey")
                     <*> (x .: "spekeKeyProvider"))

instance Hashable HlsEncryption where

instance NFData HlsEncryption where

instance ToJSON HlsEncryption where
        toJSON HlsEncryption'{..}
          = object
              (catMaybes
                 [("encryptionMethod" .=) <$> _heEncryptionMethod,
                  ("keyRotationIntervalSeconds" .=) <$>
                    _heKeyRotationIntervalSeconds,
                  ("constantInitializationVector" .=) <$>
                    _heConstantInitializationVector,
                  ("repeatExtXKey" .=) <$> _heRepeatExtXKey,
                  Just ("spekeKeyProvider" .= _heSpekeKeyProvider)])

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'hlsIngest' smart constructor.
newtype HlsIngest = HlsIngest'
  { _hiIngestEndpoints :: Maybe [IngestEndpoint]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsIngest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hiIngestEndpoints' - A list of endpoints to which the source stream should be sent.
hlsIngest
    :: HlsIngest
hlsIngest = HlsIngest' {_hiIngestEndpoints = Nothing}


-- | A list of endpoints to which the source stream should be sent.
hiIngestEndpoints :: Lens' HlsIngest [IngestEndpoint]
hiIngestEndpoints = lens _hiIngestEndpoints (\ s a -> s{_hiIngestEndpoints = a}) . _Default . _Coerce

instance FromJSON HlsIngest where
        parseJSON
          = withObject "HlsIngest"
              (\ x ->
                 HlsIngest' <$> (x .:? "ingestEndpoints" .!= mempty))

instance Hashable HlsIngest where

instance NFData HlsIngest where

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'hlsManifest' smart constructor.
data HlsManifest = HlsManifest'
  { _hmManifestName                   :: !(Maybe Text)
  , _hmURL                            :: !(Maybe Text)
  , _hmPlaylistType                   :: !(Maybe PlaylistType)
  , _hmProgramDateTimeIntervalSeconds :: !(Maybe Int)
  , _hmAdMarkers                      :: !(Maybe AdMarkers)
  , _hmIncludeIframeOnlyStream        :: !(Maybe Bool)
  , _hmPlaylistWindowSeconds          :: !(Maybe Int)
  , _hmId                             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsManifest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmManifestName' - An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- * 'hmURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'hmPlaylistType' - The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
--
-- * 'hmProgramDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
--
-- * 'hmAdMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
--
-- * 'hmIncludeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- * 'hmPlaylistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- * 'hmId' - The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hlsManifest
    :: Text -- ^ 'hmId'
    -> HlsManifest
hlsManifest pId_ =
  HlsManifest'
    { _hmManifestName = Nothing
    , _hmURL = Nothing
    , _hmPlaylistType = Nothing
    , _hmProgramDateTimeIntervalSeconds = Nothing
    , _hmAdMarkers = Nothing
    , _hmIncludeIframeOnlyStream = Nothing
    , _hmPlaylistWindowSeconds = Nothing
    , _hmId = pId_
    }


-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
hmManifestName :: Lens' HlsManifest (Maybe Text)
hmManifestName = lens _hmManifestName (\ s a -> s{_hmManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
hmURL :: Lens' HlsManifest (Maybe Text)
hmURL = lens _hmURL (\ s a -> s{_hmURL = a})

-- | The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
hmPlaylistType :: Lens' HlsManifest (Maybe PlaylistType)
hmPlaylistType = lens _hmPlaylistType (\ s a -> s{_hmPlaylistType = a})

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
hmProgramDateTimeIntervalSeconds :: Lens' HlsManifest (Maybe Int)
hmProgramDateTimeIntervalSeconds = lens _hmProgramDateTimeIntervalSeconds (\ s a -> s{_hmProgramDateTimeIntervalSeconds = a})

-- | This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
hmAdMarkers :: Lens' HlsManifest (Maybe AdMarkers)
hmAdMarkers = lens _hmAdMarkers (\ s a -> s{_hmAdMarkers = a})

-- | When enabled, an I-Frame only stream will be included in the output.
hmIncludeIframeOnlyStream :: Lens' HlsManifest (Maybe Bool)
hmIncludeIframeOnlyStream = lens _hmIncludeIframeOnlyStream (\ s a -> s{_hmIncludeIframeOnlyStream = a})

-- | Time window (in seconds) contained in each parent manifest.
hmPlaylistWindowSeconds :: Lens' HlsManifest (Maybe Int)
hmPlaylistWindowSeconds = lens _hmPlaylistWindowSeconds (\ s a -> s{_hmPlaylistWindowSeconds = a})

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hmId :: Lens' HlsManifest Text
hmId = lens _hmId (\ s a -> s{_hmId = a})

instance FromJSON HlsManifest where
        parseJSON
          = withObject "HlsManifest"
              (\ x ->
                 HlsManifest' <$>
                   (x .:? "manifestName") <*> (x .:? "url") <*>
                     (x .:? "playlistType")
                     <*> (x .:? "programDateTimeIntervalSeconds")
                     <*> (x .:? "adMarkers")
                     <*> (x .:? "includeIframeOnlyStream")
                     <*> (x .:? "playlistWindowSeconds")
                     <*> (x .: "id"))

instance Hashable HlsManifest where

instance NFData HlsManifest where

-- | A HTTP Live Streaming (HLS) manifest configuration.
--
-- /See:/ 'hlsManifestCreateOrUpdateParameters' smart constructor.
data HlsManifestCreateOrUpdateParameters = HlsManifestCreateOrUpdateParameters'
  { _hmcoupManifestName                   :: !(Maybe Text)
  , _hmcoupPlaylistType                   :: !(Maybe PlaylistType)
  , _hmcoupProgramDateTimeIntervalSeconds :: !(Maybe Int)
  , _hmcoupAdMarkers                      :: !(Maybe AdMarkers)
  , _hmcoupIncludeIframeOnlyStream        :: !(Maybe Bool)
  , _hmcoupPlaylistWindowSeconds          :: !(Maybe Int)
  , _hmcoupId                             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsManifestCreateOrUpdateParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmcoupManifestName' - An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
--
-- * 'hmcoupPlaylistType' - The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
--
-- * 'hmcoupProgramDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
--
-- * 'hmcoupAdMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
--
-- * 'hmcoupIncludeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- * 'hmcoupPlaylistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
--
-- * 'hmcoupId' - The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hlsManifestCreateOrUpdateParameters
    :: Text -- ^ 'hmcoupId'
    -> HlsManifestCreateOrUpdateParameters
hlsManifestCreateOrUpdateParameters pId_ =
  HlsManifestCreateOrUpdateParameters'
    { _hmcoupManifestName = Nothing
    , _hmcoupPlaylistType = Nothing
    , _hmcoupProgramDateTimeIntervalSeconds = Nothing
    , _hmcoupAdMarkers = Nothing
    , _hmcoupIncludeIframeOnlyStream = Nothing
    , _hmcoupPlaylistWindowSeconds = Nothing
    , _hmcoupId = pId_
    }


-- | An optional short string appended to the end of the OriginEndpoint URL. If not specified, defaults to the manifestName for the OriginEndpoint.
hmcoupManifestName :: Lens' HlsManifestCreateOrUpdateParameters (Maybe Text)
hmcoupManifestName = lens _hmcoupManifestName (\ s a -> s{_hmcoupManifestName = a})

-- | The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
hmcoupPlaylistType :: Lens' HlsManifestCreateOrUpdateParameters (Maybe PlaylistType)
hmcoupPlaylistType = lens _hmcoupPlaylistType (\ s a -> s{_hmcoupPlaylistType = a})

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
hmcoupProgramDateTimeIntervalSeconds :: Lens' HlsManifestCreateOrUpdateParameters (Maybe Int)
hmcoupProgramDateTimeIntervalSeconds = lens _hmcoupProgramDateTimeIntervalSeconds (\ s a -> s{_hmcoupProgramDateTimeIntervalSeconds = a})

-- | This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
hmcoupAdMarkers :: Lens' HlsManifestCreateOrUpdateParameters (Maybe AdMarkers)
hmcoupAdMarkers = lens _hmcoupAdMarkers (\ s a -> s{_hmcoupAdMarkers = a})

-- | When enabled, an I-Frame only stream will be included in the output.
hmcoupIncludeIframeOnlyStream :: Lens' HlsManifestCreateOrUpdateParameters (Maybe Bool)
hmcoupIncludeIframeOnlyStream = lens _hmcoupIncludeIframeOnlyStream (\ s a -> s{_hmcoupIncludeIframeOnlyStream = a})

-- | Time window (in seconds) contained in each parent manifest.
hmcoupPlaylistWindowSeconds :: Lens' HlsManifestCreateOrUpdateParameters (Maybe Int)
hmcoupPlaylistWindowSeconds = lens _hmcoupPlaylistWindowSeconds (\ s a -> s{_hmcoupPlaylistWindowSeconds = a})

-- | The ID of the manifest. The ID must be unique within the OriginEndpoint and it cannot be changed after it is created.
hmcoupId :: Lens' HlsManifestCreateOrUpdateParameters Text
hmcoupId = lens _hmcoupId (\ s a -> s{_hmcoupId = a})

instance Hashable HlsManifestCreateOrUpdateParameters
         where

instance NFData HlsManifestCreateOrUpdateParameters
         where

instance ToJSON HlsManifestCreateOrUpdateParameters
         where
        toJSON HlsManifestCreateOrUpdateParameters'{..}
          = object
              (catMaybes
                 [("manifestName" .=) <$> _hmcoupManifestName,
                  ("playlistType" .=) <$> _hmcoupPlaylistType,
                  ("programDateTimeIntervalSeconds" .=) <$>
                    _hmcoupProgramDateTimeIntervalSeconds,
                  ("adMarkers" .=) <$> _hmcoupAdMarkers,
                  ("includeIframeOnlyStream" .=) <$>
                    _hmcoupIncludeIframeOnlyStream,
                  ("playlistWindowSeconds" .=) <$>
                    _hmcoupPlaylistWindowSeconds,
                  Just ("id" .= _hmcoupId)])

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'hlsPackage' smart constructor.
data HlsPackage = HlsPackage'
  { _hpUseAudioRenditionGroup         :: !(Maybe Bool)
  , _hpPlaylistType                   :: !(Maybe PlaylistType)
  , _hpSegmentDurationSeconds         :: !(Maybe Int)
  , _hpProgramDateTimeIntervalSeconds :: !(Maybe Int)
  , _hpStreamSelection                :: !(Maybe StreamSelection)
  , _hpAdMarkers                      :: !(Maybe AdMarkers)
  , _hpEncryption                     :: !(Maybe HlsEncryption)
  , _hpIncludeIframeOnlyStream        :: !(Maybe Bool)
  , _hpPlaylistWindowSeconds          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpUseAudioRenditionGroup' - When enabled, audio streams will be placed in rendition groups in the output.
--
-- * 'hpPlaylistType' - The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
--
-- * 'hpSegmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be rounded to the nearest multiple of the source fragment duration.
--
-- * 'hpProgramDateTimeIntervalSeconds' - The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
--
-- * 'hpStreamSelection' - Undocumented member.
--
-- * 'hpAdMarkers' - This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
--
-- * 'hpEncryption' - Undocumented member.
--
-- * 'hpIncludeIframeOnlyStream' - When enabled, an I-Frame only stream will be included in the output.
--
-- * 'hpPlaylistWindowSeconds' - Time window (in seconds) contained in each parent manifest.
hlsPackage
    :: HlsPackage
hlsPackage =
  HlsPackage'
    { _hpUseAudioRenditionGroup = Nothing
    , _hpPlaylistType = Nothing
    , _hpSegmentDurationSeconds = Nothing
    , _hpProgramDateTimeIntervalSeconds = Nothing
    , _hpStreamSelection = Nothing
    , _hpAdMarkers = Nothing
    , _hpEncryption = Nothing
    , _hpIncludeIframeOnlyStream = Nothing
    , _hpPlaylistWindowSeconds = Nothing
    }


-- | When enabled, audio streams will be placed in rendition groups in the output.
hpUseAudioRenditionGroup :: Lens' HlsPackage (Maybe Bool)
hpUseAudioRenditionGroup = lens _hpUseAudioRenditionGroup (\ s a -> s{_hpUseAudioRenditionGroup = a})

-- | The HTTP Live Streaming (HLS) playlist type. When either "EVENT" or "VOD" is specified, a corresponding EXT-X-PLAYLIST-TYPE entry will be included in the media playlist.
hpPlaylistType :: Lens' HlsPackage (Maybe PlaylistType)
hpPlaylistType = lens _hpPlaylistType (\ s a -> s{_hpPlaylistType = a})

-- | Duration (in seconds) of each fragment. Actual fragments will be rounded to the nearest multiple of the source fragment duration.
hpSegmentDurationSeconds :: Lens' HlsPackage (Maybe Int)
hpSegmentDurationSeconds = lens _hpSegmentDurationSeconds (\ s a -> s{_hpSegmentDurationSeconds = a})

-- | The interval (in seconds) between each EXT-X-PROGRAM-DATE-TIME tag inserted into manifests. Additionally, when an interval is specified ID3Timed Metadata messages will be generated every 5 seconds using the ingest time of the content. If the interval is not specified, or set to 0, then no EXT-X-PROGRAM-DATE-TIME tags will be inserted into manifests and no ID3Timed Metadata messages will be generated. Note that irrespective of this parameter, if any ID3 Timed Metadata is found in HTTP Live Streaming (HLS) input, it will be passed through to HLS output.
hpProgramDateTimeIntervalSeconds :: Lens' HlsPackage (Maybe Int)
hpProgramDateTimeIntervalSeconds = lens _hpProgramDateTimeIntervalSeconds (\ s a -> s{_hpProgramDateTimeIntervalSeconds = a})

-- | Undocumented member.
hpStreamSelection :: Lens' HlsPackage (Maybe StreamSelection)
hpStreamSelection = lens _hpStreamSelection (\ s a -> s{_hpStreamSelection = a})

-- | This setting controls how ad markers are included in the packaged OriginEndpoint. "NONE" will omit all SCTE-35 ad markers from the output. "PASSTHROUGH" causes the manifest to contain a copy of the SCTE-35 ad markers (comments) taken directly from the input HTTP Live Streaming (HLS) manifest. "SCTE35_ENHANCED" generates ad markers and blackout tags based on SCTE-35 messages in the input source.
hpAdMarkers :: Lens' HlsPackage (Maybe AdMarkers)
hpAdMarkers = lens _hpAdMarkers (\ s a -> s{_hpAdMarkers = a})

-- | Undocumented member.
hpEncryption :: Lens' HlsPackage (Maybe HlsEncryption)
hpEncryption = lens _hpEncryption (\ s a -> s{_hpEncryption = a})

-- | When enabled, an I-Frame only stream will be included in the output.
hpIncludeIframeOnlyStream :: Lens' HlsPackage (Maybe Bool)
hpIncludeIframeOnlyStream = lens _hpIncludeIframeOnlyStream (\ s a -> s{_hpIncludeIframeOnlyStream = a})

-- | Time window (in seconds) contained in each parent manifest.
hpPlaylistWindowSeconds :: Lens' HlsPackage (Maybe Int)
hpPlaylistWindowSeconds = lens _hpPlaylistWindowSeconds (\ s a -> s{_hpPlaylistWindowSeconds = a})

instance FromJSON HlsPackage where
        parseJSON
          = withObject "HlsPackage"
              (\ x ->
                 HlsPackage' <$>
                   (x .:? "useAudioRenditionGroup") <*>
                     (x .:? "playlistType")
                     <*> (x .:? "segmentDurationSeconds")
                     <*> (x .:? "programDateTimeIntervalSeconds")
                     <*> (x .:? "streamSelection")
                     <*> (x .:? "adMarkers")
                     <*> (x .:? "encryption")
                     <*> (x .:? "includeIframeOnlyStream")
                     <*> (x .:? "playlistWindowSeconds"))

instance Hashable HlsPackage where

instance NFData HlsPackage where

instance ToJSON HlsPackage where
        toJSON HlsPackage'{..}
          = object
              (catMaybes
                 [("useAudioRenditionGroup" .=) <$>
                    _hpUseAudioRenditionGroup,
                  ("playlistType" .=) <$> _hpPlaylistType,
                  ("segmentDurationSeconds" .=) <$>
                    _hpSegmentDurationSeconds,
                  ("programDateTimeIntervalSeconds" .=) <$>
                    _hpProgramDateTimeIntervalSeconds,
                  ("streamSelection" .=) <$> _hpStreamSelection,
                  ("adMarkers" .=) <$> _hpAdMarkers,
                  ("encryption" .=) <$> _hpEncryption,
                  ("includeIframeOnlyStream" .=) <$>
                    _hpIncludeIframeOnlyStream,
                  ("playlistWindowSeconds" .=) <$>
                    _hpPlaylistWindowSeconds])

-- | An endpoint for ingesting source content for a Channel.
--
-- /See:/ 'ingestEndpoint' smart constructor.
data IngestEndpoint = IngestEndpoint'
  { _ieURL      :: !(Maybe Text)
  , _ieUsername :: !(Maybe Text)
  , _iePassword :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IngestEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieURL' - The ingest URL to which the source stream should be sent.
--
-- * 'ieUsername' - The system generated username for ingest authentication.
--
-- * 'iePassword' - The system generated password for ingest authentication.
ingestEndpoint
    :: IngestEndpoint
ingestEndpoint =
  IngestEndpoint'
    {_ieURL = Nothing, _ieUsername = Nothing, _iePassword = Nothing}


-- | The ingest URL to which the source stream should be sent.
ieURL :: Lens' IngestEndpoint (Maybe Text)
ieURL = lens _ieURL (\ s a -> s{_ieURL = a})

-- | The system generated username for ingest authentication.
ieUsername :: Lens' IngestEndpoint (Maybe Text)
ieUsername = lens _ieUsername (\ s a -> s{_ieUsername = a})

-- | The system generated password for ingest authentication.
iePassword :: Lens' IngestEndpoint (Maybe Text)
iePassword = lens _iePassword (\ s a -> s{_iePassword = a})

instance FromJSON IngestEndpoint where
        parseJSON
          = withObject "IngestEndpoint"
              (\ x ->
                 IngestEndpoint' <$>
                   (x .:? "url") <*> (x .:? "username") <*>
                     (x .:? "password"))

instance Hashable IngestEndpoint where

instance NFData IngestEndpoint where

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'mssEncryption' smart constructor.
newtype MssEncryption = MssEncryption'
  { _meSpekeKeyProvider :: SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MssEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meSpekeKeyProvider' - Undocumented member.
mssEncryption
    :: SpekeKeyProvider -- ^ 'meSpekeKeyProvider'
    -> MssEncryption
mssEncryption pSpekeKeyProvider_ =
  MssEncryption' {_meSpekeKeyProvider = pSpekeKeyProvider_}


-- | Undocumented member.
meSpekeKeyProvider :: Lens' MssEncryption SpekeKeyProvider
meSpekeKeyProvider = lens _meSpekeKeyProvider (\ s a -> s{_meSpekeKeyProvider = a})

instance FromJSON MssEncryption where
        parseJSON
          = withObject "MssEncryption"
              (\ x -> MssEncryption' <$> (x .: "spekeKeyProvider"))

instance Hashable MssEncryption where

instance NFData MssEncryption where

instance ToJSON MssEncryption where
        toJSON MssEncryption'{..}
          = object
              (catMaybes
                 [Just ("spekeKeyProvider" .= _meSpekeKeyProvider)])

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'mssPackage' smart constructor.
data MssPackage = MssPackage'
  { _mpSegmentDurationSeconds :: !(Maybe Int)
  , _mpStreamSelection        :: !(Maybe StreamSelection)
  , _mpEncryption             :: !(Maybe MssEncryption)
  , _mpManifestWindowSeconds  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MssPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpSegmentDurationSeconds' - The duration (in seconds) of each segment.
--
-- * 'mpStreamSelection' - Undocumented member.
--
-- * 'mpEncryption' - Undocumented member.
--
-- * 'mpManifestWindowSeconds' - The time window (in seconds) contained in each manifest.
mssPackage
    :: MssPackage
mssPackage =
  MssPackage'
    { _mpSegmentDurationSeconds = Nothing
    , _mpStreamSelection = Nothing
    , _mpEncryption = Nothing
    , _mpManifestWindowSeconds = Nothing
    }


-- | The duration (in seconds) of each segment.
mpSegmentDurationSeconds :: Lens' MssPackage (Maybe Int)
mpSegmentDurationSeconds = lens _mpSegmentDurationSeconds (\ s a -> s{_mpSegmentDurationSeconds = a})

-- | Undocumented member.
mpStreamSelection :: Lens' MssPackage (Maybe StreamSelection)
mpStreamSelection = lens _mpStreamSelection (\ s a -> s{_mpStreamSelection = a})

-- | Undocumented member.
mpEncryption :: Lens' MssPackage (Maybe MssEncryption)
mpEncryption = lens _mpEncryption (\ s a -> s{_mpEncryption = a})

-- | The time window (in seconds) contained in each manifest.
mpManifestWindowSeconds :: Lens' MssPackage (Maybe Int)
mpManifestWindowSeconds = lens _mpManifestWindowSeconds (\ s a -> s{_mpManifestWindowSeconds = a})

instance FromJSON MssPackage where
        parseJSON
          = withObject "MssPackage"
              (\ x ->
                 MssPackage' <$>
                   (x .:? "segmentDurationSeconds") <*>
                     (x .:? "streamSelection")
                     <*> (x .:? "encryption")
                     <*> (x .:? "manifestWindowSeconds"))

instance Hashable MssPackage where

instance NFData MssPackage where

instance ToJSON MssPackage where
        toJSON MssPackage'{..}
          = object
              (catMaybes
                 [("segmentDurationSeconds" .=) <$>
                    _mpSegmentDurationSeconds,
                  ("streamSelection" .=) <$> _mpStreamSelection,
                  ("encryption" .=) <$> _mpEncryption,
                  ("manifestWindowSeconds" .=) <$>
                    _mpManifestWindowSeconds])

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'originEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
  { _oeWhitelist              :: !(Maybe [Text])
  , _oeHlsPackage             :: !(Maybe HlsPackage)
  , _oeARN                    :: !(Maybe Text)
  , _oeManifestName           :: !(Maybe Text)
  , _oeURL                    :: !(Maybe Text)
  , _oeChannelId              :: !(Maybe Text)
  , _oeStartoverWindowSeconds :: !(Maybe Int)
  , _oeDashPackage            :: !(Maybe DashPackage)
  , _oeMssPackage             :: !(Maybe MssPackage)
  , _oeId                     :: !(Maybe Text)
  , _oeTimeDelaySeconds       :: !(Maybe Int)
  , _oeCmafPackage            :: !(Maybe CmafPackage)
  , _oeDescription            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OriginEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeWhitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- * 'oeHlsPackage' - Undocumented member.
--
-- * 'oeARN' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- * 'oeManifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- * 'oeURL' - The URL of the packaged OriginEndpoint for consumption.
--
-- * 'oeChannelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- * 'oeStartoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- * 'oeDashPackage' - Undocumented member.
--
-- * 'oeMssPackage' - Undocumented member.
--
-- * 'oeId' - The ID of the OriginEndpoint.
--
-- * 'oeTimeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- * 'oeCmafPackage' - Undocumented member.
--
-- * 'oeDescription' - A short text description of the OriginEndpoint.
originEndpoint
    :: OriginEndpoint
originEndpoint =
  OriginEndpoint'
    { _oeWhitelist = Nothing
    , _oeHlsPackage = Nothing
    , _oeARN = Nothing
    , _oeManifestName = Nothing
    , _oeURL = Nothing
    , _oeChannelId = Nothing
    , _oeStartoverWindowSeconds = Nothing
    , _oeDashPackage = Nothing
    , _oeMssPackage = Nothing
    , _oeId = Nothing
    , _oeTimeDelaySeconds = Nothing
    , _oeCmafPackage = Nothing
    , _oeDescription = Nothing
    }


-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
oeWhitelist :: Lens' OriginEndpoint [Text]
oeWhitelist = lens _oeWhitelist (\ s a -> s{_oeWhitelist = a}) . _Default . _Coerce

-- | Undocumented member.
oeHlsPackage :: Lens' OriginEndpoint (Maybe HlsPackage)
oeHlsPackage = lens _oeHlsPackage (\ s a -> s{_oeHlsPackage = a})

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
oeARN :: Lens' OriginEndpoint (Maybe Text)
oeARN = lens _oeARN (\ s a -> s{_oeARN = a})

-- | A short string appended to the end of the OriginEndpoint URL.
oeManifestName :: Lens' OriginEndpoint (Maybe Text)
oeManifestName = lens _oeManifestName (\ s a -> s{_oeManifestName = a})

-- | The URL of the packaged OriginEndpoint for consumption.
oeURL :: Lens' OriginEndpoint (Maybe Text)
oeURL = lens _oeURL (\ s a -> s{_oeURL = a})

-- | The ID of the Channel the OriginEndpoint is associated with.
oeChannelId :: Lens' OriginEndpoint (Maybe Text)
oeChannelId = lens _oeChannelId (\ s a -> s{_oeChannelId = a})

-- | Maximum duration (seconds) of content to retain for startover playback. If not specified, startover playback will be disabled for the OriginEndpoint.
oeStartoverWindowSeconds :: Lens' OriginEndpoint (Maybe Int)
oeStartoverWindowSeconds = lens _oeStartoverWindowSeconds (\ s a -> s{_oeStartoverWindowSeconds = a})

-- | Undocumented member.
oeDashPackage :: Lens' OriginEndpoint (Maybe DashPackage)
oeDashPackage = lens _oeDashPackage (\ s a -> s{_oeDashPackage = a})

-- | Undocumented member.
oeMssPackage :: Lens' OriginEndpoint (Maybe MssPackage)
oeMssPackage = lens _oeMssPackage (\ s a -> s{_oeMssPackage = a})

-- | The ID of the OriginEndpoint.
oeId :: Lens' OriginEndpoint (Maybe Text)
oeId = lens _oeId (\ s a -> s{_oeId = a})

-- | Amount of delay (seconds) to enforce on the playback of live content. If not specified, there will be no time delay in effect for the OriginEndpoint.
oeTimeDelaySeconds :: Lens' OriginEndpoint (Maybe Int)
oeTimeDelaySeconds = lens _oeTimeDelaySeconds (\ s a -> s{_oeTimeDelaySeconds = a})

-- | Undocumented member.
oeCmafPackage :: Lens' OriginEndpoint (Maybe CmafPackage)
oeCmafPackage = lens _oeCmafPackage (\ s a -> s{_oeCmafPackage = a})

-- | A short text description of the OriginEndpoint.
oeDescription :: Lens' OriginEndpoint (Maybe Text)
oeDescription = lens _oeDescription (\ s a -> s{_oeDescription = a})

instance FromJSON OriginEndpoint where
        parseJSON
          = withObject "OriginEndpoint"
              (\ x ->
                 OriginEndpoint' <$>
                   (x .:? "whitelist" .!= mempty) <*>
                     (x .:? "hlsPackage")
                     <*> (x .:? "arn")
                     <*> (x .:? "manifestName")
                     <*> (x .:? "url")
                     <*> (x .:? "channelId")
                     <*> (x .:? "startoverWindowSeconds")
                     <*> (x .:? "dashPackage")
                     <*> (x .:? "mssPackage")
                     <*> (x .:? "id")
                     <*> (x .:? "timeDelaySeconds")
                     <*> (x .:? "cmafPackage")
                     <*> (x .:? "description"))

instance Hashable OriginEndpoint where

instance NFData OriginEndpoint where

-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'spekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { _skpURL        :: !Text
  , _skpResourceId :: !Text
  , _skpRoleARN    :: !Text
  , _skpSystemIds  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpURL' - The URL of the external key provider service.
--
-- * 'skpResourceId' - The resource ID to include in key requests.
--
-- * 'skpRoleARN' - An Amazon Resource Name (ARN) of an IAM role that AWS Elemental MediaPackage will assume when accessing the key provider service.
--
-- * 'skpSystemIds' - The system IDs to include in key requests.
spekeKeyProvider
    :: Text -- ^ 'skpURL'
    -> Text -- ^ 'skpResourceId'
    -> Text -- ^ 'skpRoleARN'
    -> SpekeKeyProvider
spekeKeyProvider pURL_ pResourceId_ pRoleARN_ =
  SpekeKeyProvider'
    { _skpURL = pURL_
    , _skpResourceId = pResourceId_
    , _skpRoleARN = pRoleARN_
    , _skpSystemIds = mempty
    }


-- | The URL of the external key provider service.
skpURL :: Lens' SpekeKeyProvider Text
skpURL = lens _skpURL (\ s a -> s{_skpURL = a})

-- | The resource ID to include in key requests.
skpResourceId :: Lens' SpekeKeyProvider Text
skpResourceId = lens _skpResourceId (\ s a -> s{_skpResourceId = a})

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental MediaPackage will assume when accessing the key provider service.
skpRoleARN :: Lens' SpekeKeyProvider Text
skpRoleARN = lens _skpRoleARN (\ s a -> s{_skpRoleARN = a})

-- | The system IDs to include in key requests.
skpSystemIds :: Lens' SpekeKeyProvider [Text]
skpSystemIds = lens _skpSystemIds (\ s a -> s{_skpSystemIds = a}) . _Coerce

instance FromJSON SpekeKeyProvider where
        parseJSON
          = withObject "SpekeKeyProvider"
              (\ x ->
                 SpekeKeyProvider' <$>
                   (x .: "url") <*> (x .: "resourceId") <*>
                     (x .: "roleArn")
                     <*> (x .:? "systemIds" .!= mempty))

instance Hashable SpekeKeyProvider where

instance NFData SpekeKeyProvider where

instance ToJSON SpekeKeyProvider where
        toJSON SpekeKeyProvider'{..}
          = object
              (catMaybes
                 [Just ("url" .= _skpURL),
                  Just ("resourceId" .= _skpResourceId),
                  Just ("roleArn" .= _skpRoleARN),
                  Just ("systemIds" .= _skpSystemIds)])

-- | A StreamSelection configuration.
--
-- /See:/ 'streamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { _ssStreamOrder           :: !(Maybe StreamOrder)
  , _ssMinVideoBitsPerSecond :: !(Maybe Int)
  , _ssMaxVideoBitsPerSecond :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamSelection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStreamOrder' - A directive that determines the order of streams in the output.
--
-- * 'ssMinVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
--
-- * 'ssMaxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
streamSelection
    :: StreamSelection
streamSelection =
  StreamSelection'
    { _ssStreamOrder = Nothing
    , _ssMinVideoBitsPerSecond = Nothing
    , _ssMaxVideoBitsPerSecond = Nothing
    }


-- | A directive that determines the order of streams in the output.
ssStreamOrder :: Lens' StreamSelection (Maybe StreamOrder)
ssStreamOrder = lens _ssStreamOrder (\ s a -> s{_ssStreamOrder = a})

-- | The minimum video bitrate (bps) to include in output.
ssMinVideoBitsPerSecond :: Lens' StreamSelection (Maybe Int)
ssMinVideoBitsPerSecond = lens _ssMinVideoBitsPerSecond (\ s a -> s{_ssMinVideoBitsPerSecond = a})

-- | The maximum video bitrate (bps) to include in output.
ssMaxVideoBitsPerSecond :: Lens' StreamSelection (Maybe Int)
ssMaxVideoBitsPerSecond = lens _ssMaxVideoBitsPerSecond (\ s a -> s{_ssMaxVideoBitsPerSecond = a})

instance FromJSON StreamSelection where
        parseJSON
          = withObject "StreamSelection"
              (\ x ->
                 StreamSelection' <$>
                   (x .:? "streamOrder") <*>
                     (x .:? "minVideoBitsPerSecond")
                     <*> (x .:? "maxVideoBitsPerSecond"))

instance Hashable StreamSelection where

instance NFData StreamSelection where

instance ToJSON StreamSelection where
        toJSON StreamSelection'{..}
          = object
              (catMaybes
                 [("streamOrder" .=) <$> _ssStreamOrder,
                  ("minVideoBitsPerSecond" .=) <$>
                    _ssMinVideoBitsPerSecond,
                  ("maxVideoBitsPerSecond" .=) <$>
                    _ssMaxVideoBitsPerSecond])
