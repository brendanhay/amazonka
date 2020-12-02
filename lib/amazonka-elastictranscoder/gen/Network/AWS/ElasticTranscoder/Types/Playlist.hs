{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Playlist
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Playlist where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30.
--
--
--
-- /See:/ 'playlist' smart constructor.
data Playlist = Playlist'
  { _pStatus :: !(Maybe Text),
    _pPlayReadyDrm :: !(Maybe PlayReadyDrm),
    _pFormat :: !(Maybe Text),
    _pOutputKeys :: !(Maybe [Text]),
    _pName :: !(Maybe Text),
    _pStatusDetail :: !(Maybe Text),
    _pHlsContentProtection :: !(Maybe HlsContentProtection)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Playlist' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pStatus' - The status of the job with which the playlist is associated.
--
-- * 'pPlayReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- * 'pFormat' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- * 'pOutputKeys' - For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
--
-- * 'pName' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- * 'pStatusDetail' - Information that further explains the status.
--
-- * 'pHlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
playlist ::
  Playlist
playlist =
  Playlist'
    { _pStatus = Nothing,
      _pPlayReadyDrm = Nothing,
      _pFormat = Nothing,
      _pOutputKeys = Nothing,
      _pName = Nothing,
      _pStatusDetail = Nothing,
      _pHlsContentProtection = Nothing
    }

-- | The status of the job with which the playlist is associated.
pStatus :: Lens' Playlist (Maybe Text)
pStatus = lens _pStatus (\s a -> s {_pStatus = a})

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
pPlayReadyDrm :: Lens' Playlist (Maybe PlayReadyDrm)
pPlayReadyDrm = lens _pPlayReadyDrm (\s a -> s {_pPlayReadyDrm = a})

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
pFormat :: Lens' Playlist (Maybe Text)
pFormat = lens _pFormat (\s a -> s {_pFormat = a})

-- | For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
pOutputKeys :: Lens' Playlist [Text]
pOutputKeys = lens _pOutputKeys (\s a -> s {_pOutputKeys = a}) . _Default . _Coerce

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
pName :: Lens' Playlist (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

-- | Information that further explains the status.
pStatusDetail :: Lens' Playlist (Maybe Text)
pStatusDetail = lens _pStatusDetail (\s a -> s {_pStatusDetail = a})

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
pHlsContentProtection :: Lens' Playlist (Maybe HlsContentProtection)
pHlsContentProtection = lens _pHlsContentProtection (\s a -> s {_pHlsContentProtection = a})

instance FromJSON Playlist where
  parseJSON =
    withObject
      "Playlist"
      ( \x ->
          Playlist'
            <$> (x .:? "Status")
            <*> (x .:? "PlayReadyDrm")
            <*> (x .:? "Format")
            <*> (x .:? "OutputKeys" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "StatusDetail")
            <*> (x .:? "HlsContentProtection")
      )

instance Hashable Playlist

instance NFData Playlist
