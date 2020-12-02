{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CreateJobPlaylist where

import Network.AWS.ElasticTranscoder.Types.HlsContentProtection
import Network.AWS.ElasticTranscoder.Types.PlayReadyDrm
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the master playlist.
--
--
--
-- /See:/ 'createJobPlaylist' smart constructor.
data CreateJobPlaylist = CreateJobPlaylist'
  { _cjpPlayReadyDrm ::
      !(Maybe PlayReadyDrm),
    _cjpFormat :: !(Maybe Text),
    _cjpOutputKeys :: !(Maybe [Text]),
    _cjpName :: !(Maybe Text),
    _cjpHlsContentProtection ::
      !(Maybe HlsContentProtection)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJobPlaylist' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjpPlayReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- * 'cjpFormat' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- * 'cjpOutputKeys' - For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.      * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
--
-- * 'cjpName' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- * 'cjpHlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
createJobPlaylist ::
  CreateJobPlaylist
createJobPlaylist =
  CreateJobPlaylist'
    { _cjpPlayReadyDrm = Nothing,
      _cjpFormat = Nothing,
      _cjpOutputKeys = Nothing,
      _cjpName = Nothing,
      _cjpHlsContentProtection = Nothing
    }

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
cjpPlayReadyDrm :: Lens' CreateJobPlaylist (Maybe PlayReadyDrm)
cjpPlayReadyDrm = lens _cjpPlayReadyDrm (\s a -> s {_cjpPlayReadyDrm = a})

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
cjpFormat :: Lens' CreateJobPlaylist (Maybe Text)
cjpFormat = lens _cjpFormat (\s a -> s {_cjpFormat = a})

-- | For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.      * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
cjpOutputKeys :: Lens' CreateJobPlaylist [Text]
cjpOutputKeys = lens _cjpOutputKeys (\s a -> s {_cjpOutputKeys = a}) . _Default . _Coerce

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
cjpName :: Lens' CreateJobPlaylist (Maybe Text)
cjpName = lens _cjpName (\s a -> s {_cjpName = a})

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
cjpHlsContentProtection :: Lens' CreateJobPlaylist (Maybe HlsContentProtection)
cjpHlsContentProtection = lens _cjpHlsContentProtection (\s a -> s {_cjpHlsContentProtection = a})

instance Hashable CreateJobPlaylist

instance NFData CreateJobPlaylist

instance ToJSON CreateJobPlaylist where
  toJSON CreateJobPlaylist' {..} =
    object
      ( catMaybes
          [ ("PlayReadyDrm" .=) <$> _cjpPlayReadyDrm,
            ("Format" .=) <$> _cjpFormat,
            ("OutputKeys" .=) <$> _cjpOutputKeys,
            ("Name" .=) <$> _cjpName,
            ("HlsContentProtection" .=) <$> _cjpHlsContentProtection
          ]
      )
