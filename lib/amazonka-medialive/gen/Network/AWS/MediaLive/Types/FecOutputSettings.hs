{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FecOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FecOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FecOutputIncludeFec
import Network.AWS.Prelude

-- | Fec Output Settings
--
-- /See:/ 'fecOutputSettings' smart constructor.
data FecOutputSettings = FecOutputSettings'
  { _fosRowLength ::
      !(Maybe Nat),
    _fosIncludeFec :: !(Maybe FecOutputIncludeFec),
    _fosColumnDepth :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FecOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fosRowLength' - Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
--
-- * 'fosIncludeFec' - Enables column only or column and row based FEC
--
-- * 'fosColumnDepth' - Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
fecOutputSettings ::
  FecOutputSettings
fecOutputSettings =
  FecOutputSettings'
    { _fosRowLength = Nothing,
      _fosIncludeFec = Nothing,
      _fosColumnDepth = Nothing
    }

-- | Parameter L from SMPTE 2022-1. The width of the FEC protection matrix.  Must be between 1 and 20, inclusive. If only Column FEC is used, then larger values increase robustness.  If Row FEC is used, then this is the number of transport stream packets per row error correction packet, and the value must be between 4 and 20, inclusive, if includeFec is columnAndRow. If includeFec is column, this value must be 1 to 20, inclusive.
fosRowLength :: Lens' FecOutputSettings (Maybe Natural)
fosRowLength = lens _fosRowLength (\s a -> s {_fosRowLength = a}) . mapping _Nat

-- | Enables column only or column and row based FEC
fosIncludeFec :: Lens' FecOutputSettings (Maybe FecOutputIncludeFec)
fosIncludeFec = lens _fosIncludeFec (\s a -> s {_fosIncludeFec = a})

-- | Parameter D from SMPTE 2022-1. The height of the FEC protection matrix.  The number of transport stream packets per column error correction packet. Must be between 4 and 20, inclusive.
fosColumnDepth :: Lens' FecOutputSettings (Maybe Natural)
fosColumnDepth = lens _fosColumnDepth (\s a -> s {_fosColumnDepth = a}) . mapping _Nat

instance FromJSON FecOutputSettings where
  parseJSON =
    withObject
      "FecOutputSettings"
      ( \x ->
          FecOutputSettings'
            <$> (x .:? "rowLength")
            <*> (x .:? "includeFec")
            <*> (x .:? "columnDepth")
      )

instance Hashable FecOutputSettings

instance NFData FecOutputSettings

instance ToJSON FecOutputSettings where
  toJSON FecOutputSettings' {..} =
    object
      ( catMaybes
          [ ("rowLength" .=) <$> _fosRowLength,
            ("includeFec" .=) <$> _fosIncludeFec,
            ("columnDepth" .=) <$> _fosColumnDepth
          ]
      )
