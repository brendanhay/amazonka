{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossBehavior where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputLocation
import Network.AWS.MediaLive.Types.InputLossImageType
import Network.AWS.Prelude

-- | Input Loss Behavior
--
-- /See:/ 'inputLossBehavior' smart constructor.
data InputLossBehavior = InputLossBehavior'
  { _ilbInputLossImageColor ::
      !(Maybe Text),
    _ilbBlackFrameMsec :: !(Maybe Nat),
    _ilbRepeatFrameMsec :: !(Maybe Nat),
    _ilbInputLossImageType :: !(Maybe InputLossImageType),
    _ilbInputLossImageSlate :: !(Maybe InputLocation)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLossBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilbInputLossImageColor' - When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
--
-- * 'ilbBlackFrameMsec' - Documentation update needed
--
-- * 'ilbRepeatFrameMsec' - Documentation update needed
--
-- * 'ilbInputLossImageType' - Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
--
-- * 'ilbInputLossImageSlate' - When input loss image type is "slate" these fields specify the parameters for accessing the slate.
inputLossBehavior ::
  InputLossBehavior
inputLossBehavior =
  InputLossBehavior'
    { _ilbInputLossImageColor = Nothing,
      _ilbBlackFrameMsec = Nothing,
      _ilbRepeatFrameMsec = Nothing,
      _ilbInputLossImageType = Nothing,
      _ilbInputLossImageSlate = Nothing
    }

-- | When input loss image type is "color" this field specifies the color to use. Value: 6 hex characters representing the values of RGB.
ilbInputLossImageColor :: Lens' InputLossBehavior (Maybe Text)
ilbInputLossImageColor = lens _ilbInputLossImageColor (\s a -> s {_ilbInputLossImageColor = a})

-- | Documentation update needed
ilbBlackFrameMsec :: Lens' InputLossBehavior (Maybe Natural)
ilbBlackFrameMsec = lens _ilbBlackFrameMsec (\s a -> s {_ilbBlackFrameMsec = a}) . mapping _Nat

-- | Documentation update needed
ilbRepeatFrameMsec :: Lens' InputLossBehavior (Maybe Natural)
ilbRepeatFrameMsec = lens _ilbRepeatFrameMsec (\s a -> s {_ilbRepeatFrameMsec = a}) . mapping _Nat

-- | Indicates whether to substitute a solid color or a slate into the output after input loss exceeds blackFrameMsec.
ilbInputLossImageType :: Lens' InputLossBehavior (Maybe InputLossImageType)
ilbInputLossImageType = lens _ilbInputLossImageType (\s a -> s {_ilbInputLossImageType = a})

-- | When input loss image type is "slate" these fields specify the parameters for accessing the slate.
ilbInputLossImageSlate :: Lens' InputLossBehavior (Maybe InputLocation)
ilbInputLossImageSlate = lens _ilbInputLossImageSlate (\s a -> s {_ilbInputLossImageSlate = a})

instance FromJSON InputLossBehavior where
  parseJSON =
    withObject
      "InputLossBehavior"
      ( \x ->
          InputLossBehavior'
            <$> (x .:? "inputLossImageColor")
            <*> (x .:? "blackFrameMsec")
            <*> (x .:? "repeatFrameMsec")
            <*> (x .:? "inputLossImageType")
            <*> (x .:? "inputLossImageSlate")
      )

instance Hashable InputLossBehavior

instance NFData InputLossBehavior

instance ToJSON InputLossBehavior where
  toJSON InputLossBehavior' {..} =
    object
      ( catMaybes
          [ ("inputLossImageColor" .=) <$> _ilbInputLossImageColor,
            ("blackFrameMsec" .=) <$> _ilbBlackFrameMsec,
            ("repeatFrameMsec" .=) <$> _ilbRepeatFrameMsec,
            ("inputLossImageType" .=) <$> _ilbInputLossImageType,
            ("inputLossImageSlate" .=) <$> _ilbInputLossImageSlate
          ]
      )
