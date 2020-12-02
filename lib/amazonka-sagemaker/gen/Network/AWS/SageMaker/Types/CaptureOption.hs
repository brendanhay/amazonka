{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureOption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CaptureMode

-- |
--
--
--
-- /See:/ 'captureOption' smart constructor.
newtype CaptureOption = CaptureOption'
  { _coCaptureMode ::
      CaptureMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CaptureOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCaptureMode' -
captureOption ::
  -- | 'coCaptureMode'
  CaptureMode ->
  CaptureOption
captureOption pCaptureMode_ =
  CaptureOption' {_coCaptureMode = pCaptureMode_}

-- |
coCaptureMode :: Lens' CaptureOption CaptureMode
coCaptureMode = lens _coCaptureMode (\s a -> s {_coCaptureMode = a})

instance FromJSON CaptureOption where
  parseJSON =
    withObject
      "CaptureOption"
      (\x -> CaptureOption' <$> (x .: "CaptureMode"))

instance Hashable CaptureOption

instance NFData CaptureOption

instance ToJSON CaptureOption where
  toJSON CaptureOption' {..} =
    object (catMaybes [Just ("CaptureMode" .= _coCaptureMode)])
