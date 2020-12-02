{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextSourceSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /See:/ 'teletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'
  { _tssPageNumber ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TeletextSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssPageNumber' - Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
teletextSourceSettings ::
  TeletextSourceSettings
teletextSourceSettings =
  TeletextSourceSettings' {_tssPageNumber = Nothing}

-- | Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
tssPageNumber :: Lens' TeletextSourceSettings (Maybe Text)
tssPageNumber = lens _tssPageNumber (\s a -> s {_tssPageNumber = a})

instance FromJSON TeletextSourceSettings where
  parseJSON =
    withObject
      "TeletextSourceSettings"
      (\x -> TeletextSourceSettings' <$> (x .:? "pageNumber"))

instance Hashable TeletextSourceSettings

instance NFData TeletextSourceSettings

instance ToJSON TeletextSourceSettings where
  toJSON TeletextSourceSettings' {..} =
    object (catMaybes [("pageNumber" .=) <$> _tssPageNumber])
