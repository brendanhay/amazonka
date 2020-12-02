{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvailBlanking where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for Avail Blanking
--
-- /See:/ 'availBlanking' smart constructor.
newtype AvailBlanking = AvailBlanking'
  { _abAvailBlankingImage ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAvailBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
availBlanking ::
  AvailBlanking
availBlanking = AvailBlanking' {_abAvailBlankingImage = Nothing}

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
abAvailBlankingImage :: Lens' AvailBlanking (Maybe Text)
abAvailBlankingImage = lens _abAvailBlankingImage (\s a -> s {_abAvailBlankingImage = a})

instance FromJSON AvailBlanking where
  parseJSON =
    withObject
      "AvailBlanking"
      (\x -> AvailBlanking' <$> (x .:? "availBlankingImage"))

instance Hashable AvailBlanking

instance NFData AvailBlanking

instance ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    object
      (catMaybes [("availBlankingImage" .=) <$> _abAvailBlankingImage])
