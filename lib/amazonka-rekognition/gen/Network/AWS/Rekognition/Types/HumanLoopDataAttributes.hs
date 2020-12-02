{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopDataAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopDataAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.ContentClassifier

-- | Allows you to set attributes of the image. Currently, you can declare an image as free of personally identifiable information.
--
--
--
-- /See:/ 'humanLoopDataAttributes' smart constructor.
newtype HumanLoopDataAttributes = HumanLoopDataAttributes'
  { _hldaContentClassifiers ::
      Maybe [ContentClassifier]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopDataAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hldaContentClassifiers' - Sets whether the input image is free of personally identifiable information.
humanLoopDataAttributes ::
  HumanLoopDataAttributes
humanLoopDataAttributes =
  HumanLoopDataAttributes' {_hldaContentClassifiers = Nothing}

-- | Sets whether the input image is free of personally identifiable information.
hldaContentClassifiers :: Lens' HumanLoopDataAttributes [ContentClassifier]
hldaContentClassifiers = lens _hldaContentClassifiers (\s a -> s {_hldaContentClassifiers = a}) . _Default . _Coerce

instance Hashable HumanLoopDataAttributes

instance NFData HumanLoopDataAttributes

instance ToJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes' {..} =
    object
      (catMaybes [("ContentClassifiers" .=) <$> _hldaContentClassifiers])
