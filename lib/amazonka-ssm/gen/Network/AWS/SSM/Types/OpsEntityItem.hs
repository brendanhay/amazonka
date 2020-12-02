{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntityItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntityItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The OpsItem summaries result item.
--
--
--
-- /See:/ 'opsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { _oeiContent ::
      !(Maybe [Map Text (Text)]),
    _oeiCaptureTime :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsEntityItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oeiContent' - The detailed data content for an OpsItem summaries result item.
--
-- * 'oeiCaptureTime' - The time OpsItem data was captured.
opsEntityItem ::
  OpsEntityItem
opsEntityItem =
  OpsEntityItem' {_oeiContent = Nothing, _oeiCaptureTime = Nothing}

-- | The detailed data content for an OpsItem summaries result item.
oeiContent :: Lens' OpsEntityItem [HashMap Text (Text)]
oeiContent = lens _oeiContent (\s a -> s {_oeiContent = a}) . _Default . _Coerce

-- | The time OpsItem data was captured.
oeiCaptureTime :: Lens' OpsEntityItem (Maybe Text)
oeiCaptureTime = lens _oeiCaptureTime (\s a -> s {_oeiCaptureTime = a})

instance FromJSON OpsEntityItem where
  parseJSON =
    withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            <$> (x .:? "Content" .!= mempty) <*> (x .:? "CaptureTime")
      )

instance Hashable OpsEntityItem

instance NFData OpsEntityItem
