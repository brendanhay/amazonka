{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplateInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Container for user interface template information.
--
--
--
-- /See:/ 'uiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { _utiURL :: !(Maybe Text),
    _utiContentSha256 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UiTemplateInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utiURL' - The URL for the user interface template.
--
-- * 'utiContentSha256' - The SHA-256 digest of the contents of the template.
uiTemplateInfo ::
  UiTemplateInfo
uiTemplateInfo =
  UiTemplateInfo' {_utiURL = Nothing, _utiContentSha256 = Nothing}

-- | The URL for the user interface template.
utiURL :: Lens' UiTemplateInfo (Maybe Text)
utiURL = lens _utiURL (\s a -> s {_utiURL = a})

-- | The SHA-256 digest of the contents of the template.
utiContentSha256 :: Lens' UiTemplateInfo (Maybe Text)
utiContentSha256 = lens _utiContentSha256 (\s a -> s {_utiContentSha256 = a})

instance FromJSON UiTemplateInfo where
  parseJSON =
    withObject
      "UiTemplateInfo"
      ( \x ->
          UiTemplateInfo' <$> (x .:? "Url") <*> (x .:? "ContentSha256")
      )

instance Hashable UiTemplateInfo

instance NFData UiTemplateInfo
