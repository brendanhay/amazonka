{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentInformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An attribute of an attachment, such as the attachment name.
--
--
--
-- /See:/ 'attachmentInformation' smart constructor.
newtype AttachmentInformation = AttachmentInformation'
  { _aiName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachmentInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiName' - The name of the attachment.
attachmentInformation ::
  AttachmentInformation
attachmentInformation = AttachmentInformation' {_aiName = Nothing}

-- | The name of the attachment.
aiName :: Lens' AttachmentInformation (Maybe Text)
aiName = lens _aiName (\s a -> s {_aiName = a})

instance FromJSON AttachmentInformation where
  parseJSON =
    withObject
      "AttachmentInformation"
      (\x -> AttachmentInformation' <$> (x .:? "Name"))

instance Hashable AttachmentInformation

instance NFData AttachmentInformation
