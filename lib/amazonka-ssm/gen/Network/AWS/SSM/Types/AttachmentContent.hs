{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentContent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AttachmentHashType

-- | A structure that includes attributes that describe a document attachment.
--
--
--
-- /See:/ 'attachmentContent' smart constructor.
data AttachmentContent = AttachmentContent'
  { _acHash ::
      !(Maybe Text),
    _acSize :: !(Maybe Integer),
    _acURL :: !(Maybe Text),
    _acName :: !(Maybe Text),
    _acHashType :: !(Maybe AttachmentHashType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachmentContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acHash' - The cryptographic hash value of the document content.
--
-- * 'acSize' - The size of an attachment in bytes.
--
-- * 'acURL' - The URL location of the attachment content.
--
-- * 'acName' - The name of an attachment.
--
-- * 'acHashType' - The hash algorithm used to calculate the hash value.
attachmentContent ::
  AttachmentContent
attachmentContent =
  AttachmentContent'
    { _acHash = Nothing,
      _acSize = Nothing,
      _acURL = Nothing,
      _acName = Nothing,
      _acHashType = Nothing
    }

-- | The cryptographic hash value of the document content.
acHash :: Lens' AttachmentContent (Maybe Text)
acHash = lens _acHash (\s a -> s {_acHash = a})

-- | The size of an attachment in bytes.
acSize :: Lens' AttachmentContent (Maybe Integer)
acSize = lens _acSize (\s a -> s {_acSize = a})

-- | The URL location of the attachment content.
acURL :: Lens' AttachmentContent (Maybe Text)
acURL = lens _acURL (\s a -> s {_acURL = a})

-- | The name of an attachment.
acName :: Lens' AttachmentContent (Maybe Text)
acName = lens _acName (\s a -> s {_acName = a})

-- | The hash algorithm used to calculate the hash value.
acHashType :: Lens' AttachmentContent (Maybe AttachmentHashType)
acHashType = lens _acHashType (\s a -> s {_acHashType = a})

instance FromJSON AttachmentContent where
  parseJSON =
    withObject
      "AttachmentContent"
      ( \x ->
          AttachmentContent'
            <$> (x .:? "Hash")
            <*> (x .:? "Size")
            <*> (x .:? "Url")
            <*> (x .:? "Name")
            <*> (x .:? "HashType")
      )

instance Hashable AttachmentContent

instance NFData AttachmentContent
