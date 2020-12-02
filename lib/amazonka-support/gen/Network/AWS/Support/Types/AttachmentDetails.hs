{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.AttachmentDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.AttachmentDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The file name and ID of an attachment to a case communication. You can use the ID to retrieve the attachment with the 'DescribeAttachment' operation.
--
--
--
-- /See:/ 'attachmentDetails' smart constructor.
data AttachmentDetails = AttachmentDetails'
  { _adAttachmentId ::
      !(Maybe Text),
    _adFileName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachmentDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttachmentId' - The ID of the attachment.
--
-- * 'adFileName' - The file name of the attachment.
attachmentDetails ::
  AttachmentDetails
attachmentDetails =
  AttachmentDetails'
    { _adAttachmentId = Nothing,
      _adFileName = Nothing
    }

-- | The ID of the attachment.
adAttachmentId :: Lens' AttachmentDetails (Maybe Text)
adAttachmentId = lens _adAttachmentId (\s a -> s {_adAttachmentId = a})

-- | The file name of the attachment.
adFileName :: Lens' AttachmentDetails (Maybe Text)
adFileName = lens _adFileName (\s a -> s {_adFileName = a})

instance FromJSON AttachmentDetails where
  parseJSON =
    withObject
      "AttachmentDetails"
      ( \x ->
          AttachmentDetails'
            <$> (x .:? "attachmentId") <*> (x .:? "fileName")
      )

instance Hashable AttachmentDetails

instance NFData AttachmentDetails
