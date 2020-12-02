{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.GenericAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.GenericAttachment where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types.Button
import Network.AWS.Prelude

-- | Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text.
--
--
--
-- /See:/ 'genericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { _gaButtons ::
      !(Maybe [Button]),
    _gaSubTitle :: !(Maybe Text),
    _gaImageURL :: !(Maybe Text),
    _gaAttachmentLinkURL :: !(Maybe Text),
    _gaTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenericAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaButtons' - The list of options to show to the user.
--
-- * 'gaSubTitle' - The subtitle shown below the title.
--
-- * 'gaImageURL' - The URL of an image that is displayed to the user.
--
-- * 'gaAttachmentLinkURL' - The URL of an attachment to the response card.
--
-- * 'gaTitle' - The title of the option.
genericAttachment ::
  GenericAttachment
genericAttachment =
  GenericAttachment'
    { _gaButtons = Nothing,
      _gaSubTitle = Nothing,
      _gaImageURL = Nothing,
      _gaAttachmentLinkURL = Nothing,
      _gaTitle = Nothing
    }

-- | The list of options to show to the user.
gaButtons :: Lens' GenericAttachment [Button]
gaButtons = lens _gaButtons (\s a -> s {_gaButtons = a}) . _Default . _Coerce

-- | The subtitle shown below the title.
gaSubTitle :: Lens' GenericAttachment (Maybe Text)
gaSubTitle = lens _gaSubTitle (\s a -> s {_gaSubTitle = a})

-- | The URL of an image that is displayed to the user.
gaImageURL :: Lens' GenericAttachment (Maybe Text)
gaImageURL = lens _gaImageURL (\s a -> s {_gaImageURL = a})

-- | The URL of an attachment to the response card.
gaAttachmentLinkURL :: Lens' GenericAttachment (Maybe Text)
gaAttachmentLinkURL = lens _gaAttachmentLinkURL (\s a -> s {_gaAttachmentLinkURL = a})

-- | The title of the option.
gaTitle :: Lens' GenericAttachment (Maybe Text)
gaTitle = lens _gaTitle (\s a -> s {_gaTitle = a})

instance FromJSON GenericAttachment where
  parseJSON =
    withObject
      "GenericAttachment"
      ( \x ->
          GenericAttachment'
            <$> (x .:? "buttons" .!= mempty)
            <*> (x .:? "subTitle")
            <*> (x .:? "imageUrl")
            <*> (x .:? "attachmentLinkUrl")
            <*> (x .:? "title")
      )

instance Hashable GenericAttachment

instance NFData GenericAttachment
