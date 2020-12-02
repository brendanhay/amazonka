{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AttachmentStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AttachmentStateChange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a change in state for a task attachment.
--
--
--
-- /See:/ 'attachmentStateChange' smart constructor.
data AttachmentStateChange = AttachmentStateChange'
  { _ascAttachmentARN ::
      !Text,
    _ascStatus :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachmentStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascAttachmentARN' - The Amazon Resource Name (ARN) of the attachment.
--
-- * 'ascStatus' - The status of the attachment.
attachmentStateChange ::
  -- | 'ascAttachmentARN'
  Text ->
  -- | 'ascStatus'
  Text ->
  AttachmentStateChange
attachmentStateChange pAttachmentARN_ pStatus_ =
  AttachmentStateChange'
    { _ascAttachmentARN = pAttachmentARN_,
      _ascStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the attachment.
ascAttachmentARN :: Lens' AttachmentStateChange Text
ascAttachmentARN = lens _ascAttachmentARN (\s a -> s {_ascAttachmentARN = a})

-- | The status of the attachment.
ascStatus :: Lens' AttachmentStateChange Text
ascStatus = lens _ascStatus (\s a -> s {_ascStatus = a})

instance Hashable AttachmentStateChange

instance NFData AttachmentStateChange

instance ToJSON AttachmentStateChange where
  toJSON AttachmentStateChange' {..} =
    object
      ( catMaybes
          [ Just ("attachmentArn" .= _ascAttachmentARN),
            Just ("status" .= _ascStatus)
          ]
      )
