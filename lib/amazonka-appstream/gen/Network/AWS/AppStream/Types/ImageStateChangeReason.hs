{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReason where

import Network.AWS.AppStream.Types.ImageStateChangeReasonCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the reason why the last image state change occurred.
--
--
--
-- /See:/ 'imageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { _iscrCode ::
      !(Maybe ImageStateChangeReasonCode),
    _iscrMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscrCode' - The state change reason code.
--
-- * 'iscrMessage' - The state change reason message.
imageStateChangeReason ::
  ImageStateChangeReason
imageStateChangeReason =
  ImageStateChangeReason'
    { _iscrCode = Nothing,
      _iscrMessage = Nothing
    }

-- | The state change reason code.
iscrCode :: Lens' ImageStateChangeReason (Maybe ImageStateChangeReasonCode)
iscrCode = lens _iscrCode (\s a -> s {_iscrCode = a})

-- | The state change reason message.
iscrMessage :: Lens' ImageStateChangeReason (Maybe Text)
iscrMessage = lens _iscrMessage (\s a -> s {_iscrMessage = a})

instance FromJSON ImageStateChangeReason where
  parseJSON =
    withObject
      "ImageStateChangeReason"
      ( \x ->
          ImageStateChangeReason' <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable ImageStateChangeReason

instance NFData ImageStateChangeReason
