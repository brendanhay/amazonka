{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderStateChangeReason where

import Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the reason why the last image builder state change occurred.
--
--
--
-- /See:/ 'imageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { _ibscrCode ::
      !( Maybe
           ImageBuilderStateChangeReasonCode
       ),
    _ibscrMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageBuilderStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibscrCode' - The state change reason code.
--
-- * 'ibscrMessage' - The state change reason message.
imageBuilderStateChangeReason ::
  ImageBuilderStateChangeReason
imageBuilderStateChangeReason =
  ImageBuilderStateChangeReason'
    { _ibscrCode = Nothing,
      _ibscrMessage = Nothing
    }

-- | The state change reason code.
ibscrCode :: Lens' ImageBuilderStateChangeReason (Maybe ImageBuilderStateChangeReasonCode)
ibscrCode = lens _ibscrCode (\s a -> s {_ibscrCode = a})

-- | The state change reason message.
ibscrMessage :: Lens' ImageBuilderStateChangeReason (Maybe Text)
ibscrMessage = lens _ibscrMessage (\s a -> s {_ibscrMessage = a})

instance FromJSON ImageBuilderStateChangeReason where
  parseJSON =
    withObject
      "ImageBuilderStateChangeReason"
      ( \x ->
          ImageBuilderStateChangeReason'
            <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable ImageBuilderStateChangeReason

instance NFData ImageBuilderStateChangeReason
