{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlow where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mediaConnectFlow' smart constructor.
newtype MediaConnectFlow = MediaConnectFlow'
  { _mcfFlowARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaConnectFlow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcfFlowARN' - The unique ARN of the MediaConnect Flow being used as a source.
mediaConnectFlow ::
  MediaConnectFlow
mediaConnectFlow = MediaConnectFlow' {_mcfFlowARN = Nothing}

-- | The unique ARN of the MediaConnect Flow being used as a source.
mcfFlowARN :: Lens' MediaConnectFlow (Maybe Text)
mcfFlowARN = lens _mcfFlowARN (\s a -> s {_mcfFlowARN = a})

instance FromJSON MediaConnectFlow where
  parseJSON =
    withObject
      "MediaConnectFlow"
      (\x -> MediaConnectFlow' <$> (x .:? "flowArn"))

instance Hashable MediaConnectFlow

instance NFData MediaConnectFlow
