{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaConnectFlowRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaConnectFlowRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The settings for a MediaConnect Flow.
--
-- /See:/ 'mediaConnectFlowRequest' smart constructor.
newtype MediaConnectFlowRequest = MediaConnectFlowRequest'
  { _mcfrFlowARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MediaConnectFlowRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcfrFlowARN' - The ARN of the MediaConnect Flow that you want to use as a source.
mediaConnectFlowRequest ::
  MediaConnectFlowRequest
mediaConnectFlowRequest =
  MediaConnectFlowRequest' {_mcfrFlowARN = Nothing}

-- | The ARN of the MediaConnect Flow that you want to use as a source.
mcfrFlowARN :: Lens' MediaConnectFlowRequest (Maybe Text)
mcfrFlowARN = lens _mcfrFlowARN (\s a -> s {_mcfrFlowARN = a})

instance Hashable MediaConnectFlowRequest

instance NFData MediaConnectFlowRequest

instance ToJSON MediaConnectFlowRequest where
  toJSON MediaConnectFlowRequest' {..} =
    object (catMaybes [("flowArn" .=) <$> _mcfrFlowARN])
