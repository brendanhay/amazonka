{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPAuthorization where

import Network.AWS.IoT.Types.SigV4Authorization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorization method used to send messages.
--
--
--
-- /See:/ 'hTTPAuthorization' smart constructor.
newtype HTTPAuthorization = HTTPAuthorization'
  { _httpaSigv4 ::
      Maybe SigV4Authorization
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpaSigv4' - Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
hTTPAuthorization ::
  HTTPAuthorization
hTTPAuthorization = HTTPAuthorization' {_httpaSigv4 = Nothing}

-- | Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
httpaSigv4 :: Lens' HTTPAuthorization (Maybe SigV4Authorization)
httpaSigv4 = lens _httpaSigv4 (\s a -> s {_httpaSigv4 = a})

instance FromJSON HTTPAuthorization where
  parseJSON =
    withObject
      "HTTPAuthorization"
      (\x -> HTTPAuthorization' <$> (x .:? "sigv4"))

instance Hashable HTTPAuthorization

instance NFData HTTPAuthorization

instance ToJSON HTTPAuthorization where
  toJSON HTTPAuthorization' {..} =
    object (catMaybes [("sigv4" .=) <$> _httpaSigv4])
