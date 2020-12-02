{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TLSContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TLSContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the TLS context to use for the test authorizer request.
--
--
--
-- /See:/ 'tlsContext' smart constructor.
newtype TLSContext = TLSContext' {_tcServerName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TLSContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcServerName' - The value of the @serverName@ key in a TLS authorization request.
tlsContext ::
  TLSContext
tlsContext = TLSContext' {_tcServerName = Nothing}

-- | The value of the @serverName@ key in a TLS authorization request.
tcServerName :: Lens' TLSContext (Maybe Text)
tcServerName = lens _tcServerName (\s a -> s {_tcServerName = a})

instance Hashable TLSContext

instance NFData TLSContext

instance ToJSON TLSContext where
  toJSON TLSContext' {..} =
    object (catMaybes [("serverName" .=) <$> _tcServerName])
