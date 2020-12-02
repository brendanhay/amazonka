{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSEDescription where

import Network.AWS.DAX.Types.SSEStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of the server-side encryption status on the specified DAX cluster.
--
--
--
-- /See:/ 'sSEDescription' smart constructor.
newtype SSEDescription = SSEDescription'
  { _ssedStatus ::
      Maybe SSEStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssedStatus' - The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.
sSEDescription ::
  SSEDescription
sSEDescription = SSEDescription' {_ssedStatus = Nothing}

-- | The current state of server-side encryption:     * @ENABLING@ - Server-side encryption is being enabled.     * @ENABLED@ - Server-side encryption is enabled.     * @DISABLING@ - Server-side encryption is being disabled.     * @DISABLED@ - Server-side encryption is disabled.
ssedStatus :: Lens' SSEDescription (Maybe SSEStatus)
ssedStatus = lens _ssedStatus (\s a -> s {_ssedStatus = a})

instance FromJSON SSEDescription where
  parseJSON =
    withObject
      "SSEDescription"
      (\x -> SSEDescription' <$> (x .:? "Status"))

instance Hashable SSEDescription

instance NFData SSEDescription
