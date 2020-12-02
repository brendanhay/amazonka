{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Server

-- | Contains output from validating an instance.
--
--
--
-- /See:/ 'serverValidationOutput' smart constructor.
newtype ServerValidationOutput = ServerValidationOutput'
  { _svoServer ::
      Maybe Server
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerValidationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svoServer' - Undocumented member.
serverValidationOutput ::
  ServerValidationOutput
serverValidationOutput =
  ServerValidationOutput' {_svoServer = Nothing}

-- | Undocumented member.
svoServer :: Lens' ServerValidationOutput (Maybe Server)
svoServer = lens _svoServer (\s a -> s {_svoServer = a})

instance FromJSON ServerValidationOutput where
  parseJSON =
    withObject
      "ServerValidationOutput"
      (\x -> ServerValidationOutput' <$> (x .:? "server"))

instance Hashable ServerValidationOutput

instance NFData ServerValidationOutput
