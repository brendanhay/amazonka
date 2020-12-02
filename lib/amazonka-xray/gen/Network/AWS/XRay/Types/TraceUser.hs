{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TraceUser where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.ServiceId

-- | Information about a user recorded in segment documents.
--
--
--
-- /See:/ 'traceUser' smart constructor.
data TraceUser = TraceUser'
  { _tuServiceIds :: !(Maybe [ServiceId]),
    _tuUserName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TraceUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuServiceIds' - Services that the user's request hit.
--
-- * 'tuUserName' - The user's name.
traceUser ::
  TraceUser
traceUser =
  TraceUser' {_tuServiceIds = Nothing, _tuUserName = Nothing}

-- | Services that the user's request hit.
tuServiceIds :: Lens' TraceUser [ServiceId]
tuServiceIds = lens _tuServiceIds (\s a -> s {_tuServiceIds = a}) . _Default . _Coerce

-- | The user's name.
tuUserName :: Lens' TraceUser (Maybe Text)
tuUserName = lens _tuUserName (\s a -> s {_tuUserName = a})

instance FromJSON TraceUser where
  parseJSON =
    withObject
      "TraceUser"
      ( \x ->
          TraceUser'
            <$> (x .:? "ServiceIds" .!= mempty) <*> (x .:? "UserName")
      )

instance Hashable TraceUser

instance NFData TraceUser
