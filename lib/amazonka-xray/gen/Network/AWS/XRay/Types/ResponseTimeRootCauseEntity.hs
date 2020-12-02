{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseEntity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of segments and corresponding subsegments associated to a response time warning.
--
--
--
-- /See:/ 'responseTimeRootCauseEntity' smart constructor.
data ResponseTimeRootCauseEntity = ResponseTimeRootCauseEntity'
  { _rtrceRemote ::
      !(Maybe Bool),
    _rtrceCoverage :: !(Maybe Double),
    _rtrceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResponseTimeRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrceRemote' - A flag that denotes a remote subsegment.
--
-- * 'rtrceCoverage' - The type and messages of the exceptions.
--
-- * 'rtrceName' - The name of the entity.
responseTimeRootCauseEntity ::
  ResponseTimeRootCauseEntity
responseTimeRootCauseEntity =
  ResponseTimeRootCauseEntity'
    { _rtrceRemote = Nothing,
      _rtrceCoverage = Nothing,
      _rtrceName = Nothing
    }

-- | A flag that denotes a remote subsegment.
rtrceRemote :: Lens' ResponseTimeRootCauseEntity (Maybe Bool)
rtrceRemote = lens _rtrceRemote (\s a -> s {_rtrceRemote = a})

-- | The type and messages of the exceptions.
rtrceCoverage :: Lens' ResponseTimeRootCauseEntity (Maybe Double)
rtrceCoverage = lens _rtrceCoverage (\s a -> s {_rtrceCoverage = a})

-- | The name of the entity.
rtrceName :: Lens' ResponseTimeRootCauseEntity (Maybe Text)
rtrceName = lens _rtrceName (\s a -> s {_rtrceName = a})

instance FromJSON ResponseTimeRootCauseEntity where
  parseJSON =
    withObject
      "ResponseTimeRootCauseEntity"
      ( \x ->
          ResponseTimeRootCauseEntity'
            <$> (x .:? "Remote") <*> (x .:? "Coverage") <*> (x .:? "Name")
      )

instance Hashable ResponseTimeRootCauseEntity

instance NFData ResponseTimeRootCauseEntity
