{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCauseEntity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a trace summary error.
--
--
--
-- /See:/ 'errorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { _erceExceptions ::
      !(Maybe [RootCauseException]),
    _erceRemote :: !(Maybe Bool),
    _erceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ErrorRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erceExceptions' - The types and messages of the exceptions.
--
-- * 'erceRemote' - A flag that denotes a remote subsegment.
--
-- * 'erceName' - The name of the entity.
errorRootCauseEntity ::
  ErrorRootCauseEntity
errorRootCauseEntity =
  ErrorRootCauseEntity'
    { _erceExceptions = Nothing,
      _erceRemote = Nothing,
      _erceName = Nothing
    }

-- | The types and messages of the exceptions.
erceExceptions :: Lens' ErrorRootCauseEntity [RootCauseException]
erceExceptions = lens _erceExceptions (\s a -> s {_erceExceptions = a}) . _Default . _Coerce

-- | A flag that denotes a remote subsegment.
erceRemote :: Lens' ErrorRootCauseEntity (Maybe Bool)
erceRemote = lens _erceRemote (\s a -> s {_erceRemote = a})

-- | The name of the entity.
erceName :: Lens' ErrorRootCauseEntity (Maybe Text)
erceName = lens _erceName (\s a -> s {_erceName = a})

instance FromJSON ErrorRootCauseEntity where
  parseJSON =
    withObject
      "ErrorRootCauseEntity"
      ( \x ->
          ErrorRootCauseEntity'
            <$> (x .:? "Exceptions" .!= mempty)
            <*> (x .:? "Remote")
            <*> (x .:? "Name")
      )

instance Hashable ErrorRootCauseEntity

instance NFData ErrorRootCauseEntity
