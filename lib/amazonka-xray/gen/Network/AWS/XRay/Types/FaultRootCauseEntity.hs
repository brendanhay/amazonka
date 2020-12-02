{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseEntity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a trace summary fault error.
--
--
--
-- /See:/ 'faultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { _frceExceptions ::
      !(Maybe [RootCauseException]),
    _frceRemote :: !(Maybe Bool),
    _frceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaultRootCauseEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frceExceptions' - The types and messages of the exceptions.
--
-- * 'frceRemote' - A flag that denotes a remote subsegment.
--
-- * 'frceName' - The name of the entity.
faultRootCauseEntity ::
  FaultRootCauseEntity
faultRootCauseEntity =
  FaultRootCauseEntity'
    { _frceExceptions = Nothing,
      _frceRemote = Nothing,
      _frceName = Nothing
    }

-- | The types and messages of the exceptions.
frceExceptions :: Lens' FaultRootCauseEntity [RootCauseException]
frceExceptions = lens _frceExceptions (\s a -> s {_frceExceptions = a}) . _Default . _Coerce

-- | A flag that denotes a remote subsegment.
frceRemote :: Lens' FaultRootCauseEntity (Maybe Bool)
frceRemote = lens _frceRemote (\s a -> s {_frceRemote = a})

-- | The name of the entity.
frceName :: Lens' FaultRootCauseEntity (Maybe Text)
frceName = lens _frceName (\s a -> s {_frceName = a})

instance FromJSON FaultRootCauseEntity where
  parseJSON =
    withObject
      "FaultRootCauseEntity"
      ( \x ->
          FaultRootCauseEntity'
            <$> (x .:? "Exceptions" .!= mempty)
            <*> (x .:? "Remote")
            <*> (x .:? "Name")
      )

instance Hashable FaultRootCauseEntity

instance NFData FaultRootCauseEntity
