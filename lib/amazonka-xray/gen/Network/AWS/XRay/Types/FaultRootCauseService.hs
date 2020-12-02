{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultRootCauseService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.FaultRootCauseEntity

-- | A collection of fields identifying the services in a trace summary fault.
--
--
--
-- /See:/ 'faultRootCauseService' smart constructor.
data FaultRootCauseService = FaultRootCauseService'
  { _frcsEntityPath ::
      !(Maybe [FaultRootCauseEntity]),
    _frcsAccountId :: !(Maybe Text),
    _frcsNames :: !(Maybe [Text]),
    _frcsName :: !(Maybe Text),
    _frcsInferred :: !(Maybe Bool),
    _frcsType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FaultRootCauseService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frcsEntityPath' - The path of root cause entities found on the service.
--
-- * 'frcsAccountId' - The account ID associated to the service.
--
-- * 'frcsNames' - A collection of associated service names.
--
-- * 'frcsName' - The service name.
--
-- * 'frcsInferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- * 'frcsType' - The type associated to the service.
faultRootCauseService ::
  FaultRootCauseService
faultRootCauseService =
  FaultRootCauseService'
    { _frcsEntityPath = Nothing,
      _frcsAccountId = Nothing,
      _frcsNames = Nothing,
      _frcsName = Nothing,
      _frcsInferred = Nothing,
      _frcsType = Nothing
    }

-- | The path of root cause entities found on the service.
frcsEntityPath :: Lens' FaultRootCauseService [FaultRootCauseEntity]
frcsEntityPath = lens _frcsEntityPath (\s a -> s {_frcsEntityPath = a}) . _Default . _Coerce

-- | The account ID associated to the service.
frcsAccountId :: Lens' FaultRootCauseService (Maybe Text)
frcsAccountId = lens _frcsAccountId (\s a -> s {_frcsAccountId = a})

-- | A collection of associated service names.
frcsNames :: Lens' FaultRootCauseService [Text]
frcsNames = lens _frcsNames (\s a -> s {_frcsNames = a}) . _Default . _Coerce

-- | The service name.
frcsName :: Lens' FaultRootCauseService (Maybe Text)
frcsName = lens _frcsName (\s a -> s {_frcsName = a})

-- | A Boolean value indicating if the service is inferred from the trace.
frcsInferred :: Lens' FaultRootCauseService (Maybe Bool)
frcsInferred = lens _frcsInferred (\s a -> s {_frcsInferred = a})

-- | The type associated to the service.
frcsType :: Lens' FaultRootCauseService (Maybe Text)
frcsType = lens _frcsType (\s a -> s {_frcsType = a})

instance FromJSON FaultRootCauseService where
  parseJSON =
    withObject
      "FaultRootCauseService"
      ( \x ->
          FaultRootCauseService'
            <$> (x .:? "EntityPath" .!= mempty)
            <*> (x .:? "AccountId")
            <*> (x .:? "Names" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Inferred")
            <*> (x .:? "Type")
      )

instance Hashable FaultRootCauseService

instance NFData FaultRootCauseService
