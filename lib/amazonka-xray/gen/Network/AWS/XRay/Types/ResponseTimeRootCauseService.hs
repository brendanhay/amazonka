{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCauseService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCauseService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.XRay.Types.ResponseTimeRootCauseEntity

-- | A collection of fields identifying the service in a response time warning.
--
--
--
-- /See:/ 'responseTimeRootCauseService' smart constructor.
data ResponseTimeRootCauseService = ResponseTimeRootCauseService'
  { _rtrcsEntityPath ::
      !( Maybe
           [ResponseTimeRootCauseEntity]
       ),
    _rtrcsAccountId :: !(Maybe Text),
    _rtrcsNames :: !(Maybe [Text]),
    _rtrcsName :: !(Maybe Text),
    _rtrcsInferred :: !(Maybe Bool),
    _rtrcsType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResponseTimeRootCauseService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrcsEntityPath' - The path of root cause entities found on the service.
--
-- * 'rtrcsAccountId' - The account ID associated to the service.
--
-- * 'rtrcsNames' - A collection of associated service names.
--
-- * 'rtrcsName' - The service name.
--
-- * 'rtrcsInferred' - A Boolean value indicating if the service is inferred from the trace.
--
-- * 'rtrcsType' - The type associated to the service.
responseTimeRootCauseService ::
  ResponseTimeRootCauseService
responseTimeRootCauseService =
  ResponseTimeRootCauseService'
    { _rtrcsEntityPath = Nothing,
      _rtrcsAccountId = Nothing,
      _rtrcsNames = Nothing,
      _rtrcsName = Nothing,
      _rtrcsInferred = Nothing,
      _rtrcsType = Nothing
    }

-- | The path of root cause entities found on the service.
rtrcsEntityPath :: Lens' ResponseTimeRootCauseService [ResponseTimeRootCauseEntity]
rtrcsEntityPath = lens _rtrcsEntityPath (\s a -> s {_rtrcsEntityPath = a}) . _Default . _Coerce

-- | The account ID associated to the service.
rtrcsAccountId :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsAccountId = lens _rtrcsAccountId (\s a -> s {_rtrcsAccountId = a})

-- | A collection of associated service names.
rtrcsNames :: Lens' ResponseTimeRootCauseService [Text]
rtrcsNames = lens _rtrcsNames (\s a -> s {_rtrcsNames = a}) . _Default . _Coerce

-- | The service name.
rtrcsName :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsName = lens _rtrcsName (\s a -> s {_rtrcsName = a})

-- | A Boolean value indicating if the service is inferred from the trace.
rtrcsInferred :: Lens' ResponseTimeRootCauseService (Maybe Bool)
rtrcsInferred = lens _rtrcsInferred (\s a -> s {_rtrcsInferred = a})

-- | The type associated to the service.
rtrcsType :: Lens' ResponseTimeRootCauseService (Maybe Text)
rtrcsType = lens _rtrcsType (\s a -> s {_rtrcsType = a})

instance FromJSON ResponseTimeRootCauseService where
  parseJSON =
    withObject
      "ResponseTimeRootCauseService"
      ( \x ->
          ResponseTimeRootCauseService'
            <$> (x .:? "EntityPath" .!= mempty)
            <*> (x .:? "AccountId")
            <*> (x .:? "Names" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Inferred")
            <*> (x .:? "Type")
      )

instance Hashable ResponseTimeRootCauseService

instance NFData ResponseTimeRootCauseService
