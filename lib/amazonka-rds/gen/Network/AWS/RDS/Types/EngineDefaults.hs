{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EngineDefaults where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Parameter

-- | Contains the result of a successful invocation of the @DescribeEngineDefaultParameters@ action.
--
--
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { _edDBParameterGroupFamily ::
      !(Maybe Text),
    _edMarker :: !(Maybe Text),
    _edParameters :: !(Maybe [Parameter])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDBParameterGroupFamily' - Specifies the name of the DB parameter group family that the engine default parameters apply to.
--
-- * 'edMarker' - An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'edParameters' - Contains a list of engine default parameters.
engineDefaults ::
  EngineDefaults
engineDefaults =
  EngineDefaults'
    { _edDBParameterGroupFamily = Nothing,
      _edMarker = Nothing,
      _edParameters = Nothing
    }

-- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\s a -> s {_edDBParameterGroupFamily = a})

-- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s {_edMarker = a})

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\s a -> s {_edParameters = a}) . _Default . _Coerce

instance FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      <$> (x .@? "DBParameterGroupFamily")
      <*> (x .@? "Marker")
      <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "Parameter"))

instance Hashable EngineDefaults

instance NFData EngineDefaults
