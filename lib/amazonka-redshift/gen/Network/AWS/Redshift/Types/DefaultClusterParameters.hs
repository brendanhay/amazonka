{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DefaultClusterParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Parameter

-- | Describes the default cluster parameters for a parameter group family.
--
--
--
-- /See:/ 'defaultClusterParameters' smart constructor.
data DefaultClusterParameters = DefaultClusterParameters'
  { _dcpMarker ::
      !(Maybe Text),
    _dcpParameters :: !(Maybe [Parameter]),
    _dcpParameterGroupFamily :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DefaultClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcpParameters' - The list of cluster default parameters.
--
-- * 'dcpParameterGroupFamily' - The name of the cluster parameter group family to which the engine default parameters apply.
defaultClusterParameters ::
  DefaultClusterParameters
defaultClusterParameters =
  DefaultClusterParameters'
    { _dcpMarker = Nothing,
      _dcpParameters = Nothing,
      _dcpParameterGroupFamily = Nothing
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcpMarker :: Lens' DefaultClusterParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s {_dcpMarker = a})

-- | The list of cluster default parameters.
dcpParameters :: Lens' DefaultClusterParameters [Parameter]
dcpParameters = lens _dcpParameters (\s a -> s {_dcpParameters = a}) . _Default . _Coerce

-- | The name of the cluster parameter group family to which the engine default parameters apply.
dcpParameterGroupFamily :: Lens' DefaultClusterParameters (Maybe Text)
dcpParameterGroupFamily = lens _dcpParameterGroupFamily (\s a -> s {_dcpParameterGroupFamily = a})

instance FromXML DefaultClusterParameters where
  parseXML x =
    DefaultClusterParameters'
      <$> (x .@? "Marker")
      <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "Parameter"))
      <*> (x .@? "ParameterGroupFamily")

instance Hashable DefaultClusterParameters

instance NFData DefaultClusterParameters
