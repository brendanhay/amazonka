{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of a parameter group.
--
--
--
-- /See:/ 'clusterParameterStatus' smart constructor.
data ClusterParameterStatus = ClusterParameterStatus'
  { _cpsParameterApplyErrorDescription ::
      !(Maybe Text),
    _cpsParameterName :: !(Maybe Text),
    _cpsParameterApplyStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterParameterStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsParameterApplyErrorDescription' - The error that prevented the parameter from being applied to the database.
--
-- * 'cpsParameterName' - The name of the parameter.
--
-- * 'cpsParameterApplyStatus' - The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied. The following are possible statuses and descriptions.     * @in-sync@ : The parameter value is in sync with the database.     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.     * @applying@ : The parameter value is being applied to the database.     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
clusterParameterStatus ::
  ClusterParameterStatus
clusterParameterStatus =
  ClusterParameterStatus'
    { _cpsParameterApplyErrorDescription =
        Nothing,
      _cpsParameterName = Nothing,
      _cpsParameterApplyStatus = Nothing
    }

-- | The error that prevented the parameter from being applied to the database.
cpsParameterApplyErrorDescription :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyErrorDescription = lens _cpsParameterApplyErrorDescription (\s a -> s {_cpsParameterApplyErrorDescription = a})

-- | The name of the parameter.
cpsParameterName :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterName = lens _cpsParameterName (\s a -> s {_cpsParameterName = a})

-- | The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied. The following are possible statuses and descriptions.     * @in-sync@ : The parameter value is in sync with the database.     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.     * @applying@ : The parameter value is being applied to the database.     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
cpsParameterApplyStatus :: Lens' ClusterParameterStatus (Maybe Text)
cpsParameterApplyStatus = lens _cpsParameterApplyStatus (\s a -> s {_cpsParameterApplyStatus = a})

instance FromXML ClusterParameterStatus where
  parseXML x =
    ClusterParameterStatus'
      <$> (x .@? "ParameterApplyErrorDescription")
      <*> (x .@? "ParameterName")
      <*> (x .@? "ParameterApplyStatus")

instance Hashable ClusterParameterStatus

instance NFData ClusterParameterStatus
