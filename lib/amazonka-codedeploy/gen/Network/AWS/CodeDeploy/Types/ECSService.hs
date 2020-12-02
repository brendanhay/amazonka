{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSService where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the service and cluster names used to identify an Amazon ECS deployment's target.
--
--
--
-- /See:/ 'eCSService' smart constructor.
data ECSService = ECSService'
  { _ecssServiceName :: !(Maybe Text),
    _ecssClusterName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ECSService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecssServiceName' - The name of the target Amazon ECS service.
--
-- * 'ecssClusterName' - The name of the cluster that the Amazon ECS service is associated with.
eCSService ::
  ECSService
eCSService =
  ECSService'
    { _ecssServiceName = Nothing,
      _ecssClusterName = Nothing
    }

-- | The name of the target Amazon ECS service.
ecssServiceName :: Lens' ECSService (Maybe Text)
ecssServiceName = lens _ecssServiceName (\s a -> s {_ecssServiceName = a})

-- | The name of the cluster that the Amazon ECS service is associated with.
ecssClusterName :: Lens' ECSService (Maybe Text)
ecssClusterName = lens _ecssClusterName (\s a -> s {_ecssClusterName = a})

instance FromJSON ECSService where
  parseJSON =
    withObject
      "ECSService"
      ( \x ->
          ECSService' <$> (x .:? "serviceName") <*> (x .:? "clusterName")
      )

instance Hashable ECSService

instance NFData ECSService

instance ToJSON ECSService where
  toJSON ECSService' {..} =
    object
      ( catMaybes
          [ ("serviceName" .=) <$> _ecssServiceName,
            ("clusterName" .=) <$> _ecssClusterName
          ]
      )
