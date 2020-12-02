{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EFSAuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EFSAuthorizationConfig where

import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorization configuration details for the Amazon EFS file system.
--
--
--
-- /See:/ 'eFSAuthorizationConfig' smart constructor.
data EFSAuthorizationConfig = EFSAuthorizationConfig'
  { _efsacAccessPointId ::
      !(Maybe Text),
    _efsacIam ::
      !(Maybe EFSAuthorizationConfigIAM)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EFSAuthorizationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsacAccessPointId' - The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
--
-- * 'efsacIam' - Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
eFSAuthorizationConfig ::
  EFSAuthorizationConfig
eFSAuthorizationConfig =
  EFSAuthorizationConfig'
    { _efsacAccessPointId = Nothing,
      _efsacIam = Nothing
    }

-- | The Amazon EFS access point ID to use. If an access point is specified, the root directory value specified in the @EFSVolumeConfiguration@ must either be omitted or set to @/@ which will enforce the path set on the EFS access point. If an access point is used, transit encryption must be enabled in the @EFSVolumeConfiguration@ . For more information, see <https://docs.aws.amazon.com/efs/latest/ug/efs-access-points.html Working with Amazon EFS Access Points> in the /Amazon Elastic File System User Guide/ .
efsacAccessPointId :: Lens' EFSAuthorizationConfig (Maybe Text)
efsacAccessPointId = lens _efsacAccessPointId (\s a -> s {_efsacAccessPointId = a})

-- | Whether or not to use the Amazon ECS task IAM role defined in a task definition when mounting the Amazon EFS file system. If enabled, transit encryption must be enabled in the @EFSVolumeConfiguration@ . If this parameter is omitted, the default value of @DISABLED@ is used. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/efs-volumes.html#efs-volume-accesspoints Using Amazon EFS Access Points> in the /Amazon Elastic Container Service Developer Guide/ .
efsacIam :: Lens' EFSAuthorizationConfig (Maybe EFSAuthorizationConfigIAM)
efsacIam = lens _efsacIam (\s a -> s {_efsacIam = a})

instance FromJSON EFSAuthorizationConfig where
  parseJSON =
    withObject
      "EFSAuthorizationConfig"
      ( \x ->
          EFSAuthorizationConfig'
            <$> (x .:? "accessPointId") <*> (x .:? "iam")
      )

instance Hashable EFSAuthorizationConfig

instance NFData EFSAuthorizationConfig

instance ToJSON EFSAuthorizationConfig where
  toJSON EFSAuthorizationConfig' {..} =
    object
      ( catMaybes
          [ ("accessPointId" .=) <$> _efsacAccessPointId,
            ("iam" .=) <$> _efsacIam
          ]
      )
