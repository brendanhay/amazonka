{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import Network.AWS.Prelude

-- | Describes the source of a CloudFormation stack record (i.e., the export snapshot record).
--
--
--
-- /See:/ 'cloudFormationStackRecordSourceInfo' smart constructor.
data CloudFormationStackRecordSourceInfo = CloudFormationStackRecordSourceInfo'
  { _cfsrsiResourceType ::
      !( Maybe
           CloudFormationStackRecordSourceType
       ),
    _cfsrsiArn ::
      !(Maybe Text),
    _cfsrsiName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFormationStackRecordSourceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsrsiResourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- * 'cfsrsiArn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- * 'cfsrsiName' - The name of the record.
cloudFormationStackRecordSourceInfo ::
  CloudFormationStackRecordSourceInfo
cloudFormationStackRecordSourceInfo =
  CloudFormationStackRecordSourceInfo'
    { _cfsrsiResourceType =
        Nothing,
      _cfsrsiArn = Nothing,
      _cfsrsiName = Nothing
    }

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
cfsrsiResourceType :: Lens' CloudFormationStackRecordSourceInfo (Maybe CloudFormationStackRecordSourceType)
cfsrsiResourceType = lens _cfsrsiResourceType (\s a -> s {_cfsrsiResourceType = a})

-- | The Amazon Resource Name (ARN) of the export snapshot record.
cfsrsiArn :: Lens' CloudFormationStackRecordSourceInfo (Maybe Text)
cfsrsiArn = lens _cfsrsiArn (\s a -> s {_cfsrsiArn = a})

-- | The name of the record.
cfsrsiName :: Lens' CloudFormationStackRecordSourceInfo (Maybe Text)
cfsrsiName = lens _cfsrsiName (\s a -> s {_cfsrsiName = a})

instance FromJSON CloudFormationStackRecordSourceInfo where
  parseJSON =
    withObject
      "CloudFormationStackRecordSourceInfo"
      ( \x ->
          CloudFormationStackRecordSourceInfo'
            <$> (x .:? "resourceType") <*> (x .:? "arn") <*> (x .:? "name")
      )

instance Hashable CloudFormationStackRecordSourceInfo

instance NFData CloudFormationStackRecordSourceInfo
