{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecord where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes a CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
--
-- A CloudFormation stack record provides information about the AWS CloudFormation stack used to create a new Amazon Elastic Compute Cloud instance from an exported Lightsail instance snapshot.
--
--
-- /See:/ 'cloudFormationStackRecord' smart constructor.
data CloudFormationStackRecord = CloudFormationStackRecord'
  { _cfsrState ::
      !(Maybe RecordState),
    _cfsrDestinationInfo ::
      !(Maybe DestinationInfo),
    _cfsrResourceType ::
      !(Maybe ResourceType),
    _cfsrArn :: !(Maybe Text),
    _cfsrCreatedAt :: !(Maybe POSIX),
    _cfsrLocation ::
      !(Maybe ResourceLocation),
    _cfsrName :: !(Maybe Text),
    _cfsrSourceInfo ::
      !( Maybe
           [CloudFormationStackRecordSourceInfo]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFormationStackRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsrState' - The current state of the CloudFormation stack record.
--
-- * 'cfsrDestinationInfo' - A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
--
-- * 'cfsrResourceType' - The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
--
-- * 'cfsrArn' - The Amazon Resource Name (ARN) of the CloudFormation stack record.
--
-- * 'cfsrCreatedAt' - The date when the CloudFormation stack record was created.
--
-- * 'cfsrLocation' - A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
--
-- * 'cfsrName' - The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
--
-- * 'cfsrSourceInfo' - A list of objects describing the source of the CloudFormation stack record.
cloudFormationStackRecord ::
  CloudFormationStackRecord
cloudFormationStackRecord =
  CloudFormationStackRecord'
    { _cfsrState = Nothing,
      _cfsrDestinationInfo = Nothing,
      _cfsrResourceType = Nothing,
      _cfsrArn = Nothing,
      _cfsrCreatedAt = Nothing,
      _cfsrLocation = Nothing,
      _cfsrName = Nothing,
      _cfsrSourceInfo = Nothing
    }

-- | The current state of the CloudFormation stack record.
cfsrState :: Lens' CloudFormationStackRecord (Maybe RecordState)
cfsrState = lens _cfsrState (\s a -> s {_cfsrState = a})

-- | A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
cfsrDestinationInfo :: Lens' CloudFormationStackRecord (Maybe DestinationInfo)
cfsrDestinationInfo = lens _cfsrDestinationInfo (\s a -> s {_cfsrDestinationInfo = a})

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
cfsrResourceType :: Lens' CloudFormationStackRecord (Maybe ResourceType)
cfsrResourceType = lens _cfsrResourceType (\s a -> s {_cfsrResourceType = a})

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
cfsrArn :: Lens' CloudFormationStackRecord (Maybe Text)
cfsrArn = lens _cfsrArn (\s a -> s {_cfsrArn = a})

-- | The date when the CloudFormation stack record was created.
cfsrCreatedAt :: Lens' CloudFormationStackRecord (Maybe UTCTime)
cfsrCreatedAt = lens _cfsrCreatedAt (\s a -> s {_cfsrCreatedAt = a}) . mapping _Time

-- | A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
cfsrLocation :: Lens' CloudFormationStackRecord (Maybe ResourceLocation)
cfsrLocation = lens _cfsrLocation (\s a -> s {_cfsrLocation = a})

-- | The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
cfsrName :: Lens' CloudFormationStackRecord (Maybe Text)
cfsrName = lens _cfsrName (\s a -> s {_cfsrName = a})

-- | A list of objects describing the source of the CloudFormation stack record.
cfsrSourceInfo :: Lens' CloudFormationStackRecord [CloudFormationStackRecordSourceInfo]
cfsrSourceInfo = lens _cfsrSourceInfo (\s a -> s {_cfsrSourceInfo = a}) . _Default . _Coerce

instance FromJSON CloudFormationStackRecord where
  parseJSON =
    withObject
      "CloudFormationStackRecord"
      ( \x ->
          CloudFormationStackRecord'
            <$> (x .:? "state")
            <*> (x .:? "destinationInfo")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "name")
            <*> (x .:? "sourceInfo" .!= mempty)
      )

instance Hashable CloudFormationStackRecord

instance NFData CloudFormationStackRecord
