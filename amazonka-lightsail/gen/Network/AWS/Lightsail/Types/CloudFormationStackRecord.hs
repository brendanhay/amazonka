{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecord where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes a CloudFormation stack record created as a result of the
-- @create cloud formation stack@ operation.
--
-- A CloudFormation stack record provides information about the AWS
-- CloudFormation stack used to create a new Amazon Elastic Compute Cloud
-- instance from an exported Lightsail instance snapshot.
--
-- /See:/ 'newCloudFormationStackRecord' smart constructor.
data CloudFormationStackRecord = CloudFormationStackRecord'
  { -- | The date when the CloudFormation stack record was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The current state of the CloudFormation stack record.
    state :: Prelude.Maybe RecordState,
    -- | The name of the CloudFormation stack record. It starts with
    -- @CloudFormationStackRecord@ followed by a GUID.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of objects describing the source of the CloudFormation stack
    -- record.
    sourceInfo :: Prelude.Maybe [CloudFormationStackRecordSourceInfo],
    -- | A list of objects describing the Availability Zone and AWS Region of the
    -- CloudFormation stack record.
    location :: Prelude.Maybe ResourceLocation,
    -- | A list of objects describing the destination service, which is AWS
    -- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
    -- CloudFormation stack.
    destinationInfo :: Prelude.Maybe DestinationInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationStackRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'cloudFormationStackRecord_createdAt' - The date when the CloudFormation stack record was created.
--
-- 'arn', 'cloudFormationStackRecord_arn' - The Amazon Resource Name (ARN) of the CloudFormation stack record.
--
-- 'resourceType', 'cloudFormationStackRecord_resourceType' - The Lightsail resource type (e.g., @CloudFormationStackRecord@).
--
-- 'state', 'cloudFormationStackRecord_state' - The current state of the CloudFormation stack record.
--
-- 'name', 'cloudFormationStackRecord_name' - The name of the CloudFormation stack record. It starts with
-- @CloudFormationStackRecord@ followed by a GUID.
--
-- 'sourceInfo', 'cloudFormationStackRecord_sourceInfo' - A list of objects describing the source of the CloudFormation stack
-- record.
--
-- 'location', 'cloudFormationStackRecord_location' - A list of objects describing the Availability Zone and AWS Region of the
-- CloudFormation stack record.
--
-- 'destinationInfo', 'cloudFormationStackRecord_destinationInfo' - A list of objects describing the destination service, which is AWS
-- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
-- CloudFormation stack.
newCloudFormationStackRecord ::
  CloudFormationStackRecord
newCloudFormationStackRecord =
  CloudFormationStackRecord'
    { createdAt =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceInfo = Prelude.Nothing,
      location = Prelude.Nothing,
      destinationInfo = Prelude.Nothing
    }

-- | The date when the CloudFormation stack record was created.
cloudFormationStackRecord_createdAt :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.UTCTime)
cloudFormationStackRecord_createdAt = Lens.lens (\CloudFormationStackRecord' {createdAt} -> createdAt) (\s@CloudFormationStackRecord' {} a -> s {createdAt = a} :: CloudFormationStackRecord) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
cloudFormationStackRecord_arn :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.Text)
cloudFormationStackRecord_arn = Lens.lens (\CloudFormationStackRecord' {arn} -> arn) (\s@CloudFormationStackRecord' {} a -> s {arn = a} :: CloudFormationStackRecord)

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
cloudFormationStackRecord_resourceType :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe ResourceType)
cloudFormationStackRecord_resourceType = Lens.lens (\CloudFormationStackRecord' {resourceType} -> resourceType) (\s@CloudFormationStackRecord' {} a -> s {resourceType = a} :: CloudFormationStackRecord)

-- | The current state of the CloudFormation stack record.
cloudFormationStackRecord_state :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe RecordState)
cloudFormationStackRecord_state = Lens.lens (\CloudFormationStackRecord' {state} -> state) (\s@CloudFormationStackRecord' {} a -> s {state = a} :: CloudFormationStackRecord)

-- | The name of the CloudFormation stack record. It starts with
-- @CloudFormationStackRecord@ followed by a GUID.
cloudFormationStackRecord_name :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.Text)
cloudFormationStackRecord_name = Lens.lens (\CloudFormationStackRecord' {name} -> name) (\s@CloudFormationStackRecord' {} a -> s {name = a} :: CloudFormationStackRecord)

-- | A list of objects describing the source of the CloudFormation stack
-- record.
cloudFormationStackRecord_sourceInfo :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe [CloudFormationStackRecordSourceInfo])
cloudFormationStackRecord_sourceInfo = Lens.lens (\CloudFormationStackRecord' {sourceInfo} -> sourceInfo) (\s@CloudFormationStackRecord' {} a -> s {sourceInfo = a} :: CloudFormationStackRecord) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of objects describing the Availability Zone and AWS Region of the
-- CloudFormation stack record.
cloudFormationStackRecord_location :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe ResourceLocation)
cloudFormationStackRecord_location = Lens.lens (\CloudFormationStackRecord' {location} -> location) (\s@CloudFormationStackRecord' {} a -> s {location = a} :: CloudFormationStackRecord)

-- | A list of objects describing the destination service, which is AWS
-- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
-- CloudFormation stack.
cloudFormationStackRecord_destinationInfo :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe DestinationInfo)
cloudFormationStackRecord_destinationInfo = Lens.lens (\CloudFormationStackRecord' {destinationInfo} -> destinationInfo) (\s@CloudFormationStackRecord' {} a -> s {destinationInfo = a} :: CloudFormationStackRecord)

instance Prelude.FromJSON CloudFormationStackRecord where
  parseJSON =
    Prelude.withObject
      "CloudFormationStackRecord"
      ( \x ->
          CloudFormationStackRecord'
            Prelude.<$> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "sourceInfo"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "destinationInfo")
      )

instance Prelude.Hashable CloudFormationStackRecord

instance Prelude.NFData CloudFormationStackRecord
