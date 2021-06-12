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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType

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
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
    arn :: Core.Maybe Core.Text,
    -- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
    resourceType :: Core.Maybe ResourceType,
    -- | The current state of the CloudFormation stack record.
    state :: Core.Maybe RecordState,
    -- | The name of the CloudFormation stack record. It starts with
    -- @CloudFormationStackRecord@ followed by a GUID.
    name :: Core.Maybe Core.Text,
    -- | A list of objects describing the source of the CloudFormation stack
    -- record.
    sourceInfo :: Core.Maybe [CloudFormationStackRecordSourceInfo],
    -- | A list of objects describing the Availability Zone and AWS Region of the
    -- CloudFormation stack record.
    location :: Core.Maybe ResourceLocation,
    -- | A list of objects describing the destination service, which is AWS
    -- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
    -- CloudFormation stack.
    destinationInfo :: Core.Maybe DestinationInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      sourceInfo = Core.Nothing,
      location = Core.Nothing,
      destinationInfo = Core.Nothing
    }

-- | The date when the CloudFormation stack record was created.
cloudFormationStackRecord_createdAt :: Lens.Lens' CloudFormationStackRecord (Core.Maybe Core.UTCTime)
cloudFormationStackRecord_createdAt = Lens.lens (\CloudFormationStackRecord' {createdAt} -> createdAt) (\s@CloudFormationStackRecord' {} a -> s {createdAt = a} :: CloudFormationStackRecord) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
cloudFormationStackRecord_arn :: Lens.Lens' CloudFormationStackRecord (Core.Maybe Core.Text)
cloudFormationStackRecord_arn = Lens.lens (\CloudFormationStackRecord' {arn} -> arn) (\s@CloudFormationStackRecord' {} a -> s {arn = a} :: CloudFormationStackRecord)

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
cloudFormationStackRecord_resourceType :: Lens.Lens' CloudFormationStackRecord (Core.Maybe ResourceType)
cloudFormationStackRecord_resourceType = Lens.lens (\CloudFormationStackRecord' {resourceType} -> resourceType) (\s@CloudFormationStackRecord' {} a -> s {resourceType = a} :: CloudFormationStackRecord)

-- | The current state of the CloudFormation stack record.
cloudFormationStackRecord_state :: Lens.Lens' CloudFormationStackRecord (Core.Maybe RecordState)
cloudFormationStackRecord_state = Lens.lens (\CloudFormationStackRecord' {state} -> state) (\s@CloudFormationStackRecord' {} a -> s {state = a} :: CloudFormationStackRecord)

-- | The name of the CloudFormation stack record. It starts with
-- @CloudFormationStackRecord@ followed by a GUID.
cloudFormationStackRecord_name :: Lens.Lens' CloudFormationStackRecord (Core.Maybe Core.Text)
cloudFormationStackRecord_name = Lens.lens (\CloudFormationStackRecord' {name} -> name) (\s@CloudFormationStackRecord' {} a -> s {name = a} :: CloudFormationStackRecord)

-- | A list of objects describing the source of the CloudFormation stack
-- record.
cloudFormationStackRecord_sourceInfo :: Lens.Lens' CloudFormationStackRecord (Core.Maybe [CloudFormationStackRecordSourceInfo])
cloudFormationStackRecord_sourceInfo = Lens.lens (\CloudFormationStackRecord' {sourceInfo} -> sourceInfo) (\s@CloudFormationStackRecord' {} a -> s {sourceInfo = a} :: CloudFormationStackRecord) Core.. Lens.mapping Lens._Coerce

-- | A list of objects describing the Availability Zone and AWS Region of the
-- CloudFormation stack record.
cloudFormationStackRecord_location :: Lens.Lens' CloudFormationStackRecord (Core.Maybe ResourceLocation)
cloudFormationStackRecord_location = Lens.lens (\CloudFormationStackRecord' {location} -> location) (\s@CloudFormationStackRecord' {} a -> s {location = a} :: CloudFormationStackRecord)

-- | A list of objects describing the destination service, which is AWS
-- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
-- CloudFormation stack.
cloudFormationStackRecord_destinationInfo :: Lens.Lens' CloudFormationStackRecord (Core.Maybe DestinationInfo)
cloudFormationStackRecord_destinationInfo = Lens.lens (\CloudFormationStackRecord' {destinationInfo} -> destinationInfo) (\s@CloudFormationStackRecord' {} a -> s {destinationInfo = a} :: CloudFormationStackRecord)

instance Core.FromJSON CloudFormationStackRecord where
  parseJSON =
    Core.withObject
      "CloudFormationStackRecord"
      ( \x ->
          CloudFormationStackRecord'
            Core.<$> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "sourceInfo" Core..!= Core.mempty)
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "destinationInfo")
      )

instance Core.Hashable CloudFormationStackRecord

instance Core.NFData CloudFormationStackRecord
