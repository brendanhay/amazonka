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
-- Module      : Amazonka.Lightsail.Types.CloudFormationStackRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.CloudFormationStackRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Amazonka.Lightsail.Types.DestinationInfo
import Amazonka.Lightsail.Types.RecordState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes a CloudFormation stack record created as a result of the
-- @create cloud formation stack@ action.
--
-- A CloudFormation stack record provides information about the AWS
-- CloudFormation stack used to create a new Amazon Elastic Compute Cloud
-- instance from an exported Lightsail instance snapshot.
--
-- /See:/ 'newCloudFormationStackRecord' smart constructor.
data CloudFormationStackRecord = CloudFormationStackRecord'
  { -- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the CloudFormation stack record was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | A list of objects describing the destination service, which is AWS
    -- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
    -- CloudFormation stack.
    destinationInfo :: Prelude.Maybe DestinationInfo,
    -- | A list of objects describing the Availability Zone and Amazon Web
    -- Services Region of the CloudFormation stack record.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the CloudFormation stack record. It starts with
    -- @CloudFormationStackRecord@ followed by a GUID.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | A list of objects describing the source of the CloudFormation stack
    -- record.
    sourceInfo :: Prelude.Maybe [CloudFormationStackRecordSourceInfo],
    -- | The current state of the CloudFormation stack record.
    state :: Prelude.Maybe RecordState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationStackRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cloudFormationStackRecord_arn' - The Amazon Resource Name (ARN) of the CloudFormation stack record.
--
-- 'createdAt', 'cloudFormationStackRecord_createdAt' - The date when the CloudFormation stack record was created.
--
-- 'destinationInfo', 'cloudFormationStackRecord_destinationInfo' - A list of objects describing the destination service, which is AWS
-- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
-- CloudFormation stack.
--
-- 'location', 'cloudFormationStackRecord_location' - A list of objects describing the Availability Zone and Amazon Web
-- Services Region of the CloudFormation stack record.
--
-- 'name', 'cloudFormationStackRecord_name' - The name of the CloudFormation stack record. It starts with
-- @CloudFormationStackRecord@ followed by a GUID.
--
-- 'resourceType', 'cloudFormationStackRecord_resourceType' - The Lightsail resource type (e.g., @CloudFormationStackRecord@).
--
-- 'sourceInfo', 'cloudFormationStackRecord_sourceInfo' - A list of objects describing the source of the CloudFormation stack
-- record.
--
-- 'state', 'cloudFormationStackRecord_state' - The current state of the CloudFormation stack record.
newCloudFormationStackRecord ::
  CloudFormationStackRecord
newCloudFormationStackRecord =
  CloudFormationStackRecord'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      destinationInfo = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sourceInfo = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
cloudFormationStackRecord_arn :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.Text)
cloudFormationStackRecord_arn = Lens.lens (\CloudFormationStackRecord' {arn} -> arn) (\s@CloudFormationStackRecord' {} a -> s {arn = a} :: CloudFormationStackRecord)

-- | The date when the CloudFormation stack record was created.
cloudFormationStackRecord_createdAt :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.UTCTime)
cloudFormationStackRecord_createdAt = Lens.lens (\CloudFormationStackRecord' {createdAt} -> createdAt) (\s@CloudFormationStackRecord' {} a -> s {createdAt = a} :: CloudFormationStackRecord) Prelude.. Lens.mapping Data._Time

-- | A list of objects describing the destination service, which is AWS
-- CloudFormation, and the Amazon Resource Name (ARN) of the AWS
-- CloudFormation stack.
cloudFormationStackRecord_destinationInfo :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe DestinationInfo)
cloudFormationStackRecord_destinationInfo = Lens.lens (\CloudFormationStackRecord' {destinationInfo} -> destinationInfo) (\s@CloudFormationStackRecord' {} a -> s {destinationInfo = a} :: CloudFormationStackRecord)

-- | A list of objects describing the Availability Zone and Amazon Web
-- Services Region of the CloudFormation stack record.
cloudFormationStackRecord_location :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe ResourceLocation)
cloudFormationStackRecord_location = Lens.lens (\CloudFormationStackRecord' {location} -> location) (\s@CloudFormationStackRecord' {} a -> s {location = a} :: CloudFormationStackRecord)

-- | The name of the CloudFormation stack record. It starts with
-- @CloudFormationStackRecord@ followed by a GUID.
cloudFormationStackRecord_name :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe Prelude.Text)
cloudFormationStackRecord_name = Lens.lens (\CloudFormationStackRecord' {name} -> name) (\s@CloudFormationStackRecord' {} a -> s {name = a} :: CloudFormationStackRecord)

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@).
cloudFormationStackRecord_resourceType :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe ResourceType)
cloudFormationStackRecord_resourceType = Lens.lens (\CloudFormationStackRecord' {resourceType} -> resourceType) (\s@CloudFormationStackRecord' {} a -> s {resourceType = a} :: CloudFormationStackRecord)

-- | A list of objects describing the source of the CloudFormation stack
-- record.
cloudFormationStackRecord_sourceInfo :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe [CloudFormationStackRecordSourceInfo])
cloudFormationStackRecord_sourceInfo = Lens.lens (\CloudFormationStackRecord' {sourceInfo} -> sourceInfo) (\s@CloudFormationStackRecord' {} a -> s {sourceInfo = a} :: CloudFormationStackRecord) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the CloudFormation stack record.
cloudFormationStackRecord_state :: Lens.Lens' CloudFormationStackRecord (Prelude.Maybe RecordState)
cloudFormationStackRecord_state = Lens.lens (\CloudFormationStackRecord' {state} -> state) (\s@CloudFormationStackRecord' {} a -> s {state = a} :: CloudFormationStackRecord)

instance Data.FromJSON CloudFormationStackRecord where
  parseJSON =
    Data.withObject
      "CloudFormationStackRecord"
      ( \x ->
          CloudFormationStackRecord'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "destinationInfo")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "sourceInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "state")
      )

instance Prelude.Hashable CloudFormationStackRecord where
  hashWithSalt _salt CloudFormationStackRecord' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` destinationInfo
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` sourceInfo
      `Prelude.hashWithSalt` state

instance Prelude.NFData CloudFormationStackRecord where
  rnf CloudFormationStackRecord' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf destinationInfo
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf sourceInfo
      `Prelude.seq` Prelude.rnf state
