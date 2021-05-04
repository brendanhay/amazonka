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
-- Module      : Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes the source of a CloudFormation stack record (i.e., the export
-- snapshot record).
--
-- /See:/ 'newCloudFormationStackRecordSourceInfo' smart constructor.
data CloudFormationStackRecordSourceInfo = CloudFormationStackRecordSourceInfo'
  { -- | The Amazon Resource Name (ARN) of the export snapshot record.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
    resourceType :: Prelude.Maybe CloudFormationStackRecordSourceType,
    -- | The name of the record.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationStackRecordSourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cloudFormationStackRecordSourceInfo_arn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- 'resourceType', 'cloudFormationStackRecordSourceInfo_resourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@).
--
-- 'name', 'cloudFormationStackRecordSourceInfo_name' - The name of the record.
newCloudFormationStackRecordSourceInfo ::
  CloudFormationStackRecordSourceInfo
newCloudFormationStackRecordSourceInfo =
  CloudFormationStackRecordSourceInfo'
    { arn =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the export snapshot record.
cloudFormationStackRecordSourceInfo_arn :: Lens.Lens' CloudFormationStackRecordSourceInfo (Prelude.Maybe Prelude.Text)
cloudFormationStackRecordSourceInfo_arn = Lens.lens (\CloudFormationStackRecordSourceInfo' {arn} -> arn) (\s@CloudFormationStackRecordSourceInfo' {} a -> s {arn = a} :: CloudFormationStackRecordSourceInfo)

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
cloudFormationStackRecordSourceInfo_resourceType :: Lens.Lens' CloudFormationStackRecordSourceInfo (Prelude.Maybe CloudFormationStackRecordSourceType)
cloudFormationStackRecordSourceInfo_resourceType = Lens.lens (\CloudFormationStackRecordSourceInfo' {resourceType} -> resourceType) (\s@CloudFormationStackRecordSourceInfo' {} a -> s {resourceType = a} :: CloudFormationStackRecordSourceInfo)

-- | The name of the record.
cloudFormationStackRecordSourceInfo_name :: Lens.Lens' CloudFormationStackRecordSourceInfo (Prelude.Maybe Prelude.Text)
cloudFormationStackRecordSourceInfo_name = Lens.lens (\CloudFormationStackRecordSourceInfo' {name} -> name) (\s@CloudFormationStackRecordSourceInfo' {} a -> s {name = a} :: CloudFormationStackRecordSourceInfo)

instance
  Prelude.FromJSON
    CloudFormationStackRecordSourceInfo
  where
  parseJSON =
    Prelude.withObject
      "CloudFormationStackRecordSourceInfo"
      ( \x ->
          CloudFormationStackRecordSourceInfo'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "name")
      )

instance
  Prelude.Hashable
    CloudFormationStackRecordSourceInfo

instance
  Prelude.NFData
    CloudFormationStackRecordSourceInfo
