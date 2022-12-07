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
-- Module      : Amazonka.RAM.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceRegionScope
import Amazonka.RAM.Types.ResourceStatus

-- | Describes a resource associated with a resource share in RAM.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The resource type. This takes the form of:
    -- @service-code@:@resource-code@
    type' :: Prelude.Maybe Prelude.Text,
    -- | Specifies the scope of visibility of this resource:
    --
    -- -   __REGIONAL__ – The resource can be accessed only by using requests
    --     that target the Amazon Web Services Region in which the resource
    --     exists.
    --
    -- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
    --     Services Region.
    resourceRegionScope :: Prelude.Maybe ResourceRegionScope,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share this resource is associated with.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the resource.
    status :: Prelude.Maybe ResourceStatus,
    -- | The date an time when the association was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the resource was associated with the resource
    -- share.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A message about the status of the resource.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource group. This value is available only if the resource is
    -- part of a resource group.
    resourceGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resource_type' - The resource type. This takes the form of:
-- @service-code@:@resource-code@
--
-- 'resourceRegionScope', 'resource_resourceRegionScope' - Specifies the scope of visibility of this resource:
--
-- -   __REGIONAL__ – The resource can be accessed only by using requests
--     that target the Amazon Web Services Region in which the resource
--     exists.
--
-- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
--     Services Region.
--
-- 'arn', 'resource_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource.
--
-- 'resourceShareArn', 'resource_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share this resource is associated with.
--
-- 'status', 'resource_status' - The current status of the resource.
--
-- 'lastUpdatedTime', 'resource_lastUpdatedTime' - The date an time when the association was last updated.
--
-- 'creationTime', 'resource_creationTime' - The date and time when the resource was associated with the resource
-- share.
--
-- 'statusMessage', 'resource_statusMessage' - A message about the status of the resource.
--
-- 'resourceGroupArn', 'resource_resourceGroupArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource group. This value is available only if the resource is
-- part of a resource group.
newResource ::
  Resource
newResource =
  Resource'
    { type' = Prelude.Nothing,
      resourceRegionScope = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      resourceGroupArn = Prelude.Nothing
    }

-- | The resource type. This takes the form of:
-- @service-code@:@resource-code@
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | Specifies the scope of visibility of this resource:
--
-- -   __REGIONAL__ – The resource can be accessed only by using requests
--     that target the Amazon Web Services Region in which the resource
--     exists.
--
-- -   __GLOBAL__ – The resource can be accessed from any Amazon Web
--     Services Region.
resource_resourceRegionScope :: Lens.Lens' Resource (Prelude.Maybe ResourceRegionScope)
resource_resourceRegionScope = Lens.lens (\Resource' {resourceRegionScope} -> resourceRegionScope) (\s@Resource' {} a -> s {resourceRegionScope = a} :: Resource)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share this resource is associated with.
resource_resourceShareArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceShareArn = Lens.lens (\Resource' {resourceShareArn} -> resourceShareArn) (\s@Resource' {} a -> s {resourceShareArn = a} :: Resource)

-- | The current status of the resource.
resource_status :: Lens.Lens' Resource (Prelude.Maybe ResourceStatus)
resource_status = Lens.lens (\Resource' {status} -> status) (\s@Resource' {} a -> s {status = a} :: Resource)

-- | The date an time when the association was last updated.
resource_lastUpdatedTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_lastUpdatedTime = Lens.lens (\Resource' {lastUpdatedTime} -> lastUpdatedTime) (\s@Resource' {} a -> s {lastUpdatedTime = a} :: Resource) Prelude.. Lens.mapping Data._Time

-- | The date and time when the resource was associated with the resource
-- share.
resource_creationTime :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_creationTime = Lens.lens (\Resource' {creationTime} -> creationTime) (\s@Resource' {} a -> s {creationTime = a} :: Resource) Prelude.. Lens.mapping Data._Time

-- | A message about the status of the resource.
resource_statusMessage :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_statusMessage = Lens.lens (\Resource' {statusMessage} -> statusMessage) (\s@Resource' {} a -> s {statusMessage = a} :: Resource)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource group. This value is available only if the resource is
-- part of a resource group.
resource_resourceGroupArn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceGroupArn = Lens.lens (\Resource' {resourceGroupArn} -> resourceGroupArn) (\s@Resource' {} a -> s {resourceGroupArn = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "resourceRegionScope")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "resourceShareArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "resourceGroupArn")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` resourceRegionScope
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` resourceGroupArn

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf resourceRegionScope
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf resourceGroupArn
