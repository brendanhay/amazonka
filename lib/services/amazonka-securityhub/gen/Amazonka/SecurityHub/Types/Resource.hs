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
-- Module      : Amazonka.SecurityHub.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.DataClassificationDetails
import Amazonka.SecurityHub.Types.Partition
import Amazonka.SecurityHub.Types.ResourceDetails

-- | A resource related to a finding.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | A list of Amazon Web Services tags associated with a resource at the
    -- time the finding was processed.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about sensitive data that was detected on the
    -- resource.
    dataClassification :: Prelude.Maybe DataClassificationDetails,
    -- | Identifies the role of the resource in the finding. A resource is either
    -- the actor or target of the finding activity,
    resourceRole :: Prelude.Maybe Prelude.Text,
    -- | Additional details about the resource related to a finding.
    details :: Prelude.Maybe ResourceDetails,
    -- | The canonical Amazon Web Services partition name that the Region is
    -- assigned to.
    partition :: Prelude.Maybe Partition,
    -- | The canonical Amazon Web Services external Region name where this
    -- resource is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource that details are provided for. If possible, set
    -- @Type@ to one of the supported resource types. For example, if the
    -- resource is an EC2 instance, then set @Type@ to @AwsEc2Instance@.
    --
    -- If the resource does not match any of the provided types, then set
    -- @Type@ to @Other@.
    type' :: Prelude.Text,
    -- | The canonical identifier for the given resource type.
    id :: Prelude.Text
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
-- 'tags', 'resource_tags' - A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
--
-- 'dataClassification', 'resource_dataClassification' - Contains information about sensitive data that was detected on the
-- resource.
--
-- 'resourceRole', 'resource_resourceRole' - Identifies the role of the resource in the finding. A resource is either
-- the actor or target of the finding activity,
--
-- 'details', 'resource_details' - Additional details about the resource related to a finding.
--
-- 'partition', 'resource_partition' - The canonical Amazon Web Services partition name that the Region is
-- assigned to.
--
-- 'region', 'resource_region' - The canonical Amazon Web Services external Region name where this
-- resource is located.
--
-- 'type'', 'resource_type' - The type of the resource that details are provided for. If possible, set
-- @Type@ to one of the supported resource types. For example, if the
-- resource is an EC2 instance, then set @Type@ to @AwsEc2Instance@.
--
-- If the resource does not match any of the provided types, then set
-- @Type@ to @Other@.
--
-- 'id', 'resource_id' - The canonical identifier for the given resource type.
newResource ::
  -- | 'type''
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  Resource
newResource pType_ pId_ =
  Resource'
    { tags = Prelude.Nothing,
      dataClassification = Prelude.Nothing,
      resourceRole = Prelude.Nothing,
      details = Prelude.Nothing,
      partition = Prelude.Nothing,
      region = Prelude.Nothing,
      type' = pType_,
      id = pId_
    }

-- | A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
resource_tags :: Lens.Lens' Resource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resource_tags = Lens.lens (\Resource' {tags} -> tags) (\s@Resource' {} a -> s {tags = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about sensitive data that was detected on the
-- resource.
resource_dataClassification :: Lens.Lens' Resource (Prelude.Maybe DataClassificationDetails)
resource_dataClassification = Lens.lens (\Resource' {dataClassification} -> dataClassification) (\s@Resource' {} a -> s {dataClassification = a} :: Resource)

-- | Identifies the role of the resource in the finding. A resource is either
-- the actor or target of the finding activity,
resource_resourceRole :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceRole = Lens.lens (\Resource' {resourceRole} -> resourceRole) (\s@Resource' {} a -> s {resourceRole = a} :: Resource)

-- | Additional details about the resource related to a finding.
resource_details :: Lens.Lens' Resource (Prelude.Maybe ResourceDetails)
resource_details = Lens.lens (\Resource' {details} -> details) (\s@Resource' {} a -> s {details = a} :: Resource)

-- | The canonical Amazon Web Services partition name that the Region is
-- assigned to.
resource_partition :: Lens.Lens' Resource (Prelude.Maybe Partition)
resource_partition = Lens.lens (\Resource' {partition} -> partition) (\s@Resource' {} a -> s {partition = a} :: Resource)

-- | The canonical Amazon Web Services external Region name where this
-- resource is located.
resource_region :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_region = Lens.lens (\Resource' {region} -> region) (\s@Resource' {} a -> s {region = a} :: Resource)

-- | The type of the resource that details are provided for. If possible, set
-- @Type@ to one of the supported resource types. For example, if the
-- resource is an EC2 instance, then set @Type@ to @AwsEc2Instance@.
--
-- If the resource does not match any of the provided types, then set
-- @Type@ to @Other@.
resource_type :: Lens.Lens' Resource Prelude.Text
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | The canonical identifier for the given resource type.
resource_id :: Lens.Lens' Resource Prelude.Text
resource_id = Lens.lens (\Resource' {id} -> id) (\s@Resource' {} a -> s {id = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DataClassification")
            Prelude.<*> (x Data..:? "ResourceRole")
            Prelude.<*> (x Data..:? "Details")
            Prelude.<*> (x Data..:? "Partition")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dataClassification
      `Prelude.hashWithSalt` resourceRole
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` partition
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dataClassification
      `Prelude.seq` Prelude.rnf resourceRole
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf partition
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON Resource where
  toJSON Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("DataClassification" Data..=)
              Prelude.<$> dataClassification,
            ("ResourceRole" Data..=) Prelude.<$> resourceRole,
            ("Details" Data..=) Prelude.<$> details,
            ("Partition" Data..=) Prelude.<$> partition,
            ("Region" Data..=) Prelude.<$> region,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Id" Data..= id)
          ]
      )
