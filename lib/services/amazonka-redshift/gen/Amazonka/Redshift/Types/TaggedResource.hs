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
-- Module      : Amazonka.Redshift.Types.TaggedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.TaggedResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | A tag and its associated resource.
--
-- /See:/ 'newTaggedResource' smart constructor.
data TaggedResource = TaggedResource'
  { -- | The Amazon Resource Name (ARN) with which the tag is associated, for
    -- example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource with which the tag is associated. Valid resource
    -- types are:
    --
    -- -   Cluster
    --
    -- -   CIDR\/IP
    --
    -- -   EC2 security group
    --
    -- -   Snapshot
    --
    -- -   Cluster security group
    --
    -- -   Subnet group
    --
    -- -   HSM connection
    --
    -- -   HSM certificate
    --
    -- -   Parameter group
    --
    -- For more information about Amazon Redshift resource types and
    -- constructing ARNs, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)>
    -- in the Amazon Redshift Cluster Management Guide.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The tag for the resource.
    tag :: Prelude.Maybe Tag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaggedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'taggedResource_resourceName' - The Amazon Resource Name (ARN) with which the tag is associated, for
-- example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
--
-- 'resourceType', 'taggedResource_resourceType' - The type of resource with which the tag is associated. Valid resource
-- types are:
--
-- -   Cluster
--
-- -   CIDR\/IP
--
-- -   EC2 security group
--
-- -   Snapshot
--
-- -   Cluster security group
--
-- -   Subnet group
--
-- -   HSM connection
--
-- -   HSM certificate
--
-- -   Parameter group
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)>
-- in the Amazon Redshift Cluster Management Guide.
--
-- 'tag', 'taggedResource_tag' - The tag for the resource.
newTaggedResource ::
  TaggedResource
newTaggedResource =
  TaggedResource'
    { resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tag = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) with which the tag is associated, for
-- example: @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
taggedResource_resourceName :: Lens.Lens' TaggedResource (Prelude.Maybe Prelude.Text)
taggedResource_resourceName = Lens.lens (\TaggedResource' {resourceName} -> resourceName) (\s@TaggedResource' {} a -> s {resourceName = a} :: TaggedResource)

-- | The type of resource with which the tag is associated. Valid resource
-- types are:
--
-- -   Cluster
--
-- -   CIDR\/IP
--
-- -   EC2 security group
--
-- -   Snapshot
--
-- -   Cluster security group
--
-- -   Subnet group
--
-- -   HSM connection
--
-- -   HSM certificate
--
-- -   Parameter group
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Constructing an Amazon Redshift Amazon Resource Name (ARN)>
-- in the Amazon Redshift Cluster Management Guide.
taggedResource_resourceType :: Lens.Lens' TaggedResource (Prelude.Maybe Prelude.Text)
taggedResource_resourceType = Lens.lens (\TaggedResource' {resourceType} -> resourceType) (\s@TaggedResource' {} a -> s {resourceType = a} :: TaggedResource)

-- | The tag for the resource.
taggedResource_tag :: Lens.Lens' TaggedResource (Prelude.Maybe Tag)
taggedResource_tag = Lens.lens (\TaggedResource' {tag} -> tag) (\s@TaggedResource' {} a -> s {tag = a} :: TaggedResource)

instance Data.FromXML TaggedResource where
  parseXML x =
    TaggedResource'
      Prelude.<$> (x Data..@? "ResourceName")
      Prelude.<*> (x Data..@? "ResourceType")
      Prelude.<*> (x Data..@? "Tag")

instance Prelude.Hashable TaggedResource where
  hashWithSalt _salt TaggedResource' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tag

instance Prelude.NFData TaggedResource where
  rnf TaggedResource' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tag
