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
-- Module      : Network.AWS.Inspector.Types.ResourceGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.ResourceGroupTag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a resource group. The resource group defines
-- a set of tags that, when queried, identify the AWS resources that make
-- up the assessment target. This data type is used as the response element
-- in the DescribeResourceGroups action.
--
-- /See:/ 'newResourceGroup' smart constructor.
data ResourceGroup = ResourceGroup'
  { -- | The ARN of the resource group.
    arn :: Prelude.Text,
    -- | The tags (key and value pairs) of the resource group. This data type
    -- property is used in the CreateResourceGroup action.
    tags :: Prelude.NonEmpty ResourceGroupTag,
    -- | The time at which resource group is created.
    createdAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceGroup_arn' - The ARN of the resource group.
--
-- 'tags', 'resourceGroup_tags' - The tags (key and value pairs) of the resource group. This data type
-- property is used in the CreateResourceGroup action.
--
-- 'createdAt', 'resourceGroup_createdAt' - The time at which resource group is created.
newResourceGroup ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'tags'
  Prelude.NonEmpty ResourceGroupTag ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  ResourceGroup
newResourceGroup pArn_ pTags_ pCreatedAt_ =
  ResourceGroup'
    { arn = pArn_,
      tags = Lens._Coerce Lens.# pTags_,
      createdAt = Core._Time Lens.# pCreatedAt_
    }

-- | The ARN of the resource group.
resourceGroup_arn :: Lens.Lens' ResourceGroup Prelude.Text
resourceGroup_arn = Lens.lens (\ResourceGroup' {arn} -> arn) (\s@ResourceGroup' {} a -> s {arn = a} :: ResourceGroup)

-- | The tags (key and value pairs) of the resource group. This data type
-- property is used in the CreateResourceGroup action.
resourceGroup_tags :: Lens.Lens' ResourceGroup (Prelude.NonEmpty ResourceGroupTag)
resourceGroup_tags = Lens.lens (\ResourceGroup' {tags} -> tags) (\s@ResourceGroup' {} a -> s {tags = a} :: ResourceGroup) Prelude.. Lens._Coerce

-- | The time at which resource group is created.
resourceGroup_createdAt :: Lens.Lens' ResourceGroup Prelude.UTCTime
resourceGroup_createdAt = Lens.lens (\ResourceGroup' {createdAt} -> createdAt) (\s@ResourceGroup' {} a -> s {createdAt = a} :: ResourceGroup) Prelude.. Core._Time

instance Core.FromJSON ResourceGroup where
  parseJSON =
    Core.withObject
      "ResourceGroup"
      ( \x ->
          ResourceGroup'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "tags")
            Prelude.<*> (x Core..: "createdAt")
      )

instance Prelude.Hashable ResourceGroup

instance Prelude.NFData ResourceGroup
