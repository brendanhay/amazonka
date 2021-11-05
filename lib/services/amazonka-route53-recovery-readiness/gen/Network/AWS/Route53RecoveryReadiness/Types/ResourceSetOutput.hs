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
-- Module      : Network.AWS.Route53RecoveryReadiness.Types.ResourceSetOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryReadiness.Types.ResourceSetOutput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryReadiness.Types.Resource

-- | A collection of resources of the same type
--
-- /See:/ 'newResourceSetOutput' smart constructor.
data ResourceSetOutput = ResourceSetOutput'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | AWS Resource Type of the resources in the ResourceSet
    resourceSetType :: Prelude.Text,
    -- | The name of the ResourceSet
    resourceSetName :: Prelude.Text,
    -- | The arn for the ResourceSet
    resourceSetArn :: Prelude.Text,
    -- | A list of Resource objects
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSetOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'resourceSetOutput_tags' - Undocumented member.
--
-- 'resourceSetType', 'resourceSetOutput_resourceSetType' - AWS Resource Type of the resources in the ResourceSet
--
-- 'resourceSetName', 'resourceSetOutput_resourceSetName' - The name of the ResourceSet
--
-- 'resourceSetArn', 'resourceSetOutput_resourceSetArn' - The arn for the ResourceSet
--
-- 'resources', 'resourceSetOutput_resources' - A list of Resource objects
newResourceSetOutput ::
  -- | 'resourceSetType'
  Prelude.Text ->
  -- | 'resourceSetName'
  Prelude.Text ->
  -- | 'resourceSetArn'
  Prelude.Text ->
  ResourceSetOutput
newResourceSetOutput
  pResourceSetType_
  pResourceSetName_
  pResourceSetArn_ =
    ResourceSetOutput'
      { tags = Prelude.Nothing,
        resourceSetType = pResourceSetType_,
        resourceSetName = pResourceSetName_,
        resourceSetArn = pResourceSetArn_,
        resources = Prelude.mempty
      }

-- | Undocumented member.
resourceSetOutput_tags :: Lens.Lens' ResourceSetOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resourceSetOutput_tags = Lens.lens (\ResourceSetOutput' {tags} -> tags) (\s@ResourceSetOutput' {} a -> s {tags = a} :: ResourceSetOutput) Prelude.. Lens.mapping Lens.coerced

-- | AWS Resource Type of the resources in the ResourceSet
resourceSetOutput_resourceSetType :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetType = Lens.lens (\ResourceSetOutput' {resourceSetType} -> resourceSetType) (\s@ResourceSetOutput' {} a -> s {resourceSetType = a} :: ResourceSetOutput)

-- | The name of the ResourceSet
resourceSetOutput_resourceSetName :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetName = Lens.lens (\ResourceSetOutput' {resourceSetName} -> resourceSetName) (\s@ResourceSetOutput' {} a -> s {resourceSetName = a} :: ResourceSetOutput)

-- | The arn for the ResourceSet
resourceSetOutput_resourceSetArn :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetArn = Lens.lens (\ResourceSetOutput' {resourceSetArn} -> resourceSetArn) (\s@ResourceSetOutput' {} a -> s {resourceSetArn = a} :: ResourceSetOutput)

-- | A list of Resource objects
resourceSetOutput_resources :: Lens.Lens' ResourceSetOutput [Resource]
resourceSetOutput_resources = Lens.lens (\ResourceSetOutput' {resources} -> resources) (\s@ResourceSetOutput' {} a -> s {resources = a} :: ResourceSetOutput) Prelude.. Lens.coerced

instance Core.FromJSON ResourceSetOutput where
  parseJSON =
    Core.withObject
      "ResourceSetOutput"
      ( \x ->
          ResourceSetOutput'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "resourceSetType")
            Prelude.<*> (x Core..: "resourceSetName")
            Prelude.<*> (x Core..: "resourceSetArn")
            Prelude.<*> (x Core..:? "resources" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceSetOutput

instance Prelude.NFData ResourceSetOutput
