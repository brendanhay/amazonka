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
-- Module      : Network.AWS.ResourceGroups.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceIdentifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that contains the ARN of a resource and its resource type.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The ARN of a resource.
    resourceArn :: Core.Maybe Core.Text,
    -- | The resource type of a resource, such as @AWS::EC2::Instance@.
    resourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'resourceIdentifier_resourceArn' - The ARN of a resource.
--
-- 'resourceType', 'resourceIdentifier_resourceType' - The resource type of a resource, such as @AWS::EC2::Instance@.
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { resourceArn = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The ARN of a resource.
resourceIdentifier_resourceArn :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_resourceArn = Lens.lens (\ResourceIdentifier' {resourceArn} -> resourceArn) (\s@ResourceIdentifier' {} a -> s {resourceArn = a} :: ResourceIdentifier)

-- | The resource type of a resource, such as @AWS::EC2::Instance@.
resourceIdentifier_resourceType :: Lens.Lens' ResourceIdentifier (Core.Maybe Core.Text)
resourceIdentifier_resourceType = Lens.lens (\ResourceIdentifier' {resourceType} -> resourceType) (\s@ResourceIdentifier' {} a -> s {resourceType = a} :: ResourceIdentifier)

instance Core.FromJSON ResourceIdentifier where
  parseJSON =
    Core.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Core.<$> (x Core..:? "ResourceArn")
            Core.<*> (x Core..:? "ResourceType")
      )

instance Core.Hashable ResourceIdentifier

instance Core.NFData ResourceIdentifier
