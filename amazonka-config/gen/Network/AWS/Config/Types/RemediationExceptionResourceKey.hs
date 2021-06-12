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
-- Module      : Network.AWS.Config.Types.RemediationExceptionResourceKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExceptionResourceKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The details that identify a resource within AWS Config, including the
-- resource type and resource ID.
--
-- /See:/ 'newRemediationExceptionResourceKey' smart constructor.
data RemediationExceptionResourceKey = RemediationExceptionResourceKey'
  { -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Core.Maybe Core.Text,
    -- | The type of a resource.
    resourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemediationExceptionResourceKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'remediationExceptionResourceKey_resourceId' - The ID of the resource (for example., sg-xxxxxx).
--
-- 'resourceType', 'remediationExceptionResourceKey_resourceType' - The type of a resource.
newRemediationExceptionResourceKey ::
  RemediationExceptionResourceKey
newRemediationExceptionResourceKey =
  RemediationExceptionResourceKey'
    { resourceId =
        Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
remediationExceptionResourceKey_resourceId :: Lens.Lens' RemediationExceptionResourceKey (Core.Maybe Core.Text)
remediationExceptionResourceKey_resourceId = Lens.lens (\RemediationExceptionResourceKey' {resourceId} -> resourceId) (\s@RemediationExceptionResourceKey' {} a -> s {resourceId = a} :: RemediationExceptionResourceKey)

-- | The type of a resource.
remediationExceptionResourceKey_resourceType :: Lens.Lens' RemediationExceptionResourceKey (Core.Maybe Core.Text)
remediationExceptionResourceKey_resourceType = Lens.lens (\RemediationExceptionResourceKey' {resourceType} -> resourceType) (\s@RemediationExceptionResourceKey' {} a -> s {resourceType = a} :: RemediationExceptionResourceKey)

instance
  Core.FromJSON
    RemediationExceptionResourceKey
  where
  parseJSON =
    Core.withObject
      "RemediationExceptionResourceKey"
      ( \x ->
          RemediationExceptionResourceKey'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "ResourceType")
      )

instance
  Core.Hashable
    RemediationExceptionResourceKey

instance Core.NFData RemediationExceptionResourceKey

instance Core.ToJSON RemediationExceptionResourceKey where
  toJSON RemediationExceptionResourceKey' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceId" Core..=) Core.<$> resourceId,
            ("ResourceType" Core..=) Core.<$> resourceType
          ]
      )
