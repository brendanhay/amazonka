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
-- Module      : Network.AWS.IoT.Types.NonCompliantResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.NonCompliantResource where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the resource that was noncompliant with the audit
-- check.
--
-- /See:/ 'newNonCompliantResource' smart constructor.
data NonCompliantResource = NonCompliantResource'
  { -- | Other information about the noncompliant resource.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the noncompliant resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Information that identifies the noncompliant resource.
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NonCompliantResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'nonCompliantResource_additionalInfo' - Other information about the noncompliant resource.
--
-- 'resourceType', 'nonCompliantResource_resourceType' - The type of the noncompliant resource.
--
-- 'resourceIdentifier', 'nonCompliantResource_resourceIdentifier' - Information that identifies the noncompliant resource.
newNonCompliantResource ::
  NonCompliantResource
newNonCompliantResource =
  NonCompliantResource'
    { additionalInfo =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | Other information about the noncompliant resource.
nonCompliantResource_additionalInfo :: Lens.Lens' NonCompliantResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nonCompliantResource_additionalInfo = Lens.lens (\NonCompliantResource' {additionalInfo} -> additionalInfo) (\s@NonCompliantResource' {} a -> s {additionalInfo = a} :: NonCompliantResource) Prelude.. Lens.mapping Lens._Coerce

-- | The type of the noncompliant resource.
nonCompliantResource_resourceType :: Lens.Lens' NonCompliantResource (Prelude.Maybe ResourceType)
nonCompliantResource_resourceType = Lens.lens (\NonCompliantResource' {resourceType} -> resourceType) (\s@NonCompliantResource' {} a -> s {resourceType = a} :: NonCompliantResource)

-- | Information that identifies the noncompliant resource.
nonCompliantResource_resourceIdentifier :: Lens.Lens' NonCompliantResource (Prelude.Maybe ResourceIdentifier)
nonCompliantResource_resourceIdentifier = Lens.lens (\NonCompliantResource' {resourceIdentifier} -> resourceIdentifier) (\s@NonCompliantResource' {} a -> s {resourceIdentifier = a} :: NonCompliantResource)

instance Core.FromJSON NonCompliantResource where
  parseJSON =
    Core.withObject
      "NonCompliantResource"
      ( \x ->
          NonCompliantResource'
            Prelude.<$> (x Core..:? "additionalInfo" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "resourceIdentifier")
      )

instance Prelude.Hashable NonCompliantResource

instance Prelude.NFData NonCompliantResource
