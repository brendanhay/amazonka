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
-- Module      : Amazonka.IoT.Types.NonCompliantResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.NonCompliantResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ResourceIdentifier
import Amazonka.IoT.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource that was noncompliant with the audit
-- check.
--
-- /See:/ 'newNonCompliantResource' smart constructor.
data NonCompliantResource = NonCompliantResource'
  { -- | The type of the noncompliant resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Other information about the noncompliant resource.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'resourceType', 'nonCompliantResource_resourceType' - The type of the noncompliant resource.
--
-- 'additionalInfo', 'nonCompliantResource_additionalInfo' - Other information about the noncompliant resource.
--
-- 'resourceIdentifier', 'nonCompliantResource_resourceIdentifier' - Information that identifies the noncompliant resource.
newNonCompliantResource ::
  NonCompliantResource
newNonCompliantResource =
  NonCompliantResource'
    { resourceType =
        Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | The type of the noncompliant resource.
nonCompliantResource_resourceType :: Lens.Lens' NonCompliantResource (Prelude.Maybe ResourceType)
nonCompliantResource_resourceType = Lens.lens (\NonCompliantResource' {resourceType} -> resourceType) (\s@NonCompliantResource' {} a -> s {resourceType = a} :: NonCompliantResource)

-- | Other information about the noncompliant resource.
nonCompliantResource_additionalInfo :: Lens.Lens' NonCompliantResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
nonCompliantResource_additionalInfo = Lens.lens (\NonCompliantResource' {additionalInfo} -> additionalInfo) (\s@NonCompliantResource' {} a -> s {additionalInfo = a} :: NonCompliantResource) Prelude.. Lens.mapping Lens.coerced

-- | Information that identifies the noncompliant resource.
nonCompliantResource_resourceIdentifier :: Lens.Lens' NonCompliantResource (Prelude.Maybe ResourceIdentifier)
nonCompliantResource_resourceIdentifier = Lens.lens (\NonCompliantResource' {resourceIdentifier} -> resourceIdentifier) (\s@NonCompliantResource' {} a -> s {resourceIdentifier = a} :: NonCompliantResource)

instance Data.FromJSON NonCompliantResource where
  parseJSON =
    Data.withObject
      "NonCompliantResource"
      ( \x ->
          NonCompliantResource'
            Prelude.<$> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "additionalInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceIdentifier")
      )

instance Prelude.Hashable NonCompliantResource where
  hashWithSalt _salt NonCompliantResource' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData NonCompliantResource where
  rnf NonCompliantResource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf resourceIdentifier
