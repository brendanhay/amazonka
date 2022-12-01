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
-- Module      : Amazonka.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RelatedResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.ResourceIdentifier
import Amazonka.IoT.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Information about a related resource.
--
-- /See:/ 'newRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { -- | The type of resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Other information about the resource.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information that identifies the resource.
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'relatedResource_resourceType' - The type of resource.
--
-- 'additionalInfo', 'relatedResource_additionalInfo' - Other information about the resource.
--
-- 'resourceIdentifier', 'relatedResource_resourceIdentifier' - Information that identifies the resource.
newRelatedResource ::
  RelatedResource
newRelatedResource =
  RelatedResource'
    { resourceType = Prelude.Nothing,
      additionalInfo = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | The type of resource.
relatedResource_resourceType :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceType)
relatedResource_resourceType = Lens.lens (\RelatedResource' {resourceType} -> resourceType) (\s@RelatedResource' {} a -> s {resourceType = a} :: RelatedResource)

-- | Other information about the resource.
relatedResource_additionalInfo :: Lens.Lens' RelatedResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
relatedResource_additionalInfo = Lens.lens (\RelatedResource' {additionalInfo} -> additionalInfo) (\s@RelatedResource' {} a -> s {additionalInfo = a} :: RelatedResource) Prelude.. Lens.mapping Lens.coerced

-- | Information that identifies the resource.
relatedResource_resourceIdentifier :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceIdentifier)
relatedResource_resourceIdentifier = Lens.lens (\RelatedResource' {resourceIdentifier} -> resourceIdentifier) (\s@RelatedResource' {} a -> s {resourceIdentifier = a} :: RelatedResource)

instance Core.FromJSON RelatedResource where
  parseJSON =
    Core.withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "additionalInfo" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceIdentifier")
      )

instance Prelude.Hashable RelatedResource where
  hashWithSalt _salt RelatedResource' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData RelatedResource where
  rnf RelatedResource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf resourceIdentifier
