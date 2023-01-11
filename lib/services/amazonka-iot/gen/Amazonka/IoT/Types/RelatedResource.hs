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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RelatedResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ResourceIdentifier
import Amazonka.IoT.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Information about a related resource.
--
-- /See:/ 'newRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { -- | Other information about the resource.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information that identifies the resource.
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier,
    -- | The type of resource.
    resourceType :: Prelude.Maybe ResourceType
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
-- 'additionalInfo', 'relatedResource_additionalInfo' - Other information about the resource.
--
-- 'resourceIdentifier', 'relatedResource_resourceIdentifier' - Information that identifies the resource.
--
-- 'resourceType', 'relatedResource_resourceType' - The type of resource.
newRelatedResource ::
  RelatedResource
newRelatedResource =
  RelatedResource'
    { additionalInfo = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Other information about the resource.
relatedResource_additionalInfo :: Lens.Lens' RelatedResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
relatedResource_additionalInfo = Lens.lens (\RelatedResource' {additionalInfo} -> additionalInfo) (\s@RelatedResource' {} a -> s {additionalInfo = a} :: RelatedResource) Prelude.. Lens.mapping Lens.coerced

-- | Information that identifies the resource.
relatedResource_resourceIdentifier :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceIdentifier)
relatedResource_resourceIdentifier = Lens.lens (\RelatedResource' {resourceIdentifier} -> resourceIdentifier) (\s@RelatedResource' {} a -> s {resourceIdentifier = a} :: RelatedResource)

-- | The type of resource.
relatedResource_resourceType :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceType)
relatedResource_resourceType = Lens.lens (\RelatedResource' {resourceType} -> resourceType) (\s@RelatedResource' {} a -> s {resourceType = a} :: RelatedResource)

instance Data.FromJSON RelatedResource where
  parseJSON =
    Data.withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            Prelude.<$> (x Data..:? "additionalInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceIdentifier")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable RelatedResource where
  hashWithSalt _salt RelatedResource' {..} =
    _salt `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData RelatedResource where
  rnf RelatedResource' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType
