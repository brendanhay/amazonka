{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.RelatedResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RelatedResource where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a related resource.
--
-- /See:/ 'newRelatedResource' smart constructor.
data RelatedResource = RelatedResource'
  { -- | Other information about the resource.
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Information that identifies the resource.
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'resourceType', 'relatedResource_resourceType' - The type of resource.
--
-- 'resourceIdentifier', 'relatedResource_resourceIdentifier' - Information that identifies the resource.
newRelatedResource ::
  RelatedResource
newRelatedResource =
  RelatedResource'
    { additionalInfo = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | Other information about the resource.
relatedResource_additionalInfo :: Lens.Lens' RelatedResource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
relatedResource_additionalInfo = Lens.lens (\RelatedResource' {additionalInfo} -> additionalInfo) (\s@RelatedResource' {} a -> s {additionalInfo = a} :: RelatedResource) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of resource.
relatedResource_resourceType :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceType)
relatedResource_resourceType = Lens.lens (\RelatedResource' {resourceType} -> resourceType) (\s@RelatedResource' {} a -> s {resourceType = a} :: RelatedResource)

-- | Information that identifies the resource.
relatedResource_resourceIdentifier :: Lens.Lens' RelatedResource (Prelude.Maybe ResourceIdentifier)
relatedResource_resourceIdentifier = Lens.lens (\RelatedResource' {resourceIdentifier} -> resourceIdentifier) (\s@RelatedResource' {} a -> s {resourceIdentifier = a} :: RelatedResource)

instance Prelude.FromJSON RelatedResource where
  parseJSON =
    Prelude.withObject
      "RelatedResource"
      ( \x ->
          RelatedResource'
            Prelude.<$> ( x Prelude..:? "additionalInfo"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "resourceIdentifier")
      )

instance Prelude.Hashable RelatedResource

instance Prelude.NFData RelatedResource
