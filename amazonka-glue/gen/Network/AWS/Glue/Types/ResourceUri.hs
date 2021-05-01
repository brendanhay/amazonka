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
-- Module      : Network.AWS.Glue.Types.ResourceUri
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ResourceUri where

import Network.AWS.Glue.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The URIs for function resources.
--
-- /See:/ 'newResourceUri' smart constructor.
data ResourceUri = ResourceUri'
  { -- | The URI for accessing the resource.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceUri' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'resourceUri_uri' - The URI for accessing the resource.
--
-- 'resourceType', 'resourceUri_resourceType' - The type of the resource.
newResourceUri ::
  ResourceUri
newResourceUri =
  ResourceUri'
    { uri = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The URI for accessing the resource.
resourceUri_uri :: Lens.Lens' ResourceUri (Prelude.Maybe Prelude.Text)
resourceUri_uri = Lens.lens (\ResourceUri' {uri} -> uri) (\s@ResourceUri' {} a -> s {uri = a} :: ResourceUri)

-- | The type of the resource.
resourceUri_resourceType :: Lens.Lens' ResourceUri (Prelude.Maybe ResourceType)
resourceUri_resourceType = Lens.lens (\ResourceUri' {resourceType} -> resourceType) (\s@ResourceUri' {} a -> s {resourceType = a} :: ResourceUri)

instance Prelude.FromJSON ResourceUri where
  parseJSON =
    Prelude.withObject
      "ResourceUri"
      ( \x ->
          ResourceUri'
            Prelude.<$> (x Prelude..:? "Uri")
            Prelude.<*> (x Prelude..:? "ResourceType")
      )

instance Prelude.Hashable ResourceUri

instance Prelude.NFData ResourceUri

instance Prelude.ToJSON ResourceUri where
  toJSON ResourceUri' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Uri" Prelude..=) Prelude.<$> uri,
            ("ResourceType" Prelude..=)
              Prelude.<$> resourceType
          ]
      )
