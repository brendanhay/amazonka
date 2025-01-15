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
-- Module      : Amazonka.Glue.Types.ResourceUri
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ResourceUri where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | The URIs for function resources.
--
-- /See:/ 'newResourceUri' smart constructor.
data ResourceUri = ResourceUri'
  { -- | The type of the resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The URI for accessing the resource.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceUri' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceUri_resourceType' - The type of the resource.
--
-- 'uri', 'resourceUri_uri' - The URI for accessing the resource.
newResourceUri ::
  ResourceUri
newResourceUri =
  ResourceUri'
    { resourceType = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | The type of the resource.
resourceUri_resourceType :: Lens.Lens' ResourceUri (Prelude.Maybe ResourceType)
resourceUri_resourceType = Lens.lens (\ResourceUri' {resourceType} -> resourceType) (\s@ResourceUri' {} a -> s {resourceType = a} :: ResourceUri)

-- | The URI for accessing the resource.
resourceUri_uri :: Lens.Lens' ResourceUri (Prelude.Maybe Prelude.Text)
resourceUri_uri = Lens.lens (\ResourceUri' {uri} -> uri) (\s@ResourceUri' {} a -> s {uri = a} :: ResourceUri)

instance Data.FromJSON ResourceUri where
  parseJSON =
    Data.withObject
      "ResourceUri"
      ( \x ->
          ResourceUri'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Uri")
      )

instance Prelude.Hashable ResourceUri where
  hashWithSalt _salt ResourceUri' {..} =
    _salt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` uri

instance Prelude.NFData ResourceUri where
  rnf ResourceUri' {..} =
    Prelude.rnf resourceType `Prelude.seq`
      Prelude.rnf uri

instance Data.ToJSON ResourceUri where
  toJSON ResourceUri' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("Uri" Data..=) Prelude.<$> uri
          ]
      )
