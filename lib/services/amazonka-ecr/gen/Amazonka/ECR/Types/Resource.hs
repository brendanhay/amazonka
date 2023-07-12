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
-- Module      : Amazonka.ECR.Types.Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ResourceDetails
import qualified Amazonka.Prelude as Prelude

-- | Details about the resource involved in a finding.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | An object that contains details about the resource involved in a
    -- finding.
    details :: Prelude.Maybe ResourceDetails,
    -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of resource.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'resource_details' - An object that contains details about the resource involved in a
-- finding.
--
-- 'id', 'resource_id' - The ID of the resource.
--
-- 'tags', 'resource_tags' - The tags attached to the resource.
--
-- 'type'', 'resource_type' - The type of resource.
newResource ::
  Resource
newResource =
  Resource'
    { details = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | An object that contains details about the resource involved in a
-- finding.
resource_details :: Lens.Lens' Resource (Prelude.Maybe ResourceDetails)
resource_details = Lens.lens (\Resource' {details} -> details) (\s@Resource' {} a -> s {details = a} :: Resource)

-- | The ID of the resource.
resource_id :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_id = Lens.lens (\Resource' {id} -> id) (\s@Resource' {} a -> s {id = a} :: Resource)

-- | The tags attached to the resource.
resource_tags :: Lens.Lens' Resource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resource_tags = Lens.lens (\Resource' {tags} -> tags) (\s@Resource' {} a -> s {tags = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource.
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "details")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
