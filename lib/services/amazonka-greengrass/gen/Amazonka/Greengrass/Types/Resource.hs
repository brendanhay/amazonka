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
-- Module      : Amazonka.Greengrass.Types.Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.ResourceDataContainer
import qualified Amazonka.Prelude as Prelude

-- | Information about a resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | A container of data for all resource types.
    resourceDataContainer :: ResourceDataContainer,
    -- | The resource ID, used to refer to a resource in the Lambda function
    -- configuration. Max length is 128 characters with pattern
    -- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
    id :: Prelude.Text,
    -- | The descriptive resource name, which is displayed on the AWS IoT
    -- Greengrass console. Max length 128 characters with pattern
    -- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
    name :: Prelude.Text
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
-- 'resourceDataContainer', 'resource_resourceDataContainer' - A container of data for all resource types.
--
-- 'id', 'resource_id' - The resource ID, used to refer to a resource in the Lambda function
-- configuration. Max length is 128 characters with pattern
-- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
--
-- 'name', 'resource_name' - The descriptive resource name, which is displayed on the AWS IoT
-- Greengrass console. Max length 128 characters with pattern
-- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
newResource ::
  -- | 'resourceDataContainer'
  ResourceDataContainer ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Resource
newResource pResourceDataContainer_ pId_ pName_ =
  Resource'
    { resourceDataContainer =
        pResourceDataContainer_,
      id = pId_,
      name = pName_
    }

-- | A container of data for all resource types.
resource_resourceDataContainer :: Lens.Lens' Resource ResourceDataContainer
resource_resourceDataContainer = Lens.lens (\Resource' {resourceDataContainer} -> resourceDataContainer) (\s@Resource' {} a -> s {resourceDataContainer = a} :: Resource)

-- | The resource ID, used to refer to a resource in the Lambda function
-- configuration. Max length is 128 characters with pattern
-- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
resource_id :: Lens.Lens' Resource Prelude.Text
resource_id = Lens.lens (\Resource' {id} -> id) (\s@Resource' {} a -> s {id = a} :: Resource)

-- | The descriptive resource name, which is displayed on the AWS IoT
-- Greengrass console. Max length 128 characters with pattern
-- \'\'[a-zA-Z0-9:_-]+\'\'. This must be unique within a Greengrass group.
resource_name :: Lens.Lens' Resource Prelude.Text
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..: "ResourceDataContainer")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` resourceDataContainer
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf resourceDataContainer
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON Resource where
  toJSON Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResourceDataContainer"
                  Data..= resourceDataContainer
              ),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Name" Data..= name)
          ]
      )
