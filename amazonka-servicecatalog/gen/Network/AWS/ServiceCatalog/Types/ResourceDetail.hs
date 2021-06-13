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
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a resource.
--
-- /See:/ 'newResourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { -- | The identifier of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the resource.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'resourceDetail_id' - The identifier of the resource.
--
-- 'arn', 'resourceDetail_arn' - The ARN of the resource.
--
-- 'createdTime', 'resourceDetail_createdTime' - The creation time of the resource.
--
-- 'name', 'resourceDetail_name' - The name of the resource.
--
-- 'description', 'resourceDetail_description' - The description of the resource.
newResourceDetail ::
  ResourceDetail
newResourceDetail =
  ResourceDetail'
    { id = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The identifier of the resource.
resourceDetail_id :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_id = Lens.lens (\ResourceDetail' {id} -> id) (\s@ResourceDetail' {} a -> s {id = a} :: ResourceDetail)

-- | The ARN of the resource.
resourceDetail_arn :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_arn = Lens.lens (\ResourceDetail' {arn} -> arn) (\s@ResourceDetail' {} a -> s {arn = a} :: ResourceDetail)

-- | The creation time of the resource.
resourceDetail_createdTime :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.UTCTime)
resourceDetail_createdTime = Lens.lens (\ResourceDetail' {createdTime} -> createdTime) (\s@ResourceDetail' {} a -> s {createdTime = a} :: ResourceDetail) Prelude.. Lens.mapping Core._Time

-- | The name of the resource.
resourceDetail_name :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_name = Lens.lens (\ResourceDetail' {name} -> name) (\s@ResourceDetail' {} a -> s {name = a} :: ResourceDetail)

-- | The description of the resource.
resourceDetail_description :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_description = Lens.lens (\ResourceDetail' {description} -> description) (\s@ResourceDetail' {} a -> s {description = a} :: ResourceDetail)

instance Core.FromJSON ResourceDetail where
  parseJSON =
    Core.withObject
      "ResourceDetail"
      ( \x ->
          ResourceDetail'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "ARN")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ResourceDetail

instance Prelude.NFData ResourceDetail
