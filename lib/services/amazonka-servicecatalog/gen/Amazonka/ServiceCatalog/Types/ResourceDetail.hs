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
-- Module      : Amazonka.ServiceCatalog.Types.ResourceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ResourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a resource.
--
-- /See:/ 'newResourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the resource.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'resourceDetail_arn' - The ARN of the resource.
--
-- 'createdTime', 'resourceDetail_createdTime' - The creation time of the resource.
--
-- 'description', 'resourceDetail_description' - The description of the resource.
--
-- 'id', 'resourceDetail_id' - The identifier of the resource.
--
-- 'name', 'resourceDetail_name' - The name of the resource.
newResourceDetail ::
  ResourceDetail
newResourceDetail =
  ResourceDetail'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the resource.
resourceDetail_arn :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_arn = Lens.lens (\ResourceDetail' {arn} -> arn) (\s@ResourceDetail' {} a -> s {arn = a} :: ResourceDetail)

-- | The creation time of the resource.
resourceDetail_createdTime :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.UTCTime)
resourceDetail_createdTime = Lens.lens (\ResourceDetail' {createdTime} -> createdTime) (\s@ResourceDetail' {} a -> s {createdTime = a} :: ResourceDetail) Prelude.. Lens.mapping Data._Time

-- | The description of the resource.
resourceDetail_description :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_description = Lens.lens (\ResourceDetail' {description} -> description) (\s@ResourceDetail' {} a -> s {description = a} :: ResourceDetail)

-- | The identifier of the resource.
resourceDetail_id :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_id = Lens.lens (\ResourceDetail' {id} -> id) (\s@ResourceDetail' {} a -> s {id = a} :: ResourceDetail)

-- | The name of the resource.
resourceDetail_name :: Lens.Lens' ResourceDetail (Prelude.Maybe Prelude.Text)
resourceDetail_name = Lens.lens (\ResourceDetail' {name} -> name) (\s@ResourceDetail' {} a -> s {name = a} :: ResourceDetail)

instance Data.FromJSON ResourceDetail where
  parseJSON =
    Data.withObject
      "ResourceDetail"
      ( \x ->
          ResourceDetail'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ResourceDetail where
  hashWithSalt _salt ResourceDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResourceDetail where
  rnf ResourceDetail' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf name
