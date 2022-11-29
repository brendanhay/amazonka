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
-- Module      : Amazonka.ResourceExplorer2.Types.ResourceProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.ResourceProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceExplorer2.Types.Document

-- | A structure that describes a property of a resource.
--
-- /See:/ 'newResourceProperty' smart constructor.
data ResourceProperty = ResourceProperty'
  { -- | The name of this property of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the information about this resource property was
    -- last updated.
    lastReportedAt :: Prelude.Maybe Core.POSIX,
    -- | Details about this property. The content of this field is a JSON object
    -- that varies based on the resource type.
    data' :: Prelude.Maybe Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceProperty_name' - The name of this property of the resource.
--
-- 'lastReportedAt', 'resourceProperty_lastReportedAt' - The date and time that the information about this resource property was
-- last updated.
--
-- 'data'', 'resourceProperty_data' - Details about this property. The content of this field is a JSON object
-- that varies based on the resource type.
newResourceProperty ::
  ResourceProperty
newResourceProperty =
  ResourceProperty'
    { name = Prelude.Nothing,
      lastReportedAt = Prelude.Nothing,
      data' = Prelude.Nothing
    }

-- | The name of this property of the resource.
resourceProperty_name :: Lens.Lens' ResourceProperty (Prelude.Maybe Prelude.Text)
resourceProperty_name = Lens.lens (\ResourceProperty' {name} -> name) (\s@ResourceProperty' {} a -> s {name = a} :: ResourceProperty)

-- | The date and time that the information about this resource property was
-- last updated.
resourceProperty_lastReportedAt :: Lens.Lens' ResourceProperty (Prelude.Maybe Prelude.UTCTime)
resourceProperty_lastReportedAt = Lens.lens (\ResourceProperty' {lastReportedAt} -> lastReportedAt) (\s@ResourceProperty' {} a -> s {lastReportedAt = a} :: ResourceProperty) Prelude.. Lens.mapping Core._Time

-- | Details about this property. The content of this field is a JSON object
-- that varies based on the resource type.
resourceProperty_data :: Lens.Lens' ResourceProperty (Prelude.Maybe Document)
resourceProperty_data = Lens.lens (\ResourceProperty' {data'} -> data') (\s@ResourceProperty' {} a -> s {data' = a} :: ResourceProperty)

instance Core.FromJSON ResourceProperty where
  parseJSON =
    Core.withObject
      "ResourceProperty"
      ( \x ->
          ResourceProperty'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastReportedAt")
            Prelude.<*> (x Core..:? "Data")
      )

instance Prelude.Hashable ResourceProperty where
  hashWithSalt _salt ResourceProperty' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastReportedAt
      `Prelude.hashWithSalt` data'

instance Prelude.NFData ResourceProperty where
  rnf ResourceProperty' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastReportedAt
      `Prelude.seq` Prelude.rnf data'
