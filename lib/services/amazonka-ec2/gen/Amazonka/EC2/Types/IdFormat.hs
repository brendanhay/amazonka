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
-- Module      : Amazonka.EC2.Types.IdFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IdFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the ID format for a resource.
--
-- /See:/ 'newIdFormat' smart constructor.
data IdFormat = IdFormat'
  { -- | Indicates whether longer IDs (17-character IDs) are enabled for the
    -- resource.
    useLongIds :: Prelude.Maybe Prelude.Bool,
    -- | The date in UTC at which you are permanently switched over to using
    -- longer IDs. If a deadline is not yet available for this resource type,
    -- this field is not returned.
    deadline :: Prelude.Maybe Core.ISO8601,
    -- | The type of resource.
    resource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useLongIds', 'idFormat_useLongIds' - Indicates whether longer IDs (17-character IDs) are enabled for the
-- resource.
--
-- 'deadline', 'idFormat_deadline' - The date in UTC at which you are permanently switched over to using
-- longer IDs. If a deadline is not yet available for this resource type,
-- this field is not returned.
--
-- 'resource', 'idFormat_resource' - The type of resource.
newIdFormat ::
  IdFormat
newIdFormat =
  IdFormat'
    { useLongIds = Prelude.Nothing,
      deadline = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | Indicates whether longer IDs (17-character IDs) are enabled for the
-- resource.
idFormat_useLongIds :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.Bool)
idFormat_useLongIds = Lens.lens (\IdFormat' {useLongIds} -> useLongIds) (\s@IdFormat' {} a -> s {useLongIds = a} :: IdFormat)

-- | The date in UTC at which you are permanently switched over to using
-- longer IDs. If a deadline is not yet available for this resource type,
-- this field is not returned.
idFormat_deadline :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.UTCTime)
idFormat_deadline = Lens.lens (\IdFormat' {deadline} -> deadline) (\s@IdFormat' {} a -> s {deadline = a} :: IdFormat) Prelude.. Lens.mapping Core._Time

-- | The type of resource.
idFormat_resource :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.Text)
idFormat_resource = Lens.lens (\IdFormat' {resource} -> resource) (\s@IdFormat' {} a -> s {resource = a} :: IdFormat)

instance Core.FromXML IdFormat where
  parseXML x =
    IdFormat'
      Prelude.<$> (x Core..@? "useLongIds")
      Prelude.<*> (x Core..@? "deadline")
      Prelude.<*> (x Core..@? "resource")

instance Prelude.Hashable IdFormat where
  hashWithSalt _salt IdFormat' {..} =
    _salt `Prelude.hashWithSalt` useLongIds
      `Prelude.hashWithSalt` deadline
      `Prelude.hashWithSalt` resource

instance Prelude.NFData IdFormat where
  rnf IdFormat' {..} =
    Prelude.rnf useLongIds
      `Prelude.seq` Prelude.rnf deadline
      `Prelude.seq` Prelude.rnf resource
