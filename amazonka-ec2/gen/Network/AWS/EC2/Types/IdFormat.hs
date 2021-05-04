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
-- Module      : Network.AWS.EC2.Types.IdFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IdFormat where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the ID format for a resource.
--
-- /See:/ 'newIdFormat' smart constructor.
data IdFormat = IdFormat'
  { -- | Indicates whether longer IDs (17-character IDs) are enabled for the
    -- resource.
    useLongIds :: Prelude.Maybe Prelude.Bool,
    -- | The type of resource.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The date in UTC at which you are permanently switched over to using
    -- longer IDs. If a deadline is not yet available for this resource type,
    -- this field is not returned.
    deadline :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'resource', 'idFormat_resource' - The type of resource.
--
-- 'deadline', 'idFormat_deadline' - The date in UTC at which you are permanently switched over to using
-- longer IDs. If a deadline is not yet available for this resource type,
-- this field is not returned.
newIdFormat ::
  IdFormat
newIdFormat =
  IdFormat'
    { useLongIds = Prelude.Nothing,
      resource = Prelude.Nothing,
      deadline = Prelude.Nothing
    }

-- | Indicates whether longer IDs (17-character IDs) are enabled for the
-- resource.
idFormat_useLongIds :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.Bool)
idFormat_useLongIds = Lens.lens (\IdFormat' {useLongIds} -> useLongIds) (\s@IdFormat' {} a -> s {useLongIds = a} :: IdFormat)

-- | The type of resource.
idFormat_resource :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.Text)
idFormat_resource = Lens.lens (\IdFormat' {resource} -> resource) (\s@IdFormat' {} a -> s {resource = a} :: IdFormat)

-- | The date in UTC at which you are permanently switched over to using
-- longer IDs. If a deadline is not yet available for this resource type,
-- this field is not returned.
idFormat_deadline :: Lens.Lens' IdFormat (Prelude.Maybe Prelude.UTCTime)
idFormat_deadline = Lens.lens (\IdFormat' {deadline} -> deadline) (\s@IdFormat' {} a -> s {deadline = a} :: IdFormat) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML IdFormat where
  parseXML x =
    IdFormat'
      Prelude.<$> (x Prelude..@? "useLongIds")
      Prelude.<*> (x Prelude..@? "resource")
      Prelude.<*> (x Prelude..@? "deadline")

instance Prelude.Hashable IdFormat

instance Prelude.NFData IdFormat
