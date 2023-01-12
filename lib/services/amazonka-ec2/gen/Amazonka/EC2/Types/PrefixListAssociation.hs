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
-- Module      : Amazonka.EC2.Types.PrefixListAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrefixListAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the resource with which a prefix list is associated.
--
-- /See:/ 'newPrefixListAssociation' smart constructor.
data PrefixListAssociation = PrefixListAssociation'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the resource.
    resourceOwner :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrefixListAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'prefixListAssociation_resourceId' - The ID of the resource.
--
-- 'resourceOwner', 'prefixListAssociation_resourceOwner' - The owner of the resource.
newPrefixListAssociation ::
  PrefixListAssociation
newPrefixListAssociation =
  PrefixListAssociation'
    { resourceId =
        Prelude.Nothing,
      resourceOwner = Prelude.Nothing
    }

-- | The ID of the resource.
prefixListAssociation_resourceId :: Lens.Lens' PrefixListAssociation (Prelude.Maybe Prelude.Text)
prefixListAssociation_resourceId = Lens.lens (\PrefixListAssociation' {resourceId} -> resourceId) (\s@PrefixListAssociation' {} a -> s {resourceId = a} :: PrefixListAssociation)

-- | The owner of the resource.
prefixListAssociation_resourceOwner :: Lens.Lens' PrefixListAssociation (Prelude.Maybe Prelude.Text)
prefixListAssociation_resourceOwner = Lens.lens (\PrefixListAssociation' {resourceOwner} -> resourceOwner) (\s@PrefixListAssociation' {} a -> s {resourceOwner = a} :: PrefixListAssociation)

instance Data.FromXML PrefixListAssociation where
  parseXML x =
    PrefixListAssociation'
      Prelude.<$> (x Data..@? "resourceId")
      Prelude.<*> (x Data..@? "resourceOwner")

instance Prelude.Hashable PrefixListAssociation where
  hashWithSalt _salt PrefixListAssociation' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceOwner

instance Prelude.NFData PrefixListAssociation where
  rnf PrefixListAssociation' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceOwner
