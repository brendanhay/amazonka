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
-- Module      : Amazonka.ELB.Types.SourceSecurityGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.SourceSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a source security group.
--
-- /See:/ 'newSourceSecurityGroup' smart constructor.
data SourceSecurityGroup = SourceSecurityGroup'
  { -- | The owner of the security group.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'sourceSecurityGroup_ownerAlias' - The owner of the security group.
--
-- 'groupName', 'sourceSecurityGroup_groupName' - The name of the security group.
newSourceSecurityGroup ::
  SourceSecurityGroup
newSourceSecurityGroup =
  SourceSecurityGroup'
    { ownerAlias = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The owner of the security group.
sourceSecurityGroup_ownerAlias :: Lens.Lens' SourceSecurityGroup (Prelude.Maybe Prelude.Text)
sourceSecurityGroup_ownerAlias = Lens.lens (\SourceSecurityGroup' {ownerAlias} -> ownerAlias) (\s@SourceSecurityGroup' {} a -> s {ownerAlias = a} :: SourceSecurityGroup)

-- | The name of the security group.
sourceSecurityGroup_groupName :: Lens.Lens' SourceSecurityGroup (Prelude.Maybe Prelude.Text)
sourceSecurityGroup_groupName = Lens.lens (\SourceSecurityGroup' {groupName} -> groupName) (\s@SourceSecurityGroup' {} a -> s {groupName = a} :: SourceSecurityGroup)

instance Core.FromXML SourceSecurityGroup where
  parseXML x =
    SourceSecurityGroup'
      Prelude.<$> (x Core..@? "OwnerAlias")
      Prelude.<*> (x Core..@? "GroupName")

instance Prelude.Hashable SourceSecurityGroup where
  hashWithSalt _salt SourceSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` ownerAlias
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData SourceSecurityGroup where
  rnf SourceSecurityGroup' {..} =
    Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf groupName
