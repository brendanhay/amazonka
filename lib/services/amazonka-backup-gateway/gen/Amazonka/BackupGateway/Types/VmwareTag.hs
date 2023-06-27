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
-- Module      : Amazonka.BackupGateway.Types.VmwareTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.VmwareTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A VMware tag is a tag attached to a specific virtual machine. A
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_Tag.html tag>
-- is a key-value pair you can use to manage, filter, and search for your
-- resources.
--
-- The content of VMware tags can be matched to Amazon Web Services tags.
--
-- /See:/ 'newVmwareTag' smart constructor.
data VmwareTag = VmwareTag'
  { -- | The is the category of VMware.
    vmwareCategory :: Prelude.Maybe Prelude.Text,
    -- | This is a user-defined description of a VMware tag.
    vmwareTagDescription :: Prelude.Maybe Prelude.Text,
    -- | This is the user-defined name of a VMware tag.
    vmwareTagName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VmwareTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vmwareCategory', 'vmwareTag_vmwareCategory' - The is the category of VMware.
--
-- 'vmwareTagDescription', 'vmwareTag_vmwareTagDescription' - This is a user-defined description of a VMware tag.
--
-- 'vmwareTagName', 'vmwareTag_vmwareTagName' - This is the user-defined name of a VMware tag.
newVmwareTag ::
  VmwareTag
newVmwareTag =
  VmwareTag'
    { vmwareCategory = Prelude.Nothing,
      vmwareTagDescription = Prelude.Nothing,
      vmwareTagName = Prelude.Nothing
    }

-- | The is the category of VMware.
vmwareTag_vmwareCategory :: Lens.Lens' VmwareTag (Prelude.Maybe Prelude.Text)
vmwareTag_vmwareCategory = Lens.lens (\VmwareTag' {vmwareCategory} -> vmwareCategory) (\s@VmwareTag' {} a -> s {vmwareCategory = a} :: VmwareTag)

-- | This is a user-defined description of a VMware tag.
vmwareTag_vmwareTagDescription :: Lens.Lens' VmwareTag (Prelude.Maybe Prelude.Text)
vmwareTag_vmwareTagDescription = Lens.lens (\VmwareTag' {vmwareTagDescription} -> vmwareTagDescription) (\s@VmwareTag' {} a -> s {vmwareTagDescription = a} :: VmwareTag)

-- | This is the user-defined name of a VMware tag.
vmwareTag_vmwareTagName :: Lens.Lens' VmwareTag (Prelude.Maybe Prelude.Text)
vmwareTag_vmwareTagName = Lens.lens (\VmwareTag' {vmwareTagName} -> vmwareTagName) (\s@VmwareTag' {} a -> s {vmwareTagName = a} :: VmwareTag)

instance Data.FromJSON VmwareTag where
  parseJSON =
    Data.withObject
      "VmwareTag"
      ( \x ->
          VmwareTag'
            Prelude.<$> (x Data..:? "VmwareCategory")
            Prelude.<*> (x Data..:? "VmwareTagDescription")
            Prelude.<*> (x Data..:? "VmwareTagName")
      )

instance Prelude.Hashable VmwareTag where
  hashWithSalt _salt VmwareTag' {..} =
    _salt
      `Prelude.hashWithSalt` vmwareCategory
      `Prelude.hashWithSalt` vmwareTagDescription
      `Prelude.hashWithSalt` vmwareTagName

instance Prelude.NFData VmwareTag where
  rnf VmwareTag' {..} =
    Prelude.rnf vmwareCategory
      `Prelude.seq` Prelude.rnf vmwareTagDescription
      `Prelude.seq` Prelude.rnf vmwareTagName
