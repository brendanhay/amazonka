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
-- Module      : Amazonka.BackupGateway.Types.VmwareToAwsTagMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.VmwareToAwsTagMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This displays the mapping of on-premises VMware tags to the
-- corresponding Amazon Web Services tags.
--
-- /See:/ 'newVmwareToAwsTagMapping' smart constructor.
data VmwareToAwsTagMapping = VmwareToAwsTagMapping'
  { -- | The key part of the Amazon Web Services tag\'s key-value pair.
    awsTagKey :: Prelude.Text,
    -- | The value part of the Amazon Web Services tag\'s key-value pair.
    awsTagValue :: Prelude.Text,
    -- | The is the category of VMware.
    vmwareCategory :: Prelude.Text,
    -- | This is the user-defined name of a VMware tag.
    vmwareTagName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VmwareToAwsTagMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsTagKey', 'vmwareToAwsTagMapping_awsTagKey' - The key part of the Amazon Web Services tag\'s key-value pair.
--
-- 'awsTagValue', 'vmwareToAwsTagMapping_awsTagValue' - The value part of the Amazon Web Services tag\'s key-value pair.
--
-- 'vmwareCategory', 'vmwareToAwsTagMapping_vmwareCategory' - The is the category of VMware.
--
-- 'vmwareTagName', 'vmwareToAwsTagMapping_vmwareTagName' - This is the user-defined name of a VMware tag.
newVmwareToAwsTagMapping ::
  -- | 'awsTagKey'
  Prelude.Text ->
  -- | 'awsTagValue'
  Prelude.Text ->
  -- | 'vmwareCategory'
  Prelude.Text ->
  -- | 'vmwareTagName'
  Prelude.Text ->
  VmwareToAwsTagMapping
newVmwareToAwsTagMapping
  pAwsTagKey_
  pAwsTagValue_
  pVmwareCategory_
  pVmwareTagName_ =
    VmwareToAwsTagMapping'
      { awsTagKey = pAwsTagKey_,
        awsTagValue = pAwsTagValue_,
        vmwareCategory = pVmwareCategory_,
        vmwareTagName = pVmwareTagName_
      }

-- | The key part of the Amazon Web Services tag\'s key-value pair.
vmwareToAwsTagMapping_awsTagKey :: Lens.Lens' VmwareToAwsTagMapping Prelude.Text
vmwareToAwsTagMapping_awsTagKey = Lens.lens (\VmwareToAwsTagMapping' {awsTagKey} -> awsTagKey) (\s@VmwareToAwsTagMapping' {} a -> s {awsTagKey = a} :: VmwareToAwsTagMapping)

-- | The value part of the Amazon Web Services tag\'s key-value pair.
vmwareToAwsTagMapping_awsTagValue :: Lens.Lens' VmwareToAwsTagMapping Prelude.Text
vmwareToAwsTagMapping_awsTagValue = Lens.lens (\VmwareToAwsTagMapping' {awsTagValue} -> awsTagValue) (\s@VmwareToAwsTagMapping' {} a -> s {awsTagValue = a} :: VmwareToAwsTagMapping)

-- | The is the category of VMware.
vmwareToAwsTagMapping_vmwareCategory :: Lens.Lens' VmwareToAwsTagMapping Prelude.Text
vmwareToAwsTagMapping_vmwareCategory = Lens.lens (\VmwareToAwsTagMapping' {vmwareCategory} -> vmwareCategory) (\s@VmwareToAwsTagMapping' {} a -> s {vmwareCategory = a} :: VmwareToAwsTagMapping)

-- | This is the user-defined name of a VMware tag.
vmwareToAwsTagMapping_vmwareTagName :: Lens.Lens' VmwareToAwsTagMapping Prelude.Text
vmwareToAwsTagMapping_vmwareTagName = Lens.lens (\VmwareToAwsTagMapping' {vmwareTagName} -> vmwareTagName) (\s@VmwareToAwsTagMapping' {} a -> s {vmwareTagName = a} :: VmwareToAwsTagMapping)

instance Data.FromJSON VmwareToAwsTagMapping where
  parseJSON =
    Data.withObject
      "VmwareToAwsTagMapping"
      ( \x ->
          VmwareToAwsTagMapping'
            Prelude.<$> (x Data..: "AwsTagKey")
            Prelude.<*> (x Data..: "AwsTagValue")
            Prelude.<*> (x Data..: "VmwareCategory")
            Prelude.<*> (x Data..: "VmwareTagName")
      )

instance Prelude.Hashable VmwareToAwsTagMapping where
  hashWithSalt _salt VmwareToAwsTagMapping' {..} =
    _salt
      `Prelude.hashWithSalt` awsTagKey
      `Prelude.hashWithSalt` awsTagValue
      `Prelude.hashWithSalt` vmwareCategory
      `Prelude.hashWithSalt` vmwareTagName

instance Prelude.NFData VmwareToAwsTagMapping where
  rnf VmwareToAwsTagMapping' {..} =
    Prelude.rnf awsTagKey
      `Prelude.seq` Prelude.rnf awsTagValue
      `Prelude.seq` Prelude.rnf vmwareCategory
      `Prelude.seq` Prelude.rnf vmwareTagName

instance Data.ToJSON VmwareToAwsTagMapping where
  toJSON VmwareToAwsTagMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AwsTagKey" Data..= awsTagKey),
            Prelude.Just ("AwsTagValue" Data..= awsTagValue),
            Prelude.Just
              ("VmwareCategory" Data..= vmwareCategory),
            Prelude.Just
              ("VmwareTagName" Data..= vmwareTagName)
          ]
      )
