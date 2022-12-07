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
-- Module      : Amazonka.SSM.Types.PatchSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the patches to use to update the managed nodes,
-- including target operating systems and source repository. Applies to
-- Linux managed nodes only.
--
-- /See:/ 'newPatchSource' smart constructor.
data PatchSource = PatchSource'
  { -- | The name specified to identify the patch source.
    name :: Prelude.Text,
    -- | The specific operating system versions a patch repository applies to,
    -- such as \"Ubuntu16.04\", \"AmazonLinux2016.09\",
    -- \"RedhatEnterpriseLinux7.2\" or \"Suse12.7\". For lists of supported
    -- product values, see PatchFilter.
    products :: Prelude.NonEmpty Prelude.Text,
    -- | The value of the yum repo configuration. For example:
    --
    -- @[main]@
    --
    -- @name=MyCustomRepository@
    --
    -- @baseurl=https:\/\/my-custom-repository@
    --
    -- @enabled=1@
    --
    -- For information about other options available for your yum repository
    -- configuration, see
    -- <https://man7.org/linux/man-pages/man5/dnf.conf.5.html dnf.conf(5)>.
    configuration :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'patchSource_name' - The name specified to identify the patch source.
--
-- 'products', 'patchSource_products' - The specific operating system versions a patch repository applies to,
-- such as \"Ubuntu16.04\", \"AmazonLinux2016.09\",
-- \"RedhatEnterpriseLinux7.2\" or \"Suse12.7\". For lists of supported
-- product values, see PatchFilter.
--
-- 'configuration', 'patchSource_configuration' - The value of the yum repo configuration. For example:
--
-- @[main]@
--
-- @name=MyCustomRepository@
--
-- @baseurl=https:\/\/my-custom-repository@
--
-- @enabled=1@
--
-- For information about other options available for your yum repository
-- configuration, see
-- <https://man7.org/linux/man-pages/man5/dnf.conf.5.html dnf.conf(5)>.
newPatchSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'products'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'configuration'
  Prelude.Text ->
  PatchSource
newPatchSource pName_ pProducts_ pConfiguration_ =
  PatchSource'
    { name = pName_,
      products = Lens.coerced Lens.# pProducts_,
      configuration =
        Data._Sensitive Lens.# pConfiguration_
    }

-- | The name specified to identify the patch source.
patchSource_name :: Lens.Lens' PatchSource Prelude.Text
patchSource_name = Lens.lens (\PatchSource' {name} -> name) (\s@PatchSource' {} a -> s {name = a} :: PatchSource)

-- | The specific operating system versions a patch repository applies to,
-- such as \"Ubuntu16.04\", \"AmazonLinux2016.09\",
-- \"RedhatEnterpriseLinux7.2\" or \"Suse12.7\". For lists of supported
-- product values, see PatchFilter.
patchSource_products :: Lens.Lens' PatchSource (Prelude.NonEmpty Prelude.Text)
patchSource_products = Lens.lens (\PatchSource' {products} -> products) (\s@PatchSource' {} a -> s {products = a} :: PatchSource) Prelude.. Lens.coerced

-- | The value of the yum repo configuration. For example:
--
-- @[main]@
--
-- @name=MyCustomRepository@
--
-- @baseurl=https:\/\/my-custom-repository@
--
-- @enabled=1@
--
-- For information about other options available for your yum repository
-- configuration, see
-- <https://man7.org/linux/man-pages/man5/dnf.conf.5.html dnf.conf(5)>.
patchSource_configuration :: Lens.Lens' PatchSource Prelude.Text
patchSource_configuration = Lens.lens (\PatchSource' {configuration} -> configuration) (\s@PatchSource' {} a -> s {configuration = a} :: PatchSource) Prelude.. Data._Sensitive

instance Data.FromJSON PatchSource where
  parseJSON =
    Data.withObject
      "PatchSource"
      ( \x ->
          PatchSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Products")
            Prelude.<*> (x Data..: "Configuration")
      )

instance Prelude.Hashable PatchSource where
  hashWithSalt _salt PatchSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` products
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData PatchSource where
  rnf PatchSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf products
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToJSON PatchSource where
  toJSON PatchSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Products" Data..= products),
            Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )
