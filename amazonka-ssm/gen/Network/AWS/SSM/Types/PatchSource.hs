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
-- Module      : Network.AWS.SSM.Types.PatchSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repository. Applies to Linux
-- instances only.
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
    configuration :: Core.Sensitive Prelude.Text
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
      products = Lens._Coerce Lens.# pProducts_,
      configuration =
        Core._Sensitive Lens.# pConfiguration_
    }

-- | The name specified to identify the patch source.
patchSource_name :: Lens.Lens' PatchSource Prelude.Text
patchSource_name = Lens.lens (\PatchSource' {name} -> name) (\s@PatchSource' {} a -> s {name = a} :: PatchSource)

-- | The specific operating system versions a patch repository applies to,
-- such as \"Ubuntu16.04\", \"AmazonLinux2016.09\",
-- \"RedhatEnterpriseLinux7.2\" or \"Suse12.7\". For lists of supported
-- product values, see PatchFilter.
patchSource_products :: Lens.Lens' PatchSource (Prelude.NonEmpty Prelude.Text)
patchSource_products = Lens.lens (\PatchSource' {products} -> products) (\s@PatchSource' {} a -> s {products = a} :: PatchSource) Prelude.. Lens._Coerce

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
patchSource_configuration = Lens.lens (\PatchSource' {configuration} -> configuration) (\s@PatchSource' {} a -> s {configuration = a} :: PatchSource) Prelude.. Core._Sensitive

instance Core.FromJSON PatchSource where
  parseJSON =
    Core.withObject
      "PatchSource"
      ( \x ->
          PatchSource'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Products")
            Prelude.<*> (x Core..: "Configuration")
      )

instance Prelude.Hashable PatchSource

instance Prelude.NFData PatchSource

instance Core.ToJSON PatchSource where
  toJSON PatchSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Products" Core..= products),
            Prelude.Just
              ("Configuration" Core..= configuration)
          ]
      )
