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
-- Module      : Network.AWS.RDS.Types.OptionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The version for an option. Option group option versions are returned by
-- the @DescribeOptionGroupOptions@ action.
--
-- /See:/ 'newOptionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { -- | True if the version is the default version of the option, and otherwise
    -- false.
    isDefault :: Core.Maybe Core.Bool,
    -- | The version of the option.
    version :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OptionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefault', 'optionVersion_isDefault' - True if the version is the default version of the option, and otherwise
-- false.
--
-- 'version', 'optionVersion_version' - The version of the option.
newOptionVersion ::
  OptionVersion
newOptionVersion =
  OptionVersion'
    { isDefault = Core.Nothing,
      version = Core.Nothing
    }

-- | True if the version is the default version of the option, and otherwise
-- false.
optionVersion_isDefault :: Lens.Lens' OptionVersion (Core.Maybe Core.Bool)
optionVersion_isDefault = Lens.lens (\OptionVersion' {isDefault} -> isDefault) (\s@OptionVersion' {} a -> s {isDefault = a} :: OptionVersion)

-- | The version of the option.
optionVersion_version :: Lens.Lens' OptionVersion (Core.Maybe Core.Text)
optionVersion_version = Lens.lens (\OptionVersion' {version} -> version) (\s@OptionVersion' {} a -> s {version = a} :: OptionVersion)

instance Core.FromXML OptionVersion where
  parseXML x =
    OptionVersion'
      Core.<$> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "Version")

instance Core.Hashable OptionVersion

instance Core.NFData OptionVersion
