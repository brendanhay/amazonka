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
-- Module      : Network.AWS.SES.Types.ConfigurationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The name of the configuration set.
--
-- Configuration sets let you create groups of rules that you can apply to
-- the emails you send using Amazon SES. For more information about using
-- configuration sets, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html Using Amazon SES Configuration Sets>
-- in the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon SES Developer Guide>.
--
-- /See:/ 'newConfigurationSet' smart constructor.
data ConfigurationSet = ConfigurationSet'
  { -- | The name of the configuration set. The name must meet the following
    -- requirements:
    --
    -- -   Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or
    --     dashes (-).
    --
    -- -   Contain 64 characters or fewer.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'configurationSet_name' - The name of the configuration set. The name must meet the following
-- requirements:
--
-- -   Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or
--     dashes (-).
--
-- -   Contain 64 characters or fewer.
newConfigurationSet ::
  -- | 'name'
  Prelude.Text ->
  ConfigurationSet
newConfigurationSet pName_ =
  ConfigurationSet' {name = pName_}

-- | The name of the configuration set. The name must meet the following
-- requirements:
--
-- -   Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or
--     dashes (-).
--
-- -   Contain 64 characters or fewer.
configurationSet_name :: Lens.Lens' ConfigurationSet Prelude.Text
configurationSet_name = Lens.lens (\ConfigurationSet' {name} -> name) (\s@ConfigurationSet' {} a -> s {name = a} :: ConfigurationSet)

instance Prelude.FromXML ConfigurationSet where
  parseXML x =
    ConfigurationSet' Prelude.<$> (x Prelude..@ "Name")

instance Prelude.Hashable ConfigurationSet

instance Prelude.NFData ConfigurationSet

instance Prelude.ToQuery ConfigurationSet where
  toQuery ConfigurationSet' {..} =
    Prelude.mconcat ["Name" Prelude.=: name]
