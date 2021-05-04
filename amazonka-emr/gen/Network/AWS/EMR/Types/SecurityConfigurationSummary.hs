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
-- Module      : Network.AWS.EMR.Types.SecurityConfigurationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SecurityConfigurationSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The creation date and time, and name, of a security configuration.
--
-- /See:/ 'newSecurityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
  { -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time the security configuration was created.
    creationDateTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'securityConfigurationSummary_name' - The name of the security configuration.
--
-- 'creationDateTime', 'securityConfigurationSummary_creationDateTime' - The date and time the security configuration was created.
newSecurityConfigurationSummary ::
  SecurityConfigurationSummary
newSecurityConfigurationSummary =
  SecurityConfigurationSummary'
    { name =
        Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | The name of the security configuration.
securityConfigurationSummary_name :: Lens.Lens' SecurityConfigurationSummary (Prelude.Maybe Prelude.Text)
securityConfigurationSummary_name = Lens.lens (\SecurityConfigurationSummary' {name} -> name) (\s@SecurityConfigurationSummary' {} a -> s {name = a} :: SecurityConfigurationSummary)

-- | The date and time the security configuration was created.
securityConfigurationSummary_creationDateTime :: Lens.Lens' SecurityConfigurationSummary (Prelude.Maybe Prelude.UTCTime)
securityConfigurationSummary_creationDateTime = Lens.lens (\SecurityConfigurationSummary' {creationDateTime} -> creationDateTime) (\s@SecurityConfigurationSummary' {} a -> s {creationDateTime = a} :: SecurityConfigurationSummary) Prelude.. Lens.mapping Prelude._Time

instance
  Prelude.FromJSON
    SecurityConfigurationSummary
  where
  parseJSON =
    Prelude.withObject
      "SecurityConfigurationSummary"
      ( \x ->
          SecurityConfigurationSummary'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "CreationDateTime")
      )

instance
  Prelude.Hashable
    SecurityConfigurationSummary

instance Prelude.NFData SecurityConfigurationSummary
