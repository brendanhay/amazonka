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
-- Module      : Amazonka.EMR.Types.SecurityConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SecurityConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The creation date and time, and name, of a security configuration.
--
-- /See:/ 'newSecurityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
  { -- | The date and time the security configuration was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'securityConfigurationSummary_creationDateTime' - The date and time the security configuration was created.
--
-- 'name', 'securityConfigurationSummary_name' - The name of the security configuration.
newSecurityConfigurationSummary ::
  SecurityConfigurationSummary
newSecurityConfigurationSummary =
  SecurityConfigurationSummary'
    { creationDateTime =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The date and time the security configuration was created.
securityConfigurationSummary_creationDateTime :: Lens.Lens' SecurityConfigurationSummary (Prelude.Maybe Prelude.UTCTime)
securityConfigurationSummary_creationDateTime = Lens.lens (\SecurityConfigurationSummary' {creationDateTime} -> creationDateTime) (\s@SecurityConfigurationSummary' {} a -> s {creationDateTime = a} :: SecurityConfigurationSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the security configuration.
securityConfigurationSummary_name :: Lens.Lens' SecurityConfigurationSummary (Prelude.Maybe Prelude.Text)
securityConfigurationSummary_name = Lens.lens (\SecurityConfigurationSummary' {name} -> name) (\s@SecurityConfigurationSummary' {} a -> s {name = a} :: SecurityConfigurationSummary)

instance Data.FromJSON SecurityConfigurationSummary where
  parseJSON =
    Data.withObject
      "SecurityConfigurationSummary"
      ( \x ->
          SecurityConfigurationSummary'
            Prelude.<$> (x Data..:? "CreationDateTime")
            Prelude.<*> (x Data..:? "Name")
      )

instance
  Prelude.Hashable
    SecurityConfigurationSummary
  where
  hashWithSalt _salt SecurityConfigurationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData SecurityConfigurationSummary where
  rnf SecurityConfigurationSummary' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf name
