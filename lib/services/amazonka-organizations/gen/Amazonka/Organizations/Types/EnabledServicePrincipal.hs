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
-- Module      : Amazonka.Organizations.Types.EnabledServicePrincipal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.EnabledServicePrincipal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains details of a service principal that represents
-- an Amazon Web Services service that is enabled to integrate with
-- Organizations.
--
-- /See:/ 'newEnabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { -- | The date that the service principal was enabled for integration with
    -- Organizations.
    dateEnabled :: Prelude.Maybe Data.POSIX,
    -- | The name of the service principal. This is typically in the form of a
    -- URL, such as: @ @/@servicename@/@.amazonaws.com@.
    servicePrincipal :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnabledServicePrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateEnabled', 'enabledServicePrincipal_dateEnabled' - The date that the service principal was enabled for integration with
-- Organizations.
--
-- 'servicePrincipal', 'enabledServicePrincipal_servicePrincipal' - The name of the service principal. This is typically in the form of a
-- URL, such as: @ @/@servicename@/@.amazonaws.com@.
newEnabledServicePrincipal ::
  EnabledServicePrincipal
newEnabledServicePrincipal =
  EnabledServicePrincipal'
    { dateEnabled =
        Prelude.Nothing,
      servicePrincipal = Prelude.Nothing
    }

-- | The date that the service principal was enabled for integration with
-- Organizations.
enabledServicePrincipal_dateEnabled :: Lens.Lens' EnabledServicePrincipal (Prelude.Maybe Prelude.UTCTime)
enabledServicePrincipal_dateEnabled = Lens.lens (\EnabledServicePrincipal' {dateEnabled} -> dateEnabled) (\s@EnabledServicePrincipal' {} a -> s {dateEnabled = a} :: EnabledServicePrincipal) Prelude.. Lens.mapping Data._Time

-- | The name of the service principal. This is typically in the form of a
-- URL, such as: @ @/@servicename@/@.amazonaws.com@.
enabledServicePrincipal_servicePrincipal :: Lens.Lens' EnabledServicePrincipal (Prelude.Maybe Prelude.Text)
enabledServicePrincipal_servicePrincipal = Lens.lens (\EnabledServicePrincipal' {servicePrincipal} -> servicePrincipal) (\s@EnabledServicePrincipal' {} a -> s {servicePrincipal = a} :: EnabledServicePrincipal)

instance Data.FromJSON EnabledServicePrincipal where
  parseJSON =
    Data.withObject
      "EnabledServicePrincipal"
      ( \x ->
          EnabledServicePrincipal'
            Prelude.<$> (x Data..:? "DateEnabled")
            Prelude.<*> (x Data..:? "ServicePrincipal")
      )

instance Prelude.Hashable EnabledServicePrincipal where
  hashWithSalt _salt EnabledServicePrincipal' {..} =
    _salt
      `Prelude.hashWithSalt` dateEnabled
      `Prelude.hashWithSalt` servicePrincipal

instance Prelude.NFData EnabledServicePrincipal where
  rnf EnabledServicePrincipal' {..} =
    Prelude.rnf dateEnabled
      `Prelude.seq` Prelude.rnf servicePrincipal
