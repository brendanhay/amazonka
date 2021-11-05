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
-- Module      : Network.AWS.Grafana.Types.SamlAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Types.SamlAuthentication where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types.SamlConfiguration
import Network.AWS.Grafana.Types.SamlConfigurationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing information about how this workspace works with
-- SAML.
--
-- /See:/ 'newSamlAuthentication' smart constructor.
data SamlAuthentication = SamlAuthentication'
  { -- | A structure containing details about how this workspace works with SAML.
    configuration :: Prelude.Maybe SamlConfiguration,
    -- | Specifies whether the workspace\'s SAML configuration is complete.
    status :: SamlConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamlAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'samlAuthentication_configuration' - A structure containing details about how this workspace works with SAML.
--
-- 'status', 'samlAuthentication_status' - Specifies whether the workspace\'s SAML configuration is complete.
newSamlAuthentication ::
  -- | 'status'
  SamlConfigurationStatus ->
  SamlAuthentication
newSamlAuthentication pStatus_ =
  SamlAuthentication'
    { configuration =
        Prelude.Nothing,
      status = pStatus_
    }

-- | A structure containing details about how this workspace works with SAML.
samlAuthentication_configuration :: Lens.Lens' SamlAuthentication (Prelude.Maybe SamlConfiguration)
samlAuthentication_configuration = Lens.lens (\SamlAuthentication' {configuration} -> configuration) (\s@SamlAuthentication' {} a -> s {configuration = a} :: SamlAuthentication)

-- | Specifies whether the workspace\'s SAML configuration is complete.
samlAuthentication_status :: Lens.Lens' SamlAuthentication SamlConfigurationStatus
samlAuthentication_status = Lens.lens (\SamlAuthentication' {status} -> status) (\s@SamlAuthentication' {} a -> s {status = a} :: SamlAuthentication)

instance Core.FromJSON SamlAuthentication where
  parseJSON =
    Core.withObject
      "SamlAuthentication"
      ( \x ->
          SamlAuthentication'
            Prelude.<$> (x Core..:? "configuration")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable SamlAuthentication

instance Prelude.NFData SamlAuthentication
