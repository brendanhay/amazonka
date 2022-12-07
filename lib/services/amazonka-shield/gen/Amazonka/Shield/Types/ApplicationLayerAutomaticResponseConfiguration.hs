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
-- Module      : Amazonka.Shield.Types.ApplicationLayerAutomaticResponseConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ApplicationLayerAutomaticResponseConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ApplicationLayerAutomaticResponseStatus
import Amazonka.Shield.Types.ResponseAction

-- | The automatic application layer DDoS mitigation settings for a
-- Protection. This configuration determines whether Shield Advanced
-- automatically manages rules in the web ACL in order to respond to
-- application layer events that Shield Advanced determines to be DDoS
-- attacks.
--
-- /See:/ 'newApplicationLayerAutomaticResponseConfiguration' smart constructor.
data ApplicationLayerAutomaticResponseConfiguration = ApplicationLayerAutomaticResponseConfiguration'
  { -- | Indicates whether automatic application layer DDoS mitigation is enabled
    -- for the protection.
    status :: ApplicationLayerAutomaticResponseStatus,
    -- | Specifies the action setting that Shield Advanced should use in the WAF
    -- rules that it creates on behalf of the protected resource in response to
    -- DDoS attacks. You specify this as part of the configuration for the
    -- automatic application layer DDoS mitigation feature, when you enable or
    -- update automatic mitigation. Shield Advanced creates the WAF rules in a
    -- Shield Advanced-managed rule group, inside the web ACL that you have
    -- associated with the resource.
    action :: ResponseAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationLayerAutomaticResponseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'applicationLayerAutomaticResponseConfiguration_status' - Indicates whether automatic application layer DDoS mitigation is enabled
-- for the protection.
--
-- 'action', 'applicationLayerAutomaticResponseConfiguration_action' - Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
newApplicationLayerAutomaticResponseConfiguration ::
  -- | 'status'
  ApplicationLayerAutomaticResponseStatus ->
  -- | 'action'
  ResponseAction ->
  ApplicationLayerAutomaticResponseConfiguration
newApplicationLayerAutomaticResponseConfiguration
  pStatus_
  pAction_ =
    ApplicationLayerAutomaticResponseConfiguration'
      { status =
          pStatus_,
        action = pAction_
      }

-- | Indicates whether automatic application layer DDoS mitigation is enabled
-- for the protection.
applicationLayerAutomaticResponseConfiguration_status :: Lens.Lens' ApplicationLayerAutomaticResponseConfiguration ApplicationLayerAutomaticResponseStatus
applicationLayerAutomaticResponseConfiguration_status = Lens.lens (\ApplicationLayerAutomaticResponseConfiguration' {status} -> status) (\s@ApplicationLayerAutomaticResponseConfiguration' {} a -> s {status = a} :: ApplicationLayerAutomaticResponseConfiguration)

-- | Specifies the action setting that Shield Advanced should use in the WAF
-- rules that it creates on behalf of the protected resource in response to
-- DDoS attacks. You specify this as part of the configuration for the
-- automatic application layer DDoS mitigation feature, when you enable or
-- update automatic mitigation. Shield Advanced creates the WAF rules in a
-- Shield Advanced-managed rule group, inside the web ACL that you have
-- associated with the resource.
applicationLayerAutomaticResponseConfiguration_action :: Lens.Lens' ApplicationLayerAutomaticResponseConfiguration ResponseAction
applicationLayerAutomaticResponseConfiguration_action = Lens.lens (\ApplicationLayerAutomaticResponseConfiguration' {action} -> action) (\s@ApplicationLayerAutomaticResponseConfiguration' {} a -> s {action = a} :: ApplicationLayerAutomaticResponseConfiguration)

instance
  Data.FromJSON
    ApplicationLayerAutomaticResponseConfiguration
  where
  parseJSON =
    Data.withObject
      "ApplicationLayerAutomaticResponseConfiguration"
      ( \x ->
          ApplicationLayerAutomaticResponseConfiguration'
            Prelude.<$> (x Data..: "Status")
              Prelude.<*> (x Data..: "Action")
      )

instance
  Prelude.Hashable
    ApplicationLayerAutomaticResponseConfiguration
  where
  hashWithSalt
    _salt
    ApplicationLayerAutomaticResponseConfiguration' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    ApplicationLayerAutomaticResponseConfiguration
  where
  rnf
    ApplicationLayerAutomaticResponseConfiguration' {..} =
      Prelude.rnf status `Prelude.seq` Prelude.rnf action
