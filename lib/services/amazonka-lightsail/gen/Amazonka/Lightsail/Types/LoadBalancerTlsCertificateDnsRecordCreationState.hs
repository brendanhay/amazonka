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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationStateCode
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the state of the canonical name (CNAME) records
-- that are automatically added by Lightsail to the DNS of the domain to
-- validate domain ownership.
--
-- /See:/ 'newLoadBalancerTlsCertificateDnsRecordCreationState' smart constructor.
data LoadBalancerTlsCertificateDnsRecordCreationState = LoadBalancerTlsCertificateDnsRecordCreationState'
  { -- | The message that describes the reason for the status code.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code for the automated DNS record creation.
    --
    -- Following are the possible values:
    --
    -- -   @SUCCEEDED@ - The validation records were successfully added.
    --
    -- -   @STARTED@ - The automatic DNS record creation has started.
    --
    -- -   @FAILED@ - The validation record addition failed.
    code :: Prelude.Maybe LoadBalancerTlsCertificateDnsRecordCreationStateCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateDnsRecordCreationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'loadBalancerTlsCertificateDnsRecordCreationState_message' - The message that describes the reason for the status code.
--
-- 'code', 'loadBalancerTlsCertificateDnsRecordCreationState_code' - The status code for the automated DNS record creation.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The validation records were successfully added.
--
-- -   @STARTED@ - The automatic DNS record creation has started.
--
-- -   @FAILED@ - The validation record addition failed.
newLoadBalancerTlsCertificateDnsRecordCreationState ::
  LoadBalancerTlsCertificateDnsRecordCreationState
newLoadBalancerTlsCertificateDnsRecordCreationState =
  LoadBalancerTlsCertificateDnsRecordCreationState'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message that describes the reason for the status code.
loadBalancerTlsCertificateDnsRecordCreationState_message :: Lens.Lens' LoadBalancerTlsCertificateDnsRecordCreationState (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateDnsRecordCreationState_message = Lens.lens (\LoadBalancerTlsCertificateDnsRecordCreationState' {message} -> message) (\s@LoadBalancerTlsCertificateDnsRecordCreationState' {} a -> s {message = a} :: LoadBalancerTlsCertificateDnsRecordCreationState)

-- | The status code for the automated DNS record creation.
--
-- Following are the possible values:
--
-- -   @SUCCEEDED@ - The validation records were successfully added.
--
-- -   @STARTED@ - The automatic DNS record creation has started.
--
-- -   @FAILED@ - The validation record addition failed.
loadBalancerTlsCertificateDnsRecordCreationState_code :: Lens.Lens' LoadBalancerTlsCertificateDnsRecordCreationState (Prelude.Maybe LoadBalancerTlsCertificateDnsRecordCreationStateCode)
loadBalancerTlsCertificateDnsRecordCreationState_code = Lens.lens (\LoadBalancerTlsCertificateDnsRecordCreationState' {code} -> code) (\s@LoadBalancerTlsCertificateDnsRecordCreationState' {} a -> s {code = a} :: LoadBalancerTlsCertificateDnsRecordCreationState)

instance
  Data.FromJSON
    LoadBalancerTlsCertificateDnsRecordCreationState
  where
  parseJSON =
    Data.withObject
      "LoadBalancerTlsCertificateDnsRecordCreationState"
      ( \x ->
          LoadBalancerTlsCertificateDnsRecordCreationState'
            Prelude.<$> (x Data..:? "message")
              Prelude.<*> (x Data..:? "code")
      )

instance
  Prelude.Hashable
    LoadBalancerTlsCertificateDnsRecordCreationState
  where
  hashWithSalt
    _salt
    LoadBalancerTlsCertificateDnsRecordCreationState' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    LoadBalancerTlsCertificateDnsRecordCreationState
  where
  rnf
    LoadBalancerTlsCertificateDnsRecordCreationState' {..} =
      Prelude.rnf message `Prelude.seq` Prelude.rnf code
