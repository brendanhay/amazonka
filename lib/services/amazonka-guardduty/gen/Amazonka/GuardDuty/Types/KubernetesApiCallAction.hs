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
-- Module      : Amazonka.GuardDuty.Types.KubernetesApiCallAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesApiCallAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.RemoteIpDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the Kubernetes API call action described in this
-- finding.
--
-- /See:/ 'newKubernetesApiCallAction' smart constructor.
data KubernetesApiCallAction = KubernetesApiCallAction'
  { remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The Kubernetes API request URI.
    requestUri :: Prelude.Maybe Prelude.Text,
    -- | The resulting HTTP response code of the Kubernetes API call action.
    statusCode :: Prelude.Maybe Prelude.Int,
    -- | The user agent of the caller of the Kubernetes API.
    userAgent :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes API request HTTP verb.
    verb :: Prelude.Maybe Prelude.Text,
    -- | The IP of the Kubernetes API caller and the IPs of any proxies or load
    -- balancers between the caller and the API endpoint.
    sourceIps :: Prelude.Maybe [Prelude.Text],
    -- | Parameters related to the Kubernetes API call action.
    parameters :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesApiCallAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteIpDetails', 'kubernetesApiCallAction_remoteIpDetails' - Undocumented member.
--
-- 'requestUri', 'kubernetesApiCallAction_requestUri' - The Kubernetes API request URI.
--
-- 'statusCode', 'kubernetesApiCallAction_statusCode' - The resulting HTTP response code of the Kubernetes API call action.
--
-- 'userAgent', 'kubernetesApiCallAction_userAgent' - The user agent of the caller of the Kubernetes API.
--
-- 'verb', 'kubernetesApiCallAction_verb' - The Kubernetes API request HTTP verb.
--
-- 'sourceIps', 'kubernetesApiCallAction_sourceIps' - The IP of the Kubernetes API caller and the IPs of any proxies or load
-- balancers between the caller and the API endpoint.
--
-- 'parameters', 'kubernetesApiCallAction_parameters' - Parameters related to the Kubernetes API call action.
newKubernetesApiCallAction ::
  KubernetesApiCallAction
newKubernetesApiCallAction =
  KubernetesApiCallAction'
    { remoteIpDetails =
        Prelude.Nothing,
      requestUri = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      userAgent = Prelude.Nothing,
      verb = Prelude.Nothing,
      sourceIps = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | Undocumented member.
kubernetesApiCallAction_remoteIpDetails :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe RemoteIpDetails)
kubernetesApiCallAction_remoteIpDetails = Lens.lens (\KubernetesApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@KubernetesApiCallAction' {} a -> s {remoteIpDetails = a} :: KubernetesApiCallAction)

-- | The Kubernetes API request URI.
kubernetesApiCallAction_requestUri :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_requestUri = Lens.lens (\KubernetesApiCallAction' {requestUri} -> requestUri) (\s@KubernetesApiCallAction' {} a -> s {requestUri = a} :: KubernetesApiCallAction)

-- | The resulting HTTP response code of the Kubernetes API call action.
kubernetesApiCallAction_statusCode :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Int)
kubernetesApiCallAction_statusCode = Lens.lens (\KubernetesApiCallAction' {statusCode} -> statusCode) (\s@KubernetesApiCallAction' {} a -> s {statusCode = a} :: KubernetesApiCallAction)

-- | The user agent of the caller of the Kubernetes API.
kubernetesApiCallAction_userAgent :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_userAgent = Lens.lens (\KubernetesApiCallAction' {userAgent} -> userAgent) (\s@KubernetesApiCallAction' {} a -> s {userAgent = a} :: KubernetesApiCallAction)

-- | The Kubernetes API request HTTP verb.
kubernetesApiCallAction_verb :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_verb = Lens.lens (\KubernetesApiCallAction' {verb} -> verb) (\s@KubernetesApiCallAction' {} a -> s {verb = a} :: KubernetesApiCallAction)

-- | The IP of the Kubernetes API caller and the IPs of any proxies or load
-- balancers between the caller and the API endpoint.
kubernetesApiCallAction_sourceIps :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe [Prelude.Text])
kubernetesApiCallAction_sourceIps = Lens.lens (\KubernetesApiCallAction' {sourceIps} -> sourceIps) (\s@KubernetesApiCallAction' {} a -> s {sourceIps = a} :: KubernetesApiCallAction) Prelude.. Lens.mapping Lens.coerced

-- | Parameters related to the Kubernetes API call action.
kubernetesApiCallAction_parameters :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_parameters = Lens.lens (\KubernetesApiCallAction' {parameters} -> parameters) (\s@KubernetesApiCallAction' {} a -> s {parameters = a} :: KubernetesApiCallAction)

instance Core.FromJSON KubernetesApiCallAction where
  parseJSON =
    Core.withObject
      "KubernetesApiCallAction"
      ( \x ->
          KubernetesApiCallAction'
            Prelude.<$> (x Core..:? "remoteIpDetails")
            Prelude.<*> (x Core..:? "requestUri")
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "userAgent")
            Prelude.<*> (x Core..:? "verb")
            Prelude.<*> (x Core..:? "sourceIps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "parameters")
      )

instance Prelude.Hashable KubernetesApiCallAction where
  hashWithSalt _salt KubernetesApiCallAction' {..} =
    _salt `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` requestUri
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` userAgent
      `Prelude.hashWithSalt` verb
      `Prelude.hashWithSalt` sourceIps
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData KubernetesApiCallAction where
  rnf KubernetesApiCallAction' {..} =
    Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf requestUri
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf userAgent
      `Prelude.seq` Prelude.rnf verb
      `Prelude.seq` Prelude.rnf sourceIps
      `Prelude.seq` Prelude.rnf parameters
