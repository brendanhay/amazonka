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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesApiCallAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.RemoteIpDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the Kubernetes API call action described in this
-- finding.
--
-- /See:/ 'newKubernetesApiCallAction' smart constructor.
data KubernetesApiCallAction = KubernetesApiCallAction'
  { -- | Parameters related to the Kubernetes API call action.
    parameters :: Prelude.Maybe Prelude.Text,
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The Kubernetes API request URI.
    requestUri :: Prelude.Maybe Prelude.Text,
    -- | The IP of the Kubernetes API caller and the IPs of any proxies or load
    -- balancers between the caller and the API endpoint.
    sourceIps :: Prelude.Maybe [Prelude.Text],
    -- | The resulting HTTP response code of the Kubernetes API call action.
    statusCode :: Prelude.Maybe Prelude.Int,
    -- | The user agent of the caller of the Kubernetes API.
    userAgent :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes API request HTTP verb.
    verb :: Prelude.Maybe Prelude.Text
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
-- 'parameters', 'kubernetesApiCallAction_parameters' - Parameters related to the Kubernetes API call action.
--
-- 'remoteIpDetails', 'kubernetesApiCallAction_remoteIpDetails' - Undocumented member.
--
-- 'requestUri', 'kubernetesApiCallAction_requestUri' - The Kubernetes API request URI.
--
-- 'sourceIps', 'kubernetesApiCallAction_sourceIps' - The IP of the Kubernetes API caller and the IPs of any proxies or load
-- balancers between the caller and the API endpoint.
--
-- 'statusCode', 'kubernetesApiCallAction_statusCode' - The resulting HTTP response code of the Kubernetes API call action.
--
-- 'userAgent', 'kubernetesApiCallAction_userAgent' - The user agent of the caller of the Kubernetes API.
--
-- 'verb', 'kubernetesApiCallAction_verb' - The Kubernetes API request HTTP verb.
newKubernetesApiCallAction ::
  KubernetesApiCallAction
newKubernetesApiCallAction =
  KubernetesApiCallAction'
    { parameters =
        Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      requestUri = Prelude.Nothing,
      sourceIps = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      userAgent = Prelude.Nothing,
      verb = Prelude.Nothing
    }

-- | Parameters related to the Kubernetes API call action.
kubernetesApiCallAction_parameters :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_parameters = Lens.lens (\KubernetesApiCallAction' {parameters} -> parameters) (\s@KubernetesApiCallAction' {} a -> s {parameters = a} :: KubernetesApiCallAction)

-- | Undocumented member.
kubernetesApiCallAction_remoteIpDetails :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe RemoteIpDetails)
kubernetesApiCallAction_remoteIpDetails = Lens.lens (\KubernetesApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@KubernetesApiCallAction' {} a -> s {remoteIpDetails = a} :: KubernetesApiCallAction)

-- | The Kubernetes API request URI.
kubernetesApiCallAction_requestUri :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_requestUri = Lens.lens (\KubernetesApiCallAction' {requestUri} -> requestUri) (\s@KubernetesApiCallAction' {} a -> s {requestUri = a} :: KubernetesApiCallAction)

-- | The IP of the Kubernetes API caller and the IPs of any proxies or load
-- balancers between the caller and the API endpoint.
kubernetesApiCallAction_sourceIps :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe [Prelude.Text])
kubernetesApiCallAction_sourceIps = Lens.lens (\KubernetesApiCallAction' {sourceIps} -> sourceIps) (\s@KubernetesApiCallAction' {} a -> s {sourceIps = a} :: KubernetesApiCallAction) Prelude.. Lens.mapping Lens.coerced

-- | The resulting HTTP response code of the Kubernetes API call action.
kubernetesApiCallAction_statusCode :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Int)
kubernetesApiCallAction_statusCode = Lens.lens (\KubernetesApiCallAction' {statusCode} -> statusCode) (\s@KubernetesApiCallAction' {} a -> s {statusCode = a} :: KubernetesApiCallAction)

-- | The user agent of the caller of the Kubernetes API.
kubernetesApiCallAction_userAgent :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_userAgent = Lens.lens (\KubernetesApiCallAction' {userAgent} -> userAgent) (\s@KubernetesApiCallAction' {} a -> s {userAgent = a} :: KubernetesApiCallAction)

-- | The Kubernetes API request HTTP verb.
kubernetesApiCallAction_verb :: Lens.Lens' KubernetesApiCallAction (Prelude.Maybe Prelude.Text)
kubernetesApiCallAction_verb = Lens.lens (\KubernetesApiCallAction' {verb} -> verb) (\s@KubernetesApiCallAction' {} a -> s {verb = a} :: KubernetesApiCallAction)

instance Data.FromJSON KubernetesApiCallAction where
  parseJSON =
    Data.withObject
      "KubernetesApiCallAction"
      ( \x ->
          KubernetesApiCallAction'
            Prelude.<$> (x Data..:? "parameters")
            Prelude.<*> (x Data..:? "remoteIpDetails")
            Prelude.<*> (x Data..:? "requestUri")
            Prelude.<*> (x Data..:? "sourceIps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "userAgent")
            Prelude.<*> (x Data..:? "verb")
      )

instance Prelude.Hashable KubernetesApiCallAction where
  hashWithSalt _salt KubernetesApiCallAction' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` requestUri
      `Prelude.hashWithSalt` sourceIps
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` userAgent
      `Prelude.hashWithSalt` verb

instance Prelude.NFData KubernetesApiCallAction where
  rnf KubernetesApiCallAction' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf requestUri
      `Prelude.seq` Prelude.rnf sourceIps
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf userAgent
      `Prelude.seq` Prelude.rnf verb
