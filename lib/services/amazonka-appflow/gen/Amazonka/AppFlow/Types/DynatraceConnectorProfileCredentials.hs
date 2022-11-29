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
-- Module      : Amazonka.AppFlow.Types.DynatraceConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DynatraceConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required by Dynatrace.
--
-- /See:/ 'newDynatraceConnectorProfileCredentials' smart constructor.
data DynatraceConnectorProfileCredentials = DynatraceConnectorProfileCredentials'
  { -- | The API tokens used by Dynatrace API to authenticate various API calls.
    apiToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynatraceConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiToken', 'dynatraceConnectorProfileCredentials_apiToken' - The API tokens used by Dynatrace API to authenticate various API calls.
newDynatraceConnectorProfileCredentials ::
  -- | 'apiToken'
  Prelude.Text ->
  DynatraceConnectorProfileCredentials
newDynatraceConnectorProfileCredentials pApiToken_ =
  DynatraceConnectorProfileCredentials'
    { apiToken =
        pApiToken_
    }

-- | The API tokens used by Dynatrace API to authenticate various API calls.
dynatraceConnectorProfileCredentials_apiToken :: Lens.Lens' DynatraceConnectorProfileCredentials Prelude.Text
dynatraceConnectorProfileCredentials_apiToken = Lens.lens (\DynatraceConnectorProfileCredentials' {apiToken} -> apiToken) (\s@DynatraceConnectorProfileCredentials' {} a -> s {apiToken = a} :: DynatraceConnectorProfileCredentials)

instance
  Prelude.Hashable
    DynatraceConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    DynatraceConnectorProfileCredentials' {..} =
      _salt `Prelude.hashWithSalt` apiToken

instance
  Prelude.NFData
    DynatraceConnectorProfileCredentials
  where
  rnf DynatraceConnectorProfileCredentials' {..} =
    Prelude.rnf apiToken

instance
  Core.ToJSON
    DynatraceConnectorProfileCredentials
  where
  toJSON DynatraceConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("apiToken" Core..= apiToken)]
      )
