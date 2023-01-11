{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalogAppRegistry.PutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a @TagKey@ configuration to an account.
module Amazonka.ServiceCatalogAppRegistry.PutConfiguration
  ( -- * Creating a Request
    PutConfiguration (..),
    newPutConfiguration,

    -- * Request Lenses
    putConfiguration_configuration,

    -- * Destructuring the Response
    PutConfigurationResponse (..),
    newPutConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newPutConfiguration' smart constructor.
data PutConfiguration = PutConfiguration'
  { -- | Associates a @TagKey@ configuration to an account.
    configuration :: AppRegistryConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putConfiguration_configuration' - Associates a @TagKey@ configuration to an account.
newPutConfiguration ::
  -- | 'configuration'
  AppRegistryConfiguration ->
  PutConfiguration
newPutConfiguration pConfiguration_ =
  PutConfiguration' {configuration = pConfiguration_}

-- | Associates a @TagKey@ configuration to an account.
putConfiguration_configuration :: Lens.Lens' PutConfiguration AppRegistryConfiguration
putConfiguration_configuration = Lens.lens (\PutConfiguration' {configuration} -> configuration) (\s@PutConfiguration' {} a -> s {configuration = a} :: PutConfiguration)

instance Core.AWSRequest PutConfiguration where
  type
    AWSResponse PutConfiguration =
      PutConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull PutConfigurationResponse'

instance Prelude.Hashable PutConfiguration where
  hashWithSalt _salt PutConfiguration' {..} =
    _salt `Prelude.hashWithSalt` configuration

instance Prelude.NFData PutConfiguration where
  rnf PutConfiguration' {..} = Prelude.rnf configuration

instance Data.ToHeaders PutConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutConfiguration where
  toJSON PutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configuration" Data..= configuration)
          ]
      )

instance Data.ToPath PutConfiguration where
  toPath = Prelude.const "/configuration"

instance Data.ToQuery PutConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConfigurationResponse' smart constructor.
data PutConfigurationResponse = PutConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutConfigurationResponse ::
  PutConfigurationResponse
newPutConfigurationResponse =
  PutConfigurationResponse'

instance Prelude.NFData PutConfigurationResponse where
  rnf _ = ()
