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
-- Module      : Amazonka.Config.DeleteConfigurationAggregator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration aggregator and the aggregated data
-- associated with the aggregator.
module Amazonka.Config.DeleteConfigurationAggregator
  ( -- * Creating a Request
    DeleteConfigurationAggregator (..),
    newDeleteConfigurationAggregator,

    -- * Request Lenses
    deleteConfigurationAggregator_configurationAggregatorName,

    -- * Destructuring the Response
    DeleteConfigurationAggregatorResponse (..),
    newDeleteConfigurationAggregatorResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfigurationAggregator' smart constructor.
data DeleteConfigurationAggregator = DeleteConfigurationAggregator'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregatorName', 'deleteConfigurationAggregator_configurationAggregatorName' - The name of the configuration aggregator.
newDeleteConfigurationAggregator ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  DeleteConfigurationAggregator
newDeleteConfigurationAggregator
  pConfigurationAggregatorName_ =
    DeleteConfigurationAggregator'
      { configurationAggregatorName =
          pConfigurationAggregatorName_
      }

-- | The name of the configuration aggregator.
deleteConfigurationAggregator_configurationAggregatorName :: Lens.Lens' DeleteConfigurationAggregator Prelude.Text
deleteConfigurationAggregator_configurationAggregatorName = Lens.lens (\DeleteConfigurationAggregator' {configurationAggregatorName} -> configurationAggregatorName) (\s@DeleteConfigurationAggregator' {} a -> s {configurationAggregatorName = a} :: DeleteConfigurationAggregator)

instance
  Core.AWSRequest
    DeleteConfigurationAggregator
  where
  type
    AWSResponse DeleteConfigurationAggregator =
      DeleteConfigurationAggregatorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteConfigurationAggregatorResponse'

instance
  Prelude.Hashable
    DeleteConfigurationAggregator
  where
  hashWithSalt _salt DeleteConfigurationAggregator' {..} =
    _salt
      `Prelude.hashWithSalt` configurationAggregatorName

instance Prelude.NFData DeleteConfigurationAggregator where
  rnf DeleteConfigurationAggregator' {..} =
    Prelude.rnf configurationAggregatorName

instance Data.ToHeaders DeleteConfigurationAggregator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteConfigurationAggregator" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConfigurationAggregator where
  toJSON DeleteConfigurationAggregator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              )
          ]
      )

instance Data.ToPath DeleteConfigurationAggregator where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConfigurationAggregator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationAggregatorResponse' smart constructor.
data DeleteConfigurationAggregatorResponse = DeleteConfigurationAggregatorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationAggregatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationAggregatorResponse ::
  DeleteConfigurationAggregatorResponse
newDeleteConfigurationAggregatorResponse =
  DeleteConfigurationAggregatorResponse'

instance
  Prelude.NFData
    DeleteConfigurationAggregatorResponse
  where
  rnf _ = ()
