{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.DeleteConfigurationAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration aggregator and the aggregated data
-- associated with the aggregator.
module Network.AWS.Config.DeleteConfigurationAggregator
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConfigurationAggregator' smart constructor.
data DeleteConfigurationAggregator = DeleteConfigurationAggregator'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteConfigurationAggregator
  where
  type
    Rs DeleteConfigurationAggregator =
      DeleteConfigurationAggregatorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteConfigurationAggregatorResponse'

instance
  Prelude.Hashable
    DeleteConfigurationAggregator

instance Prelude.NFData DeleteConfigurationAggregator

instance
  Prelude.ToHeaders
    DeleteConfigurationAggregator
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteConfigurationAggregator" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConfigurationAggregator where
  toJSON DeleteConfigurationAggregator' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationAggregatorName"
                  Prelude..= configurationAggregatorName
              )
          ]
      )

instance Prelude.ToPath DeleteConfigurationAggregator where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteConfigurationAggregator
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationAggregatorResponse' smart constructor.
data DeleteConfigurationAggregatorResponse = DeleteConfigurationAggregatorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
