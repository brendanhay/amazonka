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
-- Module      : Amazonka.ChimeSdkMediaPipelines.DeleteMediaInsightsPipelineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration settings.
module Amazonka.ChimeSdkMediaPipelines.DeleteMediaInsightsPipelineConfiguration
  ( -- * Creating a Request
    DeleteMediaInsightsPipelineConfiguration (..),
    newDeleteMediaInsightsPipelineConfiguration,

    -- * Request Lenses
    deleteMediaInsightsPipelineConfiguration_identifier,

    -- * Destructuring the Response
    DeleteMediaInsightsPipelineConfigurationResponse (..),
    newDeleteMediaInsightsPipelineConfigurationResponse,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMediaInsightsPipelineConfiguration' smart constructor.
data DeleteMediaInsightsPipelineConfiguration = DeleteMediaInsightsPipelineConfiguration'
  { -- | The unique identifier of the resource to be deleted. Valid values
    -- include the name and ARN of the media insights pipeline configuration.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMediaInsightsPipelineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'deleteMediaInsightsPipelineConfiguration_identifier' - The unique identifier of the resource to be deleted. Valid values
-- include the name and ARN of the media insights pipeline configuration.
newDeleteMediaInsightsPipelineConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteMediaInsightsPipelineConfiguration
newDeleteMediaInsightsPipelineConfiguration
  pIdentifier_ =
    DeleteMediaInsightsPipelineConfiguration'
      { identifier =
          pIdentifier_
      }

-- | The unique identifier of the resource to be deleted. Valid values
-- include the name and ARN of the media insights pipeline configuration.
deleteMediaInsightsPipelineConfiguration_identifier :: Lens.Lens' DeleteMediaInsightsPipelineConfiguration Prelude.Text
deleteMediaInsightsPipelineConfiguration_identifier = Lens.lens (\DeleteMediaInsightsPipelineConfiguration' {identifier} -> identifier) (\s@DeleteMediaInsightsPipelineConfiguration' {} a -> s {identifier = a} :: DeleteMediaInsightsPipelineConfiguration)

instance
  Core.AWSRequest
    DeleteMediaInsightsPipelineConfiguration
  where
  type
    AWSResponse
      DeleteMediaInsightsPipelineConfiguration =
      DeleteMediaInsightsPipelineConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMediaInsightsPipelineConfigurationResponse'

instance
  Prelude.Hashable
    DeleteMediaInsightsPipelineConfiguration
  where
  hashWithSalt
    _salt
    DeleteMediaInsightsPipelineConfiguration' {..} =
      _salt `Prelude.hashWithSalt` identifier

instance
  Prelude.NFData
    DeleteMediaInsightsPipelineConfiguration
  where
  rnf DeleteMediaInsightsPipelineConfiguration' {..} =
    Prelude.rnf identifier

instance
  Data.ToHeaders
    DeleteMediaInsightsPipelineConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteMediaInsightsPipelineConfiguration
  where
  toPath DeleteMediaInsightsPipelineConfiguration' {..} =
    Prelude.mconcat
      [ "/media-insights-pipeline-configurations/",
        Data.toBS identifier
      ]

instance
  Data.ToQuery
    DeleteMediaInsightsPipelineConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMediaInsightsPipelineConfigurationResponse' smart constructor.
data DeleteMediaInsightsPipelineConfigurationResponse = DeleteMediaInsightsPipelineConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMediaInsightsPipelineConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMediaInsightsPipelineConfigurationResponse ::
  DeleteMediaInsightsPipelineConfigurationResponse
newDeleteMediaInsightsPipelineConfigurationResponse =
  DeleteMediaInsightsPipelineConfigurationResponse'

instance
  Prelude.NFData
    DeleteMediaInsightsPipelineConfigurationResponse
  where
  rnf _ = ()
