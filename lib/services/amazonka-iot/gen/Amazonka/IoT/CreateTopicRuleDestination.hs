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
-- Module      : Amazonka.IoT.CreateTopicRuleDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic rule destination. The destination must be confirmed
-- prior to use.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateTopicRuleDestination>
-- action.
module Amazonka.IoT.CreateTopicRuleDestination
  ( -- * Creating a Request
    CreateTopicRuleDestination (..),
    newCreateTopicRuleDestination,

    -- * Request Lenses
    createTopicRuleDestination_destinationConfiguration,

    -- * Destructuring the Response
    CreateTopicRuleDestinationResponse (..),
    newCreateTopicRuleDestinationResponse,

    -- * Response Lenses
    createTopicRuleDestinationResponse_topicRuleDestination,
    createTopicRuleDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTopicRuleDestination' smart constructor.
data CreateTopicRuleDestination = CreateTopicRuleDestination'
  { -- | The topic rule destination configuration.
    destinationConfiguration :: TopicRuleDestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfiguration', 'createTopicRuleDestination_destinationConfiguration' - The topic rule destination configuration.
newCreateTopicRuleDestination ::
  -- | 'destinationConfiguration'
  TopicRuleDestinationConfiguration ->
  CreateTopicRuleDestination
newCreateTopicRuleDestination
  pDestinationConfiguration_ =
    CreateTopicRuleDestination'
      { destinationConfiguration =
          pDestinationConfiguration_
      }

-- | The topic rule destination configuration.
createTopicRuleDestination_destinationConfiguration :: Lens.Lens' CreateTopicRuleDestination TopicRuleDestinationConfiguration
createTopicRuleDestination_destinationConfiguration = Lens.lens (\CreateTopicRuleDestination' {destinationConfiguration} -> destinationConfiguration) (\s@CreateTopicRuleDestination' {} a -> s {destinationConfiguration = a} :: CreateTopicRuleDestination)

instance Core.AWSRequest CreateTopicRuleDestination where
  type
    AWSResponse CreateTopicRuleDestination =
      CreateTopicRuleDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTopicRuleDestinationResponse'
            Prelude.<$> (x Data..?> "topicRuleDestination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTopicRuleDestination where
  hashWithSalt _salt CreateTopicRuleDestination' {..} =
    _salt
      `Prelude.hashWithSalt` destinationConfiguration

instance Prelude.NFData CreateTopicRuleDestination where
  rnf CreateTopicRuleDestination' {..} =
    Prelude.rnf destinationConfiguration

instance Data.ToHeaders CreateTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateTopicRuleDestination where
  toJSON CreateTopicRuleDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "destinationConfiguration"
                  Data..= destinationConfiguration
              )
          ]
      )

instance Data.ToPath CreateTopicRuleDestination where
  toPath = Prelude.const "/destinations"

instance Data.ToQuery CreateTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTopicRuleDestinationResponse' smart constructor.
data CreateTopicRuleDestinationResponse = CreateTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Prelude.Maybe TopicRuleDestination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTopicRuleDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicRuleDestination', 'createTopicRuleDestinationResponse_topicRuleDestination' - The topic rule destination.
--
-- 'httpStatus', 'createTopicRuleDestinationResponse_httpStatus' - The response's http status code.
newCreateTopicRuleDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTopicRuleDestinationResponse
newCreateTopicRuleDestinationResponse pHttpStatus_ =
  CreateTopicRuleDestinationResponse'
    { topicRuleDestination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The topic rule destination.
createTopicRuleDestinationResponse_topicRuleDestination :: Lens.Lens' CreateTopicRuleDestinationResponse (Prelude.Maybe TopicRuleDestination)
createTopicRuleDestinationResponse_topicRuleDestination = Lens.lens (\CreateTopicRuleDestinationResponse' {topicRuleDestination} -> topicRuleDestination) (\s@CreateTopicRuleDestinationResponse' {} a -> s {topicRuleDestination = a} :: CreateTopicRuleDestinationResponse)

-- | The response's http status code.
createTopicRuleDestinationResponse_httpStatus :: Lens.Lens' CreateTopicRuleDestinationResponse Prelude.Int
createTopicRuleDestinationResponse_httpStatus = Lens.lens (\CreateTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: CreateTopicRuleDestinationResponse)

instance
  Prelude.NFData
    CreateTopicRuleDestinationResponse
  where
  rnf CreateTopicRuleDestinationResponse' {..} =
    Prelude.rnf topicRuleDestination
      `Prelude.seq` Prelude.rnf httpStatus
