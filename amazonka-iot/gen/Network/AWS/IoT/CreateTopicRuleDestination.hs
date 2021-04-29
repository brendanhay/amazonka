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
-- Module      : Network.AWS.IoT.CreateTopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic rule destination. The destination must be confirmed
-- prior to use.
module Network.AWS.IoT.CreateTopicRuleDestination
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTopicRuleDestination' smart constructor.
data CreateTopicRuleDestination = CreateTopicRuleDestination'
  { -- | The topic rule destination configuration.
    destinationConfiguration :: TopicRuleDestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    CreateTopicRuleDestination
  where
  type
    Rs CreateTopicRuleDestination =
      CreateTopicRuleDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTopicRuleDestinationResponse'
            Prelude.<$> (x Prelude..?> "topicRuleDestination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTopicRuleDestination

instance Prelude.NFData CreateTopicRuleDestination

instance Prelude.ToHeaders CreateTopicRuleDestination where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateTopicRuleDestination where
  toJSON CreateTopicRuleDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "destinationConfiguration"
                  Prelude..= destinationConfiguration
              )
          ]
      )

instance Prelude.ToPath CreateTopicRuleDestination where
  toPath = Prelude.const "/destinations"

instance Prelude.ToQuery CreateTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTopicRuleDestinationResponse' smart constructor.
data CreateTopicRuleDestinationResponse = CreateTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Prelude.Maybe TopicRuleDestination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
