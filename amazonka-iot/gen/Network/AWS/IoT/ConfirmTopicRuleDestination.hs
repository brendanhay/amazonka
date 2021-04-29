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
-- Module      : Network.AWS.IoT.ConfirmTopicRuleDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a topic rule destination. When you create a rule requiring a
-- destination, AWS IoT sends a confirmation message to the endpoint or
-- base address you specify. The message includes a token which you pass
-- back when calling @ConfirmTopicRuleDestination@ to confirm that you own
-- or have access to the endpoint.
module Network.AWS.IoT.ConfirmTopicRuleDestination
  ( -- * Creating a Request
    ConfirmTopicRuleDestination (..),
    newConfirmTopicRuleDestination,

    -- * Request Lenses
    confirmTopicRuleDestination_confirmationToken,

    -- * Destructuring the Response
    ConfirmTopicRuleDestinationResponse (..),
    newConfirmTopicRuleDestinationResponse,

    -- * Response Lenses
    confirmTopicRuleDestinationResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newConfirmTopicRuleDestination' smart constructor.
data ConfirmTopicRuleDestination = ConfirmTopicRuleDestination'
  { -- | The token used to confirm ownership or access to the topic rule
    -- confirmation URL.
    confirmationToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfirmTopicRuleDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confirmationToken', 'confirmTopicRuleDestination_confirmationToken' - The token used to confirm ownership or access to the topic rule
-- confirmation URL.
newConfirmTopicRuleDestination ::
  -- | 'confirmationToken'
  Prelude.Text ->
  ConfirmTopicRuleDestination
newConfirmTopicRuleDestination pConfirmationToken_ =
  ConfirmTopicRuleDestination'
    { confirmationToken =
        pConfirmationToken_
    }

-- | The token used to confirm ownership or access to the topic rule
-- confirmation URL.
confirmTopicRuleDestination_confirmationToken :: Lens.Lens' ConfirmTopicRuleDestination Prelude.Text
confirmTopicRuleDestination_confirmationToken = Lens.lens (\ConfirmTopicRuleDestination' {confirmationToken} -> confirmationToken) (\s@ConfirmTopicRuleDestination' {} a -> s {confirmationToken = a} :: ConfirmTopicRuleDestination)

instance
  Prelude.AWSRequest
    ConfirmTopicRuleDestination
  where
  type
    Rs ConfirmTopicRuleDestination =
      ConfirmTopicRuleDestinationResponse
  request = Request.get defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ConfirmTopicRuleDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConfirmTopicRuleDestination

instance Prelude.NFData ConfirmTopicRuleDestination

instance
  Prelude.ToHeaders
    ConfirmTopicRuleDestination
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ConfirmTopicRuleDestination where
  toPath ConfirmTopicRuleDestination' {..} =
    Prelude.mconcat
      [ "/confirmdestination/",
        Prelude.toBS confirmationToken
      ]

instance Prelude.ToQuery ConfirmTopicRuleDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfirmTopicRuleDestinationResponse' smart constructor.
data ConfirmTopicRuleDestinationResponse = ConfirmTopicRuleDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfirmTopicRuleDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'confirmTopicRuleDestinationResponse_httpStatus' - The response's http status code.
newConfirmTopicRuleDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmTopicRuleDestinationResponse
newConfirmTopicRuleDestinationResponse pHttpStatus_ =
  ConfirmTopicRuleDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
confirmTopicRuleDestinationResponse_httpStatus :: Lens.Lens' ConfirmTopicRuleDestinationResponse Prelude.Int
confirmTopicRuleDestinationResponse_httpStatus = Lens.lens (\ConfirmTopicRuleDestinationResponse' {httpStatus} -> httpStatus) (\s@ConfirmTopicRuleDestinationResponse' {} a -> s {httpStatus = a} :: ConfirmTopicRuleDestinationResponse)

instance
  Prelude.NFData
    ConfirmTopicRuleDestinationResponse
