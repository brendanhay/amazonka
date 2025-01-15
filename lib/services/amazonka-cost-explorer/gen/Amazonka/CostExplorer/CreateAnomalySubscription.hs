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
-- Module      : Amazonka.CostExplorer.CreateAnomalySubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an alert subscription to a cost anomaly detection monitor. You can
-- use each subscription to define subscribers with email or SNS
-- notifications. Email subscribers can set an absolute or percentage
-- threshold and a time frequency for receiving notifications.
module Amazonka.CostExplorer.CreateAnomalySubscription
  ( -- * Creating a Request
    CreateAnomalySubscription (..),
    newCreateAnomalySubscription,

    -- * Request Lenses
    createAnomalySubscription_resourceTags,
    createAnomalySubscription_anomalySubscription,

    -- * Destructuring the Response
    CreateAnomalySubscriptionResponse (..),
    newCreateAnomalySubscriptionResponse,

    -- * Response Lenses
    createAnomalySubscriptionResponse_httpStatus,
    createAnomalySubscriptionResponse_subscriptionArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAnomalySubscription' smart constructor.
data CreateAnomalySubscription = CreateAnomalySubscription'
  { -- | An optional list of tags to associate with the specified
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalySubscription.html AnomalySubscription>
    -- . You can use resource tags to control access to your @subscription@
    -- using IAM policies.
    --
    -- Each tag consists of a key and a value, and each key must be unique for
    -- the resource. The following restrictions apply to resource tags:
    --
    -- -   Although the maximum number of array members is 200, you can assign
    --     a maximum of 50 user-tags to one resource. The remaining are
    --     reserved for Amazon Web Services use
    --
    -- -   The maximum length of a key is 128 characters
    --
    -- -   The maximum length of a value is 256 characters
    --
    -- -   Keys and values can only contain alphanumeric characters, spaces,
    --     and any of the following: @_.:\/=+\@-@
    --
    -- -   Keys and values are case sensitive
    --
    -- -   Keys and values are trimmed for any leading or trailing whitespaces
    --
    -- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
    --     for Amazon Web Services use
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The cost anomaly subscription object that you want to create.
    anomalySubscription :: AnomalySubscription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTags', 'createAnomalySubscription_resourceTags' - An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalySubscription.html AnomalySubscription>
-- . You can use resource tags to control access to your @subscription@
-- using IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
--
-- 'anomalySubscription', 'createAnomalySubscription_anomalySubscription' - The cost anomaly subscription object that you want to create.
newCreateAnomalySubscription ::
  -- | 'anomalySubscription'
  AnomalySubscription ->
  CreateAnomalySubscription
newCreateAnomalySubscription pAnomalySubscription_ =
  CreateAnomalySubscription'
    { resourceTags =
        Prelude.Nothing,
      anomalySubscription = pAnomalySubscription_
    }

-- | An optional list of tags to associate with the specified
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_AnomalySubscription.html AnomalySubscription>
-- . You can use resource tags to control access to your @subscription@
-- using IAM policies.
--
-- Each tag consists of a key and a value, and each key must be unique for
-- the resource. The following restrictions apply to resource tags:
--
-- -   Although the maximum number of array members is 200, you can assign
--     a maximum of 50 user-tags to one resource. The remaining are
--     reserved for Amazon Web Services use
--
-- -   The maximum length of a key is 128 characters
--
-- -   The maximum length of a value is 256 characters
--
-- -   Keys and values can only contain alphanumeric characters, spaces,
--     and any of the following: @_.:\/=+\@-@
--
-- -   Keys and values are case sensitive
--
-- -   Keys and values are trimmed for any leading or trailing whitespaces
--
-- -   Don’t use @aws:@ as a prefix for your keys. This prefix is reserved
--     for Amazon Web Services use
createAnomalySubscription_resourceTags :: Lens.Lens' CreateAnomalySubscription (Prelude.Maybe [ResourceTag])
createAnomalySubscription_resourceTags = Lens.lens (\CreateAnomalySubscription' {resourceTags} -> resourceTags) (\s@CreateAnomalySubscription' {} a -> s {resourceTags = a} :: CreateAnomalySubscription) Prelude.. Lens.mapping Lens.coerced

-- | The cost anomaly subscription object that you want to create.
createAnomalySubscription_anomalySubscription :: Lens.Lens' CreateAnomalySubscription AnomalySubscription
createAnomalySubscription_anomalySubscription = Lens.lens (\CreateAnomalySubscription' {anomalySubscription} -> anomalySubscription) (\s@CreateAnomalySubscription' {} a -> s {anomalySubscription = a} :: CreateAnomalySubscription)

instance Core.AWSRequest CreateAnomalySubscription where
  type
    AWSResponse CreateAnomalySubscription =
      CreateAnomalySubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnomalySubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SubscriptionArn")
      )

instance Prelude.Hashable CreateAnomalySubscription where
  hashWithSalt _salt CreateAnomalySubscription' {..} =
    _salt
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` anomalySubscription

instance Prelude.NFData CreateAnomalySubscription where
  rnf CreateAnomalySubscription' {..} =
    Prelude.rnf resourceTags `Prelude.seq`
      Prelude.rnf anomalySubscription

instance Data.ToHeaders CreateAnomalySubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.CreateAnomalySubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAnomalySubscription where
  toJSON CreateAnomalySubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceTags" Data..=) Prelude.<$> resourceTags,
            Prelude.Just
              ("AnomalySubscription" Data..= anomalySubscription)
          ]
      )

instance Data.ToPath CreateAnomalySubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAnomalySubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnomalySubscriptionResponse' smart constructor.
data CreateAnomalySubscriptionResponse = CreateAnomalySubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of your newly created cost anomaly subscription.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalySubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAnomalySubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'subscriptionArn', 'createAnomalySubscriptionResponse_subscriptionArn' - The unique identifier of your newly created cost anomaly subscription.
newCreateAnomalySubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'subscriptionArn'
  Prelude.Text ->
  CreateAnomalySubscriptionResponse
newCreateAnomalySubscriptionResponse
  pHttpStatus_
  pSubscriptionArn_ =
    CreateAnomalySubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        subscriptionArn = pSubscriptionArn_
      }

-- | The response's http status code.
createAnomalySubscriptionResponse_httpStatus :: Lens.Lens' CreateAnomalySubscriptionResponse Prelude.Int
createAnomalySubscriptionResponse_httpStatus = Lens.lens (\CreateAnomalySubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateAnomalySubscriptionResponse' {} a -> s {httpStatus = a} :: CreateAnomalySubscriptionResponse)

-- | The unique identifier of your newly created cost anomaly subscription.
createAnomalySubscriptionResponse_subscriptionArn :: Lens.Lens' CreateAnomalySubscriptionResponse Prelude.Text
createAnomalySubscriptionResponse_subscriptionArn = Lens.lens (\CreateAnomalySubscriptionResponse' {subscriptionArn} -> subscriptionArn) (\s@CreateAnomalySubscriptionResponse' {} a -> s {subscriptionArn = a} :: CreateAnomalySubscriptionResponse)

instance
  Prelude.NFData
    CreateAnomalySubscriptionResponse
  where
  rnf CreateAnomalySubscriptionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf subscriptionArn
