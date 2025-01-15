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
-- Module      : Amazonka.SecurityLake.CreateSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription permission for accounts that are already enabled
-- in Amazon Security Lake. You can create a subscriber with access to data
-- in the current Amazon Web Services Region.
module Amazonka.SecurityLake.CreateSubscriber
  ( -- * Creating a Request
    CreateSubscriber (..),
    newCreateSubscriber,

    -- * Request Lenses
    createSubscriber_accessTypes,
    createSubscriber_subscriberDescription,
    createSubscriber_accountId,
    createSubscriber_externalId,
    createSubscriber_sourceTypes,
    createSubscriber_subscriberName,

    -- * Destructuring the Response
    CreateSubscriberResponse (..),
    newCreateSubscriberResponse,

    -- * Response Lenses
    createSubscriberResponse_roleArn,
    createSubscriberResponse_s3BucketArn,
    createSubscriberResponse_snsArn,
    createSubscriberResponse_httpStatus,
    createSubscriberResponse_subscriptionId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { -- | The Amazon S3 or Lake Formation access type.
    accessTypes :: Prelude.Maybe [AccessType],
    -- | The description for your subscriber account in Security Lake.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID used to access your data.
    accountId :: Prelude.Text,
    -- | The external ID of the subscriber. This lets the user that is assuming
    -- the role assert the circumstances in which they are operating. It also
    -- provides a way for the account owner to permit the role to be assumed
    -- only under specific circumstances.
    externalId :: Prelude.Text,
    -- | The supported Amazon Web Services from which logs and events are
    -- collected. Security Lake supports log and event collection for natively
    -- supported Amazon Web Services.
    sourceTypes :: [SourceType],
    -- | The name of your Security Lake subscriber account.
    subscriberName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessTypes', 'createSubscriber_accessTypes' - The Amazon S3 or Lake Formation access type.
--
-- 'subscriberDescription', 'createSubscriber_subscriberDescription' - The description for your subscriber account in Security Lake.
--
-- 'accountId', 'createSubscriber_accountId' - The Amazon Web Services account ID used to access your data.
--
-- 'externalId', 'createSubscriber_externalId' - The external ID of the subscriber. This lets the user that is assuming
-- the role assert the circumstances in which they are operating. It also
-- provides a way for the account owner to permit the role to be assumed
-- only under specific circumstances.
--
-- 'sourceTypes', 'createSubscriber_sourceTypes' - The supported Amazon Web Services from which logs and events are
-- collected. Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
--
-- 'subscriberName', 'createSubscriber_subscriberName' - The name of your Security Lake subscriber account.
newCreateSubscriber ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  -- | 'subscriberName'
  Prelude.Text ->
  CreateSubscriber
newCreateSubscriber
  pAccountId_
  pExternalId_
  pSubscriberName_ =
    CreateSubscriber'
      { accessTypes = Prelude.Nothing,
        subscriberDescription = Prelude.Nothing,
        accountId = pAccountId_,
        externalId = pExternalId_,
        sourceTypes = Prelude.mempty,
        subscriberName = pSubscriberName_
      }

-- | The Amazon S3 or Lake Formation access type.
createSubscriber_accessTypes :: Lens.Lens' CreateSubscriber (Prelude.Maybe [AccessType])
createSubscriber_accessTypes = Lens.lens (\CreateSubscriber' {accessTypes} -> accessTypes) (\s@CreateSubscriber' {} a -> s {accessTypes = a} :: CreateSubscriber) Prelude.. Lens.mapping Lens.coerced

-- | The description for your subscriber account in Security Lake.
createSubscriber_subscriberDescription :: Lens.Lens' CreateSubscriber (Prelude.Maybe Prelude.Text)
createSubscriber_subscriberDescription = Lens.lens (\CreateSubscriber' {subscriberDescription} -> subscriberDescription) (\s@CreateSubscriber' {} a -> s {subscriberDescription = a} :: CreateSubscriber)

-- | The Amazon Web Services account ID used to access your data.
createSubscriber_accountId :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_accountId = Lens.lens (\CreateSubscriber' {accountId} -> accountId) (\s@CreateSubscriber' {} a -> s {accountId = a} :: CreateSubscriber)

-- | The external ID of the subscriber. This lets the user that is assuming
-- the role assert the circumstances in which they are operating. It also
-- provides a way for the account owner to permit the role to be assumed
-- only under specific circumstances.
createSubscriber_externalId :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_externalId = Lens.lens (\CreateSubscriber' {externalId} -> externalId) (\s@CreateSubscriber' {} a -> s {externalId = a} :: CreateSubscriber)

-- | The supported Amazon Web Services from which logs and events are
-- collected. Security Lake supports log and event collection for natively
-- supported Amazon Web Services.
createSubscriber_sourceTypes :: Lens.Lens' CreateSubscriber [SourceType]
createSubscriber_sourceTypes = Lens.lens (\CreateSubscriber' {sourceTypes} -> sourceTypes) (\s@CreateSubscriber' {} a -> s {sourceTypes = a} :: CreateSubscriber) Prelude.. Lens.coerced

-- | The name of your Security Lake subscriber account.
createSubscriber_subscriberName :: Lens.Lens' CreateSubscriber Prelude.Text
createSubscriber_subscriberName = Lens.lens (\CreateSubscriber' {subscriberName} -> subscriberName) (\s@CreateSubscriber' {} a -> s {subscriberName = a} :: CreateSubscriber)

instance Core.AWSRequest CreateSubscriber where
  type
    AWSResponse CreateSubscriber =
      CreateSubscriberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriberResponse'
            Prelude.<$> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "s3BucketArn")
            Prelude.<*> (x Data..?> "snsArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "subscriptionId")
      )

instance Prelude.Hashable CreateSubscriber where
  hashWithSalt _salt CreateSubscriber' {..} =
    _salt
      `Prelude.hashWithSalt` accessTypes
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` sourceTypes
      `Prelude.hashWithSalt` subscriberName

instance Prelude.NFData CreateSubscriber where
  rnf CreateSubscriber' {..} =
    Prelude.rnf accessTypes `Prelude.seq`
      Prelude.rnf subscriberDescription `Prelude.seq`
        Prelude.rnf accountId `Prelude.seq`
          Prelude.rnf externalId `Prelude.seq`
            Prelude.rnf sourceTypes `Prelude.seq`
              Prelude.rnf subscriberName

instance Data.ToHeaders CreateSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSubscriber where
  toJSON CreateSubscriber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessTypes" Data..=) Prelude.<$> accessTypes,
            ("subscriberDescription" Data..=)
              Prelude.<$> subscriberDescription,
            Prelude.Just ("accountId" Data..= accountId),
            Prelude.Just ("externalId" Data..= externalId),
            Prelude.Just ("sourceTypes" Data..= sourceTypes),
            Prelude.Just
              ("subscriberName" Data..= subscriberName)
          ]
      )

instance Data.ToPath CreateSubscriber where
  toPath = Prelude.const "/v1/subscribers"

instance Data.ToQuery CreateSubscriber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSubscriberResponse' smart constructor.
data CreateSubscriberResponse = CreateSubscriberResponse'
  { -- | The Amazon Resource Name (ARN) created by you to provide to the
    -- subscriber. For more information about ARNs and how to use them in
    -- policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers in the Identity and Access Management (IAM) User Guide>.
    -- .
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the Amazon S3 bucket.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the Amazon Simple Notification Service.
    snsArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The @subscriptionId@ created by the @CreateSubscriber@ API call.
    subscriptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'createSubscriberResponse_roleArn' - The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers in the Identity and Access Management (IAM) User Guide>.
-- .
--
-- 's3BucketArn', 'createSubscriberResponse_s3BucketArn' - The ARN for the Amazon S3 bucket.
--
-- 'snsArn', 'createSubscriberResponse_snsArn' - The ARN for the Amazon Simple Notification Service.
--
-- 'httpStatus', 'createSubscriberResponse_httpStatus' - The response's http status code.
--
-- 'subscriptionId', 'createSubscriberResponse_subscriptionId' - The @subscriptionId@ created by the @CreateSubscriber@ API call.
newCreateSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'subscriptionId'
  Prelude.Text ->
  CreateSubscriberResponse
newCreateSubscriberResponse
  pHttpStatus_
  pSubscriptionId_ =
    CreateSubscriberResponse'
      { roleArn =
          Prelude.Nothing,
        s3BucketArn = Prelude.Nothing,
        snsArn = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        subscriptionId = pSubscriptionId_
      }

-- | The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers in the Identity and Access Management (IAM) User Guide>.
-- .
createSubscriberResponse_roleArn :: Lens.Lens' CreateSubscriberResponse (Prelude.Maybe Prelude.Text)
createSubscriberResponse_roleArn = Lens.lens (\CreateSubscriberResponse' {roleArn} -> roleArn) (\s@CreateSubscriberResponse' {} a -> s {roleArn = a} :: CreateSubscriberResponse)

-- | The ARN for the Amazon S3 bucket.
createSubscriberResponse_s3BucketArn :: Lens.Lens' CreateSubscriberResponse (Prelude.Maybe Prelude.Text)
createSubscriberResponse_s3BucketArn = Lens.lens (\CreateSubscriberResponse' {s3BucketArn} -> s3BucketArn) (\s@CreateSubscriberResponse' {} a -> s {s3BucketArn = a} :: CreateSubscriberResponse)

-- | The ARN for the Amazon Simple Notification Service.
createSubscriberResponse_snsArn :: Lens.Lens' CreateSubscriberResponse (Prelude.Maybe Prelude.Text)
createSubscriberResponse_snsArn = Lens.lens (\CreateSubscriberResponse' {snsArn} -> snsArn) (\s@CreateSubscriberResponse' {} a -> s {snsArn = a} :: CreateSubscriberResponse)

-- | The response's http status code.
createSubscriberResponse_httpStatus :: Lens.Lens' CreateSubscriberResponse Prelude.Int
createSubscriberResponse_httpStatus = Lens.lens (\CreateSubscriberResponse' {httpStatus} -> httpStatus) (\s@CreateSubscriberResponse' {} a -> s {httpStatus = a} :: CreateSubscriberResponse)

-- | The @subscriptionId@ created by the @CreateSubscriber@ API call.
createSubscriberResponse_subscriptionId :: Lens.Lens' CreateSubscriberResponse Prelude.Text
createSubscriberResponse_subscriptionId = Lens.lens (\CreateSubscriberResponse' {subscriptionId} -> subscriptionId) (\s@CreateSubscriberResponse' {} a -> s {subscriptionId = a} :: CreateSubscriberResponse)

instance Prelude.NFData CreateSubscriberResponse where
  rnf CreateSubscriberResponse' {..} =
    Prelude.rnf roleArn `Prelude.seq`
      Prelude.rnf s3BucketArn `Prelude.seq`
        Prelude.rnf snsArn `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf subscriptionId
