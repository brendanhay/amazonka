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
-- Module      : Amazonka.CloudFormation.DescribePublisher
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a CloudFormation extension publisher.
--
-- If you don\'t supply a @PublisherId@, and you have registered as an
-- extension publisher, @DescribePublisher@ returns information about your
-- own publisher account.
--
-- For more information about registering as a publisher, see:
--
-- -   <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterPublisher.html RegisterPublisher>
--
-- -   <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html Publishing extensions to make them available for public use>
--     in the /CloudFormation CLI User Guide/
module Amazonka.CloudFormation.DescribePublisher
  ( -- * Creating a Request
    DescribePublisher (..),
    newDescribePublisher,

    -- * Request Lenses
    describePublisher_publisherId,

    -- * Destructuring the Response
    DescribePublisherResponse (..),
    newDescribePublisherResponse,

    -- * Response Lenses
    describePublisherResponse_identityProvider,
    describePublisherResponse_publisherId,
    describePublisherResponse_publisherProfile,
    describePublisherResponse_publisherStatus,
    describePublisherResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePublisher' smart constructor.
data DescribePublisher = DescribePublisher'
  { -- | The ID of the extension publisher.
    --
    -- If you don\'t supply a @PublisherId@, and you have registered as an
    -- extension publisher, @DescribePublisher@ returns information about your
    -- own publisher account.
    publisherId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePublisher' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publisherId', 'describePublisher_publisherId' - The ID of the extension publisher.
--
-- If you don\'t supply a @PublisherId@, and you have registered as an
-- extension publisher, @DescribePublisher@ returns information about your
-- own publisher account.
newDescribePublisher ::
  DescribePublisher
newDescribePublisher =
  DescribePublisher' {publisherId = Prelude.Nothing}

-- | The ID of the extension publisher.
--
-- If you don\'t supply a @PublisherId@, and you have registered as an
-- extension publisher, @DescribePublisher@ returns information about your
-- own publisher account.
describePublisher_publisherId :: Lens.Lens' DescribePublisher (Prelude.Maybe Prelude.Text)
describePublisher_publisherId = Lens.lens (\DescribePublisher' {publisherId} -> publisherId) (\s@DescribePublisher' {} a -> s {publisherId = a} :: DescribePublisher)

instance Core.AWSRequest DescribePublisher where
  type
    AWSResponse DescribePublisher =
      DescribePublisherResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribePublisherResult"
      ( \s h x ->
          DescribePublisherResponse'
            Prelude.<$> (x Data..@? "IdentityProvider")
            Prelude.<*> (x Data..@? "PublisherId")
            Prelude.<*> (x Data..@? "PublisherProfile")
            Prelude.<*> (x Data..@? "PublisherStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePublisher where
  hashWithSalt _salt DescribePublisher' {..} =
    _salt `Prelude.hashWithSalt` publisherId

instance Prelude.NFData DescribePublisher where
  rnf DescribePublisher' {..} = Prelude.rnf publisherId

instance Data.ToHeaders DescribePublisher where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePublisher where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePublisher where
  toQuery DescribePublisher' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePublisher" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "PublisherId" Data.=: publisherId
      ]

-- | /See:/ 'newDescribePublisherResponse' smart constructor.
data DescribePublisherResponse = DescribePublisherResponse'
  { -- | The type of account used as the identity provider when registering this
    -- publisher with CloudFormation.
    identityProvider :: Prelude.Maybe IdentityProvider,
    -- | The ID of the extension publisher.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The URL to the publisher\'s profile with the identity provider.
    publisherProfile :: Prelude.Maybe Prelude.Text,
    -- | Whether the publisher is verified. Currently, all registered publishers
    -- are verified.
    publisherStatus :: Prelude.Maybe PublisherStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePublisherResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProvider', 'describePublisherResponse_identityProvider' - The type of account used as the identity provider when registering this
-- publisher with CloudFormation.
--
-- 'publisherId', 'describePublisherResponse_publisherId' - The ID of the extension publisher.
--
-- 'publisherProfile', 'describePublisherResponse_publisherProfile' - The URL to the publisher\'s profile with the identity provider.
--
-- 'publisherStatus', 'describePublisherResponse_publisherStatus' - Whether the publisher is verified. Currently, all registered publishers
-- are verified.
--
-- 'httpStatus', 'describePublisherResponse_httpStatus' - The response's http status code.
newDescribePublisherResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePublisherResponse
newDescribePublisherResponse pHttpStatus_ =
  DescribePublisherResponse'
    { identityProvider =
        Prelude.Nothing,
      publisherId = Prelude.Nothing,
      publisherProfile = Prelude.Nothing,
      publisherStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of account used as the identity provider when registering this
-- publisher with CloudFormation.
describePublisherResponse_identityProvider :: Lens.Lens' DescribePublisherResponse (Prelude.Maybe IdentityProvider)
describePublisherResponse_identityProvider = Lens.lens (\DescribePublisherResponse' {identityProvider} -> identityProvider) (\s@DescribePublisherResponse' {} a -> s {identityProvider = a} :: DescribePublisherResponse)

-- | The ID of the extension publisher.
describePublisherResponse_publisherId :: Lens.Lens' DescribePublisherResponse (Prelude.Maybe Prelude.Text)
describePublisherResponse_publisherId = Lens.lens (\DescribePublisherResponse' {publisherId} -> publisherId) (\s@DescribePublisherResponse' {} a -> s {publisherId = a} :: DescribePublisherResponse)

-- | The URL to the publisher\'s profile with the identity provider.
describePublisherResponse_publisherProfile :: Lens.Lens' DescribePublisherResponse (Prelude.Maybe Prelude.Text)
describePublisherResponse_publisherProfile = Lens.lens (\DescribePublisherResponse' {publisherProfile} -> publisherProfile) (\s@DescribePublisherResponse' {} a -> s {publisherProfile = a} :: DescribePublisherResponse)

-- | Whether the publisher is verified. Currently, all registered publishers
-- are verified.
describePublisherResponse_publisherStatus :: Lens.Lens' DescribePublisherResponse (Prelude.Maybe PublisherStatus)
describePublisherResponse_publisherStatus = Lens.lens (\DescribePublisherResponse' {publisherStatus} -> publisherStatus) (\s@DescribePublisherResponse' {} a -> s {publisherStatus = a} :: DescribePublisherResponse)

-- | The response's http status code.
describePublisherResponse_httpStatus :: Lens.Lens' DescribePublisherResponse Prelude.Int
describePublisherResponse_httpStatus = Lens.lens (\DescribePublisherResponse' {httpStatus} -> httpStatus) (\s@DescribePublisherResponse' {} a -> s {httpStatus = a} :: DescribePublisherResponse)

instance Prelude.NFData DescribePublisherResponse where
  rnf DescribePublisherResponse' {..} =
    Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf publisherId
      `Prelude.seq` Prelude.rnf publisherProfile
      `Prelude.seq` Prelude.rnf publisherStatus
      `Prelude.seq` Prelude.rnf httpStatus
