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
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition version.
module Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
  ( -- * Creating a Request
    GetSubscriptionDefinitionVersion (..),
    newGetSubscriptionDefinitionVersion,

    -- * Request Lenses
    getSubscriptionDefinitionVersion_nextToken,
    getSubscriptionDefinitionVersion_subscriptionDefinitionId,
    getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId,

    -- * Destructuring the Response
    GetSubscriptionDefinitionVersionResponse (..),
    newGetSubscriptionDefinitionVersionResponse,

    -- * Response Lenses
    getSubscriptionDefinitionVersionResponse_creationTimestamp,
    getSubscriptionDefinitionVersionResponse_nextToken,
    getSubscriptionDefinitionVersionResponse_arn,
    getSubscriptionDefinitionVersionResponse_id,
    getSubscriptionDefinitionVersionResponse_version,
    getSubscriptionDefinitionVersionResponse_definition,
    getSubscriptionDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSubscriptionDefinitionVersion' smart constructor.
data GetSubscriptionDefinitionVersion = GetSubscriptionDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text,
    -- | The ID of the subscription definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListSubscriptionDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- subscription definition, the value also maps to the
    -- \'\'LatestVersion\'\' property of the corresponding
    -- \'\'DefinitionInformation\'\' object.
    subscriptionDefinitionVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSubscriptionDefinitionVersion_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'subscriptionDefinitionId', 'getSubscriptionDefinitionVersion_subscriptionDefinitionId' - The ID of the subscription definition.
--
-- 'subscriptionDefinitionVersionId', 'getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId' - The ID of the subscription definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListSubscriptionDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- subscription definition, the value also maps to the
-- \'\'LatestVersion\'\' property of the corresponding
-- \'\'DefinitionInformation\'\' object.
newGetSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Prelude.Text ->
  -- | 'subscriptionDefinitionVersionId'
  Prelude.Text ->
  GetSubscriptionDefinitionVersion
newGetSubscriptionDefinitionVersion
  pSubscriptionDefinitionId_
  pSubscriptionDefinitionVersionId_ =
    GetSubscriptionDefinitionVersion'
      { nextToken =
          Prelude.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_,
        subscriptionDefinitionVersionId =
          pSubscriptionDefinitionVersionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getSubscriptionDefinitionVersion_nextToken :: Lens.Lens' GetSubscriptionDefinitionVersion (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersion_nextToken = Lens.lens (\GetSubscriptionDefinitionVersion' {nextToken} -> nextToken) (\s@GetSubscriptionDefinitionVersion' {} a -> s {nextToken = a} :: GetSubscriptionDefinitionVersion)

-- | The ID of the subscription definition.
getSubscriptionDefinitionVersion_subscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinitionVersion Prelude.Text
getSubscriptionDefinitionVersion_subscriptionDefinitionId = Lens.lens (\GetSubscriptionDefinitionVersion' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@GetSubscriptionDefinitionVersion' {} a -> s {subscriptionDefinitionId = a} :: GetSubscriptionDefinitionVersion)

-- | The ID of the subscription definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListSubscriptionDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- subscription definition, the value also maps to the
-- \'\'LatestVersion\'\' property of the corresponding
-- \'\'DefinitionInformation\'\' object.
getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId :: Lens.Lens' GetSubscriptionDefinitionVersion Prelude.Text
getSubscriptionDefinitionVersion_subscriptionDefinitionVersionId = Lens.lens (\GetSubscriptionDefinitionVersion' {subscriptionDefinitionVersionId} -> subscriptionDefinitionVersionId) (\s@GetSubscriptionDefinitionVersion' {} a -> s {subscriptionDefinitionVersionId = a} :: GetSubscriptionDefinitionVersion)

instance
  Core.AWSRequest
    GetSubscriptionDefinitionVersion
  where
  type
    AWSResponse GetSubscriptionDefinitionVersion =
      GetSubscriptionDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionVersionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSubscriptionDefinitionVersion

instance
  Prelude.NFData
    GetSubscriptionDefinitionVersion

instance
  Core.ToHeaders
    GetSubscriptionDefinitionVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSubscriptionDefinitionVersion where
  toPath GetSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId,
        "/versions/",
        Core.toBS subscriptionDefinitionVersionId
      ]

instance
  Core.ToQuery
    GetSubscriptionDefinitionVersion
  where
  toQuery GetSubscriptionDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetSubscriptionDefinitionVersionResponse' smart constructor.
data GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the subscription
    -- definition version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the subscription definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the subscription definition version.
    definition :: Prelude.Maybe SubscriptionDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubscriptionDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getSubscriptionDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the subscription
-- definition version was created.
--
-- 'nextToken', 'getSubscriptionDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getSubscriptionDefinitionVersionResponse_arn' - The ARN of the subscription definition version.
--
-- 'id', 'getSubscriptionDefinitionVersionResponse_id' - The ID of the subscription definition version.
--
-- 'version', 'getSubscriptionDefinitionVersionResponse_version' - The version of the subscription definition version.
--
-- 'definition', 'getSubscriptionDefinitionVersionResponse_definition' - Information about the subscription definition version.
--
-- 'httpStatus', 'getSubscriptionDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetSubscriptionDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSubscriptionDefinitionVersionResponse
newGetSubscriptionDefinitionVersionResponse
  pHttpStatus_ =
    GetSubscriptionDefinitionVersionResponse'
      { creationTimestamp =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        arn = Prelude.Nothing,
        id = Prelude.Nothing,
        version = Prelude.Nothing,
        definition = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The time, in milliseconds since the epoch, when the subscription
-- definition version was created.
getSubscriptionDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetSubscriptionDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getSubscriptionDefinitionVersionResponse_nextToken :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersionResponse_nextToken = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetSubscriptionDefinitionVersionResponse)

-- | The ARN of the subscription definition version.
getSubscriptionDefinitionVersionResponse_arn :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersionResponse_arn = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {arn} -> arn) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {arn = a} :: GetSubscriptionDefinitionVersionResponse)

-- | The ID of the subscription definition version.
getSubscriptionDefinitionVersionResponse_id :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersionResponse_id = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {id} -> id) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {id = a} :: GetSubscriptionDefinitionVersionResponse)

-- | The version of the subscription definition version.
getSubscriptionDefinitionVersionResponse_version :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getSubscriptionDefinitionVersionResponse_version = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {version} -> version) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {version = a} :: GetSubscriptionDefinitionVersionResponse)

-- | Information about the subscription definition version.
getSubscriptionDefinitionVersionResponse_definition :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Prelude.Maybe SubscriptionDefinitionVersion)
getSubscriptionDefinitionVersionResponse_definition = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {definition} -> definition) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {definition = a} :: GetSubscriptionDefinitionVersionResponse)

-- | The response's http status code.
getSubscriptionDefinitionVersionResponse_httpStatus :: Lens.Lens' GetSubscriptionDefinitionVersionResponse Prelude.Int
getSubscriptionDefinitionVersionResponse_httpStatus = Lens.lens (\GetSubscriptionDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetSubscriptionDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetSubscriptionDefinitionVersionResponse)

instance
  Prelude.NFData
    GetSubscriptionDefinitionVersionResponse
