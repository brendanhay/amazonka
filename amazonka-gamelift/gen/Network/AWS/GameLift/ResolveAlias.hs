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
-- Module      : Network.AWS.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that an alias is currently pointing to.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
module Network.AWS.GameLift.ResolveAlias
  ( -- * Creating a Request
    ResolveAlias (..),
    newResolveAlias,

    -- * Request Lenses
    resolveAlias_aliasId,

    -- * Destructuring the Response
    ResolveAliasResponse (..),
    newResolveAliasResponse,

    -- * Response Lenses
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_fleetId,
    resolveAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newResolveAlias' smart constructor.
data ResolveAlias = ResolveAlias'
  { -- | The unique identifier of the alias that you want to retrieve a fleet ID
    -- for. You can use either the alias ID or ARN value.
    aliasId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResolveAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'resolveAlias_aliasId' - The unique identifier of the alias that you want to retrieve a fleet ID
-- for. You can use either the alias ID or ARN value.
newResolveAlias ::
  -- | 'aliasId'
  Core.Text ->
  ResolveAlias
newResolveAlias pAliasId_ =
  ResolveAlias' {aliasId = pAliasId_}

-- | The unique identifier of the alias that you want to retrieve a fleet ID
-- for. You can use either the alias ID or ARN value.
resolveAlias_aliasId :: Lens.Lens' ResolveAlias Core.Text
resolveAlias_aliasId = Lens.lens (\ResolveAlias' {aliasId} -> aliasId) (\s@ResolveAlias' {} a -> s {aliasId = a} :: ResolveAlias)

instance Core.AWSRequest ResolveAlias where
  type AWSResponse ResolveAlias = ResolveAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveAliasResponse'
            Core.<$> (x Core..?> "FleetArn")
            Core.<*> (x Core..?> "FleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResolveAlias

instance Core.NFData ResolveAlias

instance Core.ToHeaders ResolveAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ResolveAlias" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ResolveAlias where
  toJSON ResolveAlias' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AliasId" Core..= aliasId)]
      )

instance Core.ToPath ResolveAlias where
  toPath = Core.const "/"

instance Core.ToQuery ResolveAlias where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newResolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet resource that this alias points to.
    fleetArn :: Core.Maybe Core.Text,
    -- | The fleet identifier that the alias is pointing to.
    fleetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResolveAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'resolveAliasResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet resource that this alias points to.
--
-- 'fleetId', 'resolveAliasResponse_fleetId' - The fleet identifier that the alias is pointing to.
--
-- 'httpStatus', 'resolveAliasResponse_httpStatus' - The response's http status code.
newResolveAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResolveAliasResponse
newResolveAliasResponse pHttpStatus_ =
  ResolveAliasResponse'
    { fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet resource that this alias points to.
resolveAliasResponse_fleetArn :: Lens.Lens' ResolveAliasResponse (Core.Maybe Core.Text)
resolveAliasResponse_fleetArn = Lens.lens (\ResolveAliasResponse' {fleetArn} -> fleetArn) (\s@ResolveAliasResponse' {} a -> s {fleetArn = a} :: ResolveAliasResponse)

-- | The fleet identifier that the alias is pointing to.
resolveAliasResponse_fleetId :: Lens.Lens' ResolveAliasResponse (Core.Maybe Core.Text)
resolveAliasResponse_fleetId = Lens.lens (\ResolveAliasResponse' {fleetId} -> fleetId) (\s@ResolveAliasResponse' {} a -> s {fleetId = a} :: ResolveAliasResponse)

-- | The response's http status code.
resolveAliasResponse_httpStatus :: Lens.Lens' ResolveAliasResponse Core.Int
resolveAliasResponse_httpStatus = Lens.lens (\ResolveAliasResponse' {httpStatus} -> httpStatus) (\s@ResolveAliasResponse' {} a -> s {httpStatus = a} :: ResolveAliasResponse)

instance Core.NFData ResolveAliasResponse
