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
-- Module      : Amazonka.GameLift.ResolveAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the fleet ID that an alias is currently pointing to.
--
-- __Related actions__
--
-- CreateAlias | ListAliases | DescribeAlias | UpdateAlias | DeleteAlias |
-- ResolveAlias |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.ResolveAlias
  ( -- * Creating a Request
    ResolveAlias (..),
    newResolveAlias,

    -- * Request Lenses
    resolveAlias_aliasId,

    -- * Destructuring the Response
    ResolveAliasResponse (..),
    newResolveAliasResponse,

    -- * Response Lenses
    resolveAliasResponse_fleetId,
    resolveAliasResponse_fleetArn,
    resolveAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newResolveAlias' smart constructor.
data ResolveAlias = ResolveAlias'
  { -- | The unique identifier of the alias that you want to retrieve a fleet ID
    -- for. You can use either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ResolveAlias
newResolveAlias pAliasId_ =
  ResolveAlias' {aliasId = pAliasId_}

-- | The unique identifier of the alias that you want to retrieve a fleet ID
-- for. You can use either the alias ID or ARN value.
resolveAlias_aliasId :: Lens.Lens' ResolveAlias Prelude.Text
resolveAlias_aliasId = Lens.lens (\ResolveAlias' {aliasId} -> aliasId) (\s@ResolveAlias' {} a -> s {aliasId = a} :: ResolveAlias)

instance Core.AWSRequest ResolveAlias where
  type AWSResponse ResolveAlias = ResolveAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveAliasResponse'
            Prelude.<$> (x Core..?> "FleetId")
            Prelude.<*> (x Core..?> "FleetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveAlias where
  hashWithSalt _salt ResolveAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasId

instance Prelude.NFData ResolveAlias where
  rnf ResolveAlias' {..} = Prelude.rnf aliasId

instance Core.ToHeaders ResolveAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.ResolveAlias" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResolveAlias where
  toJSON ResolveAlias' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Core..= aliasId)]
      )

instance Core.ToPath ResolveAlias where
  toPath = Prelude.const "/"

instance Core.ToQuery ResolveAlias where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newResolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { -- | The fleet identifier that the alias is pointing to.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift fleet resource that this alias points to.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'resolveAliasResponse_fleetId' - The fleet identifier that the alias is pointing to.
--
-- 'fleetArn', 'resolveAliasResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource that this alias points to.
--
-- 'httpStatus', 'resolveAliasResponse_httpStatus' - The response's http status code.
newResolveAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveAliasResponse
newResolveAliasResponse pHttpStatus_ =
  ResolveAliasResponse'
    { fleetId = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The fleet identifier that the alias is pointing to.
resolveAliasResponse_fleetId :: Lens.Lens' ResolveAliasResponse (Prelude.Maybe Prelude.Text)
resolveAliasResponse_fleetId = Lens.lens (\ResolveAliasResponse' {fleetId} -> fleetId) (\s@ResolveAliasResponse' {} a -> s {fleetId = a} :: ResolveAliasResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource that this alias points to.
resolveAliasResponse_fleetArn :: Lens.Lens' ResolveAliasResponse (Prelude.Maybe Prelude.Text)
resolveAliasResponse_fleetArn = Lens.lens (\ResolveAliasResponse' {fleetArn} -> fleetArn) (\s@ResolveAliasResponse' {} a -> s {fleetArn = a} :: ResolveAliasResponse)

-- | The response's http status code.
resolveAliasResponse_httpStatus :: Lens.Lens' ResolveAliasResponse Prelude.Int
resolveAliasResponse_httpStatus = Lens.lens (\ResolveAliasResponse' {httpStatus} -> httpStatus) (\s@ResolveAliasResponse' {} a -> s {httpStatus = a} :: ResolveAliasResponse)

instance Prelude.NFData ResolveAliasResponse where
  rnf ResolveAliasResponse' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf httpStatus
