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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newResolveAlias' smart constructor.
data ResolveAlias = ResolveAlias'
  { -- | The unique identifier of the alias that you want to retrieve a fleet ID
    -- for. You can use either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ResolveAlias where
  type Rs ResolveAlias = ResolveAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveAliasResponse'
            Prelude.<$> (x Prelude..?> "FleetArn")
            Prelude.<*> (x Prelude..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveAlias

instance Prelude.NFData ResolveAlias

instance Prelude.ToHeaders ResolveAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.ResolveAlias" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResolveAlias where
  toJSON ResolveAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Prelude..= aliasId)]
      )

instance Prelude.ToPath ResolveAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResolveAlias where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newResolveAliasResponse' smart constructor.
data ResolveAliasResponse = ResolveAliasResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet resource that this alias points to.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The fleet identifier that the alias is pointing to.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ResolveAliasResponse
newResolveAliasResponse pHttpStatus_ =
  ResolveAliasResponse'
    { fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet resource that this alias points to.
resolveAliasResponse_fleetArn :: Lens.Lens' ResolveAliasResponse (Prelude.Maybe Prelude.Text)
resolveAliasResponse_fleetArn = Lens.lens (\ResolveAliasResponse' {fleetArn} -> fleetArn) (\s@ResolveAliasResponse' {} a -> s {fleetArn = a} :: ResolveAliasResponse)

-- | The fleet identifier that the alias is pointing to.
resolveAliasResponse_fleetId :: Lens.Lens' ResolveAliasResponse (Prelude.Maybe Prelude.Text)
resolveAliasResponse_fleetId = Lens.lens (\ResolveAliasResponse' {fleetId} -> fleetId) (\s@ResolveAliasResponse' {} a -> s {fleetId = a} :: ResolveAliasResponse)

-- | The response's http status code.
resolveAliasResponse_httpStatus :: Lens.Lens' ResolveAliasResponse Prelude.Int
resolveAliasResponse_httpStatus = Lens.lens (\ResolveAliasResponse' {httpStatus} -> httpStatus) (\s@ResolveAliasResponse' {} a -> s {httpStatus = a} :: ResolveAliasResponse)

instance Prelude.NFData ResolveAliasResponse
