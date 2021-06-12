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
-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon
-- EC2.
module Network.AWS.EC2.DeleteKeyPair
  ( -- * Creating a Request
    DeleteKeyPair (..),
    newDeleteKeyPair,

    -- * Request Lenses
    deleteKeyPair_dryRun,
    deleteKeyPair_keyPairId,
    deleteKeyPair_keyName,

    -- * Destructuring the Response
    DeleteKeyPairResponse (..),
    newDeleteKeyPairResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the key pair.
    keyPairId :: Core.Maybe Core.Text,
    -- | The name of the key pair.
    keyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteKeyPair_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'keyPairId', 'deleteKeyPair_keyPairId' - The ID of the key pair.
--
-- 'keyName', 'deleteKeyPair_keyName' - The name of the key pair.
newDeleteKeyPair ::
  DeleteKeyPair
newDeleteKeyPair =
  DeleteKeyPair'
    { dryRun = Core.Nothing,
      keyPairId = Core.Nothing,
      keyName = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteKeyPair_dryRun :: Lens.Lens' DeleteKeyPair (Core.Maybe Core.Bool)
deleteKeyPair_dryRun = Lens.lens (\DeleteKeyPair' {dryRun} -> dryRun) (\s@DeleteKeyPair' {} a -> s {dryRun = a} :: DeleteKeyPair)

-- | The ID of the key pair.
deleteKeyPair_keyPairId :: Lens.Lens' DeleteKeyPair (Core.Maybe Core.Text)
deleteKeyPair_keyPairId = Lens.lens (\DeleteKeyPair' {keyPairId} -> keyPairId) (\s@DeleteKeyPair' {} a -> s {keyPairId = a} :: DeleteKeyPair)

-- | The name of the key pair.
deleteKeyPair_keyName :: Lens.Lens' DeleteKeyPair (Core.Maybe Core.Text)
deleteKeyPair_keyName = Lens.lens (\DeleteKeyPair' {keyName} -> keyName) (\s@DeleteKeyPair' {} a -> s {keyName = a} :: DeleteKeyPair)

instance Core.AWSRequest DeleteKeyPair where
  type
    AWSResponse DeleteKeyPair =
      DeleteKeyPairResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteKeyPairResponse'

instance Core.Hashable DeleteKeyPair

instance Core.NFData DeleteKeyPair

instance Core.ToHeaders DeleteKeyPair where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery DeleteKeyPair where
  toQuery DeleteKeyPair' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteKeyPair" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "KeyPairId" Core.=: keyPairId,
        "KeyName" Core.=: keyName
      ]

-- | /See:/ 'newDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteKeyPairResponse ::
  DeleteKeyPairResponse
newDeleteKeyPairResponse = DeleteKeyPairResponse'

instance Core.NFData DeleteKeyPairResponse
