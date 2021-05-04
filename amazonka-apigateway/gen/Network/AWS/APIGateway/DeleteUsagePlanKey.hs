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
-- Module      : Network.AWS.APIGateway.DeleteUsagePlanKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan key and remove the underlying API key from the
-- associated usage plan.
module Network.AWS.APIGateway.DeleteUsagePlanKey
  ( -- * Creating a Request
    DeleteUsagePlanKey (..),
    newDeleteUsagePlanKey,

    -- * Request Lenses
    deleteUsagePlanKey_usagePlanId,
    deleteUsagePlanKey_keyId,

    -- * Destructuring the Response
    DeleteUsagePlanKeyResponse (..),
    newDeleteUsagePlanKeyResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The DELETE request to delete a usage plan key and remove the underlying
-- API key from the associated usage plan.
--
-- /See:/ 'newDeleteUsagePlanKey' smart constructor.
data DeleteUsagePlanKey = DeleteUsagePlanKey'
  { -- | [Required] The Id of the UsagePlan resource representing the usage plan
    -- containing the to-be-deleted UsagePlanKey resource representing a plan
    -- customer.
    usagePlanId :: Prelude.Text,
    -- | [Required] The Id of the UsagePlanKey resource to be deleted.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsagePlanKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'deleteUsagePlanKey_usagePlanId' - [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-deleted UsagePlanKey resource representing a plan
-- customer.
--
-- 'keyId', 'deleteUsagePlanKey_keyId' - [Required] The Id of the UsagePlanKey resource to be deleted.
newDeleteUsagePlanKey ::
  -- | 'usagePlanId'
  Prelude.Text ->
  -- | 'keyId'
  Prelude.Text ->
  DeleteUsagePlanKey
newDeleteUsagePlanKey pUsagePlanId_ pKeyId_ =
  DeleteUsagePlanKey'
    { usagePlanId = pUsagePlanId_,
      keyId = pKeyId_
    }

-- | [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-deleted UsagePlanKey resource representing a plan
-- customer.
deleteUsagePlanKey_usagePlanId :: Lens.Lens' DeleteUsagePlanKey Prelude.Text
deleteUsagePlanKey_usagePlanId = Lens.lens (\DeleteUsagePlanKey' {usagePlanId} -> usagePlanId) (\s@DeleteUsagePlanKey' {} a -> s {usagePlanId = a} :: DeleteUsagePlanKey)

-- | [Required] The Id of the UsagePlanKey resource to be deleted.
deleteUsagePlanKey_keyId :: Lens.Lens' DeleteUsagePlanKey Prelude.Text
deleteUsagePlanKey_keyId = Lens.lens (\DeleteUsagePlanKey' {keyId} -> keyId) (\s@DeleteUsagePlanKey' {} a -> s {keyId = a} :: DeleteUsagePlanKey)

instance Prelude.AWSRequest DeleteUsagePlanKey where
  type
    Rs DeleteUsagePlanKey =
      DeleteUsagePlanKeyResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteUsagePlanKeyResponse'

instance Prelude.Hashable DeleteUsagePlanKey

instance Prelude.NFData DeleteUsagePlanKey

instance Prelude.ToHeaders DeleteUsagePlanKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteUsagePlanKey where
  toPath DeleteUsagePlanKey' {..} =
    Prelude.mconcat
      [ "/usageplans/",
        Prelude.toBS usagePlanId,
        "/keys/",
        Prelude.toBS keyId
      ]

instance Prelude.ToQuery DeleteUsagePlanKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsagePlanKeyResponse' smart constructor.
data DeleteUsagePlanKeyResponse = DeleteUsagePlanKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsagePlanKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsagePlanKeyResponse ::
  DeleteUsagePlanKeyResponse
newDeleteUsagePlanKeyResponse =
  DeleteUsagePlanKeyResponse'

instance Prelude.NFData DeleteUsagePlanKeyResponse
