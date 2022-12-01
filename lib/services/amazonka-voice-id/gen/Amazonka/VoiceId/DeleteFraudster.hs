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
-- Module      : Amazonka.VoiceId.DeleteFraudster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fraudster from Voice ID.
module Amazonka.VoiceId.DeleteFraudster
  ( -- * Creating a Request
    DeleteFraudster (..),
    newDeleteFraudster,

    -- * Request Lenses
    deleteFraudster_domainId,
    deleteFraudster_fraudsterId,

    -- * Destructuring the Response
    DeleteFraudsterResponse (..),
    newDeleteFraudsterResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDeleteFraudster' smart constructor.
data DeleteFraudster = DeleteFraudster'
  { -- | The identifier of the domain containing the fraudster.
    domainId :: Prelude.Text,
    -- | The identifier of the fraudster you want to delete.
    fraudsterId :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'deleteFraudster_domainId' - The identifier of the domain containing the fraudster.
--
-- 'fraudsterId', 'deleteFraudster_fraudsterId' - The identifier of the fraudster you want to delete.
newDeleteFraudster ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fraudsterId'
  Prelude.Text ->
  DeleteFraudster
newDeleteFraudster pDomainId_ pFraudsterId_ =
  DeleteFraudster'
    { domainId = pDomainId_,
      fraudsterId = Core._Sensitive Lens.# pFraudsterId_
    }

-- | The identifier of the domain containing the fraudster.
deleteFraudster_domainId :: Lens.Lens' DeleteFraudster Prelude.Text
deleteFraudster_domainId = Lens.lens (\DeleteFraudster' {domainId} -> domainId) (\s@DeleteFraudster' {} a -> s {domainId = a} :: DeleteFraudster)

-- | The identifier of the fraudster you want to delete.
deleteFraudster_fraudsterId :: Lens.Lens' DeleteFraudster Prelude.Text
deleteFraudster_fraudsterId = Lens.lens (\DeleteFraudster' {fraudsterId} -> fraudsterId) (\s@DeleteFraudster' {} a -> s {fraudsterId = a} :: DeleteFraudster) Prelude.. Core._Sensitive

instance Core.AWSRequest DeleteFraudster where
  type
    AWSResponse DeleteFraudster =
      DeleteFraudsterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteFraudsterResponse'

instance Prelude.Hashable DeleteFraudster where
  hashWithSalt _salt DeleteFraudster' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fraudsterId

instance Prelude.NFData DeleteFraudster where
  rnf DeleteFraudster' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fraudsterId

instance Core.ToHeaders DeleteFraudster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.DeleteFraudster" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFraudster where
  toJSON DeleteFraudster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just ("FraudsterId" Core..= fraudsterId)
          ]
      )

instance Core.ToPath DeleteFraudster where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFraudster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFraudsterResponse' smart constructor.
data DeleteFraudsterResponse = DeleteFraudsterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFraudsterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFraudsterResponse ::
  DeleteFraudsterResponse
newDeleteFraudsterResponse = DeleteFraudsterResponse'

instance Prelude.NFData DeleteFraudsterResponse where
  rnf _ = ()
