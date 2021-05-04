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
-- Module      : Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITTypeOfHIT@ operation allows you to change the HITType
-- properties of a HIT. This operation disassociates the HIT from its old
-- HITType properties and associates it with the new HITType properties.
-- The HIT takes on the properties of the new HITType in place of the old
-- ones.
module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
  ( -- * Creating a Request
    UpdateHITTypeOfHIT (..),
    newUpdateHITTypeOfHIT,

    -- * Request Lenses
    updateHITTypeOfHIT_hITId,
    updateHITTypeOfHIT_hITTypeId,

    -- * Destructuring the Response
    UpdateHITTypeOfHITResponse (..),
    newUpdateHITTypeOfHITResponse,

    -- * Response Lenses
    updateHITTypeOfHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateHITTypeOfHIT' smart constructor.
data UpdateHITTypeOfHIT = UpdateHITTypeOfHIT'
  { -- | The HIT to update.
    hITId :: Prelude.Text,
    -- | The ID of the new HIT type.
    hITTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateHITTypeOfHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITId', 'updateHITTypeOfHIT_hITId' - The HIT to update.
--
-- 'hITTypeId', 'updateHITTypeOfHIT_hITTypeId' - The ID of the new HIT type.
newUpdateHITTypeOfHIT ::
  -- | 'hITId'
  Prelude.Text ->
  -- | 'hITTypeId'
  Prelude.Text ->
  UpdateHITTypeOfHIT
newUpdateHITTypeOfHIT pHITId_ pHITTypeId_ =
  UpdateHITTypeOfHIT'
    { hITId = pHITId_,
      hITTypeId = pHITTypeId_
    }

-- | The HIT to update.
updateHITTypeOfHIT_hITId :: Lens.Lens' UpdateHITTypeOfHIT Prelude.Text
updateHITTypeOfHIT_hITId = Lens.lens (\UpdateHITTypeOfHIT' {hITId} -> hITId) (\s@UpdateHITTypeOfHIT' {} a -> s {hITId = a} :: UpdateHITTypeOfHIT)

-- | The ID of the new HIT type.
updateHITTypeOfHIT_hITTypeId :: Lens.Lens' UpdateHITTypeOfHIT Prelude.Text
updateHITTypeOfHIT_hITTypeId = Lens.lens (\UpdateHITTypeOfHIT' {hITTypeId} -> hITTypeId) (\s@UpdateHITTypeOfHIT' {} a -> s {hITTypeId = a} :: UpdateHITTypeOfHIT)

instance Prelude.AWSRequest UpdateHITTypeOfHIT where
  type
    Rs UpdateHITTypeOfHIT =
      UpdateHITTypeOfHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateHITTypeOfHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateHITTypeOfHIT

instance Prelude.NFData UpdateHITTypeOfHIT

instance Prelude.ToHeaders UpdateHITTypeOfHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.UpdateHITTypeOfHIT" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateHITTypeOfHIT where
  toJSON UpdateHITTypeOfHIT' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HITId" Prelude..= hITId),
            Prelude.Just ("HITTypeId" Prelude..= hITTypeId)
          ]
      )

instance Prelude.ToPath UpdateHITTypeOfHIT where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateHITTypeOfHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHITTypeOfHITResponse' smart constructor.
data UpdateHITTypeOfHITResponse = UpdateHITTypeOfHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateHITTypeOfHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHITTypeOfHITResponse_httpStatus' - The response's http status code.
newUpdateHITTypeOfHITResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateHITTypeOfHITResponse
newUpdateHITTypeOfHITResponse pHttpStatus_ =
  UpdateHITTypeOfHITResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateHITTypeOfHITResponse_httpStatus :: Lens.Lens' UpdateHITTypeOfHITResponse Prelude.Int
updateHITTypeOfHITResponse_httpStatus = Lens.lens (\UpdateHITTypeOfHITResponse' {httpStatus} -> httpStatus) (\s@UpdateHITTypeOfHITResponse' {} a -> s {httpStatus = a} :: UpdateHITTypeOfHITResponse)

instance Prelude.NFData UpdateHITTypeOfHITResponse
