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
-- Module      : Network.AWS.SSM.StartAssociationsOnce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to run an association immediately and only one time.
-- This action can be helpful when troubleshooting associations.
module Network.AWS.SSM.StartAssociationsOnce
  ( -- * Creating a Request
    StartAssociationsOnce (..),
    newStartAssociationsOnce,

    -- * Request Lenses
    startAssociationsOnce_associationIds,

    -- * Destructuring the Response
    StartAssociationsOnceResponse (..),
    newStartAssociationsOnceResponse,

    -- * Response Lenses
    startAssociationsOnceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newStartAssociationsOnce' smart constructor.
data StartAssociationsOnce = StartAssociationsOnce'
  { -- | The association IDs that you want to run immediately and only one time.
    associationIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartAssociationsOnce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationIds', 'startAssociationsOnce_associationIds' - The association IDs that you want to run immediately and only one time.
newStartAssociationsOnce ::
  -- | 'associationIds'
  Core.NonEmpty Core.Text ->
  StartAssociationsOnce
newStartAssociationsOnce pAssociationIds_ =
  StartAssociationsOnce'
    { associationIds =
        Lens._Coerce Lens.# pAssociationIds_
    }

-- | The association IDs that you want to run immediately and only one time.
startAssociationsOnce_associationIds :: Lens.Lens' StartAssociationsOnce (Core.NonEmpty Core.Text)
startAssociationsOnce_associationIds = Lens.lens (\StartAssociationsOnce' {associationIds} -> associationIds) (\s@StartAssociationsOnce' {} a -> s {associationIds = a} :: StartAssociationsOnce) Core.. Lens._Coerce

instance Core.AWSRequest StartAssociationsOnce where
  type
    AWSResponse StartAssociationsOnce =
      StartAssociationsOnceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAssociationsOnceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartAssociationsOnce

instance Core.NFData StartAssociationsOnce

instance Core.ToHeaders StartAssociationsOnce where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.StartAssociationsOnce" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartAssociationsOnce where
  toJSON StartAssociationsOnce' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AssociationIds" Core..= associationIds)
          ]
      )

instance Core.ToPath StartAssociationsOnce where
  toPath = Core.const "/"

instance Core.ToQuery StartAssociationsOnce where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartAssociationsOnceResponse' smart constructor.
data StartAssociationsOnceResponse = StartAssociationsOnceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartAssociationsOnceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAssociationsOnceResponse_httpStatus' - The response's http status code.
newStartAssociationsOnceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartAssociationsOnceResponse
newStartAssociationsOnceResponse pHttpStatus_ =
  StartAssociationsOnceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startAssociationsOnceResponse_httpStatus :: Lens.Lens' StartAssociationsOnceResponse Core.Int
startAssociationsOnceResponse_httpStatus = Lens.lens (\StartAssociationsOnceResponse' {httpStatus} -> httpStatus) (\s@StartAssociationsOnceResponse' {} a -> s {httpStatus = a} :: StartAssociationsOnceResponse)

instance Core.NFData StartAssociationsOnceResponse
