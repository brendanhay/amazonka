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
-- Module      : Amazonka.SSM.StartAssociationsOnce
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an association immediately and only one time. This operation can be
-- helpful when troubleshooting associations.
module Amazonka.SSM.StartAssociationsOnce
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newStartAssociationsOnce' smart constructor.
data StartAssociationsOnce = StartAssociationsOnce'
  { -- | The association IDs that you want to run immediately and only one time.
    associationIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  StartAssociationsOnce
newStartAssociationsOnce pAssociationIds_ =
  StartAssociationsOnce'
    { associationIds =
        Lens.coerced Lens.# pAssociationIds_
    }

-- | The association IDs that you want to run immediately and only one time.
startAssociationsOnce_associationIds :: Lens.Lens' StartAssociationsOnce (Prelude.NonEmpty Prelude.Text)
startAssociationsOnce_associationIds = Lens.lens (\StartAssociationsOnce' {associationIds} -> associationIds) (\s@StartAssociationsOnce' {} a -> s {associationIds = a} :: StartAssociationsOnce) Prelude.. Lens.coerced

instance Core.AWSRequest StartAssociationsOnce where
  type
    AWSResponse StartAssociationsOnce =
      StartAssociationsOnceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAssociationsOnceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAssociationsOnce where
  hashWithSalt _salt StartAssociationsOnce' {..} =
    _salt `Prelude.hashWithSalt` associationIds

instance Prelude.NFData StartAssociationsOnce where
  rnf StartAssociationsOnce' {..} =
    Prelude.rnf associationIds

instance Core.ToHeaders StartAssociationsOnce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.StartAssociationsOnce" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartAssociationsOnce where
  toJSON StartAssociationsOnce' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AssociationIds" Core..= associationIds)
          ]
      )

instance Core.ToPath StartAssociationsOnce where
  toPath = Prelude.const "/"

instance Core.ToQuery StartAssociationsOnce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAssociationsOnceResponse' smart constructor.
data StartAssociationsOnceResponse = StartAssociationsOnceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartAssociationsOnceResponse
newStartAssociationsOnceResponse pHttpStatus_ =
  StartAssociationsOnceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startAssociationsOnceResponse_httpStatus :: Lens.Lens' StartAssociationsOnceResponse Prelude.Int
startAssociationsOnceResponse_httpStatus = Lens.lens (\StartAssociationsOnceResponse' {httpStatus} -> httpStatus) (\s@StartAssociationsOnceResponse' {} a -> s {httpStatus = a} :: StartAssociationsOnceResponse)

instance Prelude.NFData StartAssociationsOnceResponse where
  rnf StartAssociationsOnceResponse' {..} =
    Prelude.rnf httpStatus
