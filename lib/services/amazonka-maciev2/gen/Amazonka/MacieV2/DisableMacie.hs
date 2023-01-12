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
-- Module      : Amazonka.MacieV2.DisableMacie
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables Amazon Macie and deletes all settings and resources for a Macie
-- account.
module Amazonka.MacieV2.DisableMacie
  ( -- * Creating a Request
    DisableMacie (..),
    newDisableMacie,

    -- * Destructuring the Response
    DisableMacieResponse (..),
    newDisableMacieResponse,

    -- * Response Lenses
    disableMacieResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableMacie' smart constructor.
data DisableMacie = DisableMacie'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableMacie' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableMacie ::
  DisableMacie
newDisableMacie = DisableMacie'

instance Core.AWSRequest DisableMacie where
  type AWSResponse DisableMacie = DisableMacieResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableMacieResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableMacie where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisableMacie where
  rnf _ = ()

instance Data.ToHeaders DisableMacie where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisableMacie where
  toPath = Prelude.const "/macie"

instance Data.ToQuery DisableMacie where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableMacieResponse' smart constructor.
data DisableMacieResponse = DisableMacieResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableMacieResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableMacieResponse_httpStatus' - The response's http status code.
newDisableMacieResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableMacieResponse
newDisableMacieResponse pHttpStatus_ =
  DisableMacieResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableMacieResponse_httpStatus :: Lens.Lens' DisableMacieResponse Prelude.Int
disableMacieResponse_httpStatus = Lens.lens (\DisableMacieResponse' {httpStatus} -> httpStatus) (\s@DisableMacieResponse' {} a -> s {httpStatus = a} :: DisableMacieResponse)

instance Prelude.NFData DisableMacieResponse where
  rnf DisableMacieResponse' {..} =
    Prelude.rnf httpStatus
