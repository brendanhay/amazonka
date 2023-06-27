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
-- Module      : Amazonka.WellArchitected.GetProfileTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get profile template.
module Amazonka.WellArchitected.GetProfileTemplate
  ( -- * Creating a Request
    GetProfileTemplate (..),
    newGetProfileTemplate,

    -- * Destructuring the Response
    GetProfileTemplateResponse (..),
    newGetProfileTemplateResponse,

    -- * Response Lenses
    getProfileTemplateResponse_profileTemplate,
    getProfileTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newGetProfileTemplate' smart constructor.
data GetProfileTemplate = GetProfileTemplate'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetProfileTemplate ::
  GetProfileTemplate
newGetProfileTemplate = GetProfileTemplate'

instance Core.AWSRequest GetProfileTemplate where
  type
    AWSResponse GetProfileTemplate =
      GetProfileTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileTemplateResponse'
            Prelude.<$> (x Data..?> "ProfileTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProfileTemplate where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetProfileTemplate where
  rnf _ = ()

instance Data.ToHeaders GetProfileTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProfileTemplate where
  toPath = Prelude.const "/profileTemplate"

instance Data.ToQuery GetProfileTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProfileTemplateResponse' smart constructor.
data GetProfileTemplateResponse = GetProfileTemplateResponse'
  { -- | The profile template.
    profileTemplate :: Prelude.Maybe ProfileTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileTemplate', 'getProfileTemplateResponse_profileTemplate' - The profile template.
--
-- 'httpStatus', 'getProfileTemplateResponse_httpStatus' - The response's http status code.
newGetProfileTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProfileTemplateResponse
newGetProfileTemplateResponse pHttpStatus_ =
  GetProfileTemplateResponse'
    { profileTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The profile template.
getProfileTemplateResponse_profileTemplate :: Lens.Lens' GetProfileTemplateResponse (Prelude.Maybe ProfileTemplate)
getProfileTemplateResponse_profileTemplate = Lens.lens (\GetProfileTemplateResponse' {profileTemplate} -> profileTemplate) (\s@GetProfileTemplateResponse' {} a -> s {profileTemplate = a} :: GetProfileTemplateResponse)

-- | The response's http status code.
getProfileTemplateResponse_httpStatus :: Lens.Lens' GetProfileTemplateResponse Prelude.Int
getProfileTemplateResponse_httpStatus = Lens.lens (\GetProfileTemplateResponse' {httpStatus} -> httpStatus) (\s@GetProfileTemplateResponse' {} a -> s {httpStatus = a} :: GetProfileTemplateResponse)

instance Prelude.NFData GetProfileTemplateResponse where
  rnf GetProfileTemplateResponse' {..} =
    Prelude.rnf profileTemplate
      `Prelude.seq` Prelude.rnf httpStatus
