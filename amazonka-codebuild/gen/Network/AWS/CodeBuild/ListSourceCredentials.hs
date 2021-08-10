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
-- Module      : Network.AWS.CodeBuild.ListSourceCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @SourceCredentialsInfo@ objects.
module Network.AWS.CodeBuild.ListSourceCredentials
  ( -- * Creating a Request
    ListSourceCredentials (..),
    newListSourceCredentials,

    -- * Destructuring the Response
    ListSourceCredentialsResponse (..),
    newListSourceCredentialsResponse,

    -- * Response Lenses
    listSourceCredentialsResponse_sourceCredentialsInfos,
    listSourceCredentialsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSourceCredentials' smart constructor.
data ListSourceCredentials = ListSourceCredentials'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListSourceCredentials ::
  ListSourceCredentials
newListSourceCredentials = ListSourceCredentials'

instance Core.AWSRequest ListSourceCredentials where
  type
    AWSResponse ListSourceCredentials =
      ListSourceCredentialsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSourceCredentialsResponse'
            Prelude.<$> ( x Core..?> "sourceCredentialsInfos"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSourceCredentials

instance Prelude.NFData ListSourceCredentials

instance Core.ToHeaders ListSourceCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListSourceCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSourceCredentials where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListSourceCredentials where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSourceCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSourceCredentialsResponse' smart constructor.
data ListSourceCredentialsResponse = ListSourceCredentialsResponse'
  { -- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@
    -- object includes the authentication type, token ARN, and type of source
    -- provider for one set of credentials.
    sourceCredentialsInfos :: Prelude.Maybe [SourceCredentialsInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCredentialsInfos', 'listSourceCredentialsResponse_sourceCredentialsInfos' - A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@
-- object includes the authentication type, token ARN, and type of source
-- provider for one set of credentials.
--
-- 'httpStatus', 'listSourceCredentialsResponse_httpStatus' - The response's http status code.
newListSourceCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSourceCredentialsResponse
newListSourceCredentialsResponse pHttpStatus_ =
  ListSourceCredentialsResponse'
    { sourceCredentialsInfos =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@
-- object includes the authentication type, token ARN, and type of source
-- provider for one set of credentials.
listSourceCredentialsResponse_sourceCredentialsInfos :: Lens.Lens' ListSourceCredentialsResponse (Prelude.Maybe [SourceCredentialsInfo])
listSourceCredentialsResponse_sourceCredentialsInfos = Lens.lens (\ListSourceCredentialsResponse' {sourceCredentialsInfos} -> sourceCredentialsInfos) (\s@ListSourceCredentialsResponse' {} a -> s {sourceCredentialsInfos = a} :: ListSourceCredentialsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSourceCredentialsResponse_httpStatus :: Lens.Lens' ListSourceCredentialsResponse Prelude.Int
listSourceCredentialsResponse_httpStatus = Lens.lens (\ListSourceCredentialsResponse' {httpStatus} -> httpStatus) (\s@ListSourceCredentialsResponse' {} a -> s {httpStatus = a} :: ListSourceCredentialsResponse)

instance Prelude.NFData ListSourceCredentialsResponse
