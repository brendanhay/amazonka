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
-- Module      : Amazonka.Personalize.DescribeSolutionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific version of a solution. For more information on
-- solutions, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>
module Amazonka.Personalize.DescribeSolutionVersion
  ( -- * Creating a Request
    DescribeSolutionVersion (..),
    newDescribeSolutionVersion,

    -- * Request Lenses
    describeSolutionVersion_solutionVersionArn,

    -- * Destructuring the Response
    DescribeSolutionVersionResponse (..),
    newDescribeSolutionVersionResponse,

    -- * Response Lenses
    describeSolutionVersionResponse_solutionVersion,
    describeSolutionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSolutionVersion' smart constructor.
data DescribeSolutionVersion = DescribeSolutionVersion'
  { -- | The Amazon Resource Name (ARN) of the solution version.
    solutionVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSolutionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionVersionArn', 'describeSolutionVersion_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version.
newDescribeSolutionVersion ::
  -- | 'solutionVersionArn'
  Prelude.Text ->
  DescribeSolutionVersion
newDescribeSolutionVersion pSolutionVersionArn_ =
  DescribeSolutionVersion'
    { solutionVersionArn =
        pSolutionVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the solution version.
describeSolutionVersion_solutionVersionArn :: Lens.Lens' DescribeSolutionVersion Prelude.Text
describeSolutionVersion_solutionVersionArn = Lens.lens (\DescribeSolutionVersion' {solutionVersionArn} -> solutionVersionArn) (\s@DescribeSolutionVersion' {} a -> s {solutionVersionArn = a} :: DescribeSolutionVersion)

instance Core.AWSRequest DescribeSolutionVersion where
  type
    AWSResponse DescribeSolutionVersion =
      DescribeSolutionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSolutionVersionResponse'
            Prelude.<$> (x Data..?> "solutionVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSolutionVersion where
  hashWithSalt _salt DescribeSolutionVersion' {..} =
    _salt `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData DescribeSolutionVersion where
  rnf DescribeSolutionVersion' {..} =
    Prelude.rnf solutionVersionArn

instance Data.ToHeaders DescribeSolutionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeSolutionVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSolutionVersion where
  toJSON DescribeSolutionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("solutionVersionArn" Data..= solutionVersionArn)
          ]
      )

instance Data.ToPath DescribeSolutionVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSolutionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSolutionVersionResponse' smart constructor.
data DescribeSolutionVersionResponse = DescribeSolutionVersionResponse'
  { -- | The solution version.
    solutionVersion :: Prelude.Maybe SolutionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSolutionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionVersion', 'describeSolutionVersionResponse_solutionVersion' - The solution version.
--
-- 'httpStatus', 'describeSolutionVersionResponse_httpStatus' - The response's http status code.
newDescribeSolutionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSolutionVersionResponse
newDescribeSolutionVersionResponse pHttpStatus_ =
  DescribeSolutionVersionResponse'
    { solutionVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The solution version.
describeSolutionVersionResponse_solutionVersion :: Lens.Lens' DescribeSolutionVersionResponse (Prelude.Maybe SolutionVersion)
describeSolutionVersionResponse_solutionVersion = Lens.lens (\DescribeSolutionVersionResponse' {solutionVersion} -> solutionVersion) (\s@DescribeSolutionVersionResponse' {} a -> s {solutionVersion = a} :: DescribeSolutionVersionResponse)

-- | The response's http status code.
describeSolutionVersionResponse_httpStatus :: Lens.Lens' DescribeSolutionVersionResponse Prelude.Int
describeSolutionVersionResponse_httpStatus = Lens.lens (\DescribeSolutionVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeSolutionVersionResponse' {} a -> s {httpStatus = a} :: DescribeSolutionVersionResponse)

instance
  Prelude.NFData
    DescribeSolutionVersionResponse
  where
  rnf DescribeSolutionVersionResponse' {..} =
    Prelude.rnf solutionVersion
      `Prelude.seq` Prelude.rnf httpStatus
