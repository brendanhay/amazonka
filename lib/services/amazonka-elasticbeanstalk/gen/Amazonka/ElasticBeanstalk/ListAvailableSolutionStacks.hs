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
-- Module      : Amazonka.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names, with the public
-- version first and then in reverse chronological order.
module Amazonka.ElasticBeanstalk.ListAvailableSolutionStacks
  ( -- * Creating a Request
    ListAvailableSolutionStacks (..),
    newListAvailableSolutionStacks,

    -- * Destructuring the Response
    ListAvailableSolutionStacksResponse (..),
    newListAvailableSolutionStacksResponse,

    -- * Response Lenses
    listAvailableSolutionStacksResponse_solutionStackDetails,
    listAvailableSolutionStacksResponse_solutionStacks,
    listAvailableSolutionStacksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks = ListAvailableSolutionStacks'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableSolutionStacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListAvailableSolutionStacks ::
  ListAvailableSolutionStacks
newListAvailableSolutionStacks =
  ListAvailableSolutionStacks'

instance Core.AWSRequest ListAvailableSolutionStacks where
  type
    AWSResponse ListAvailableSolutionStacks =
      ListAvailableSolutionStacksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListAvailableSolutionStacksResult"
      ( \s h x ->
          ListAvailableSolutionStacksResponse'
            Prelude.<$> ( x
                            Data..@? "SolutionStackDetails"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "SolutionStacks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAvailableSolutionStacks where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListAvailableSolutionStacks where
  rnf _ = ()

instance Data.ToHeaders ListAvailableSolutionStacks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAvailableSolutionStacks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAvailableSolutionStacks where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "ListAvailableSolutionStacks" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'newListAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { -- | A list of available solution stacks and their SolutionStackDescription.
    solutionStackDetails :: Prelude.Maybe [SolutionStackDescription],
    -- | A list of available solution stacks.
    solutionStacks :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableSolutionStacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionStackDetails', 'listAvailableSolutionStacksResponse_solutionStackDetails' - A list of available solution stacks and their SolutionStackDescription.
--
-- 'solutionStacks', 'listAvailableSolutionStacksResponse_solutionStacks' - A list of available solution stacks.
--
-- 'httpStatus', 'listAvailableSolutionStacksResponse_httpStatus' - The response's http status code.
newListAvailableSolutionStacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableSolutionStacksResponse
newListAvailableSolutionStacksResponse pHttpStatus_ =
  ListAvailableSolutionStacksResponse'
    { solutionStackDetails =
        Prelude.Nothing,
      solutionStacks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of available solution stacks and their SolutionStackDescription.
listAvailableSolutionStacksResponse_solutionStackDetails :: Lens.Lens' ListAvailableSolutionStacksResponse (Prelude.Maybe [SolutionStackDescription])
listAvailableSolutionStacksResponse_solutionStackDetails = Lens.lens (\ListAvailableSolutionStacksResponse' {solutionStackDetails} -> solutionStackDetails) (\s@ListAvailableSolutionStacksResponse' {} a -> s {solutionStackDetails = a} :: ListAvailableSolutionStacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of available solution stacks.
listAvailableSolutionStacksResponse_solutionStacks :: Lens.Lens' ListAvailableSolutionStacksResponse (Prelude.Maybe [Prelude.Text])
listAvailableSolutionStacksResponse_solutionStacks = Lens.lens (\ListAvailableSolutionStacksResponse' {solutionStacks} -> solutionStacks) (\s@ListAvailableSolutionStacksResponse' {} a -> s {solutionStacks = a} :: ListAvailableSolutionStacksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAvailableSolutionStacksResponse_httpStatus :: Lens.Lens' ListAvailableSolutionStacksResponse Prelude.Int
listAvailableSolutionStacksResponse_httpStatus = Lens.lens (\ListAvailableSolutionStacksResponse' {httpStatus} -> httpStatus) (\s@ListAvailableSolutionStacksResponse' {} a -> s {httpStatus = a} :: ListAvailableSolutionStacksResponse)

instance
  Prelude.NFData
    ListAvailableSolutionStacksResponse
  where
  rnf ListAvailableSolutionStacksResponse' {..} =
    Prelude.rnf solutionStackDetails `Prelude.seq`
      Prelude.rnf solutionStacks `Prelude.seq`
        Prelude.rnf httpStatus
