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
-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names, with the public
-- version first and then in reverse chronological order.
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
  ( -- * Creating a Request
    ListAvailableSolutionStacks (..),
    newListAvailableSolutionStacks,

    -- * Destructuring the Response
    ListAvailableSolutionStacksResponse (..),
    newListAvailableSolutionStacksResponse,

    -- * Response Lenses
    listAvailableSolutionStacksResponse_solutionStacks,
    listAvailableSolutionStacksResponse_solutionStackDetails,
    listAvailableSolutionStacksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks = ListAvailableSolutionStacks'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAvailableSolutionStacksResult"
      ( \s h x ->
          ListAvailableSolutionStacksResponse'
            Core.<$> ( x Core..@? "SolutionStacks" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "SolutionStackDetails"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAvailableSolutionStacks

instance Core.NFData ListAvailableSolutionStacks

instance Core.ToHeaders ListAvailableSolutionStacks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListAvailableSolutionStacks where
  toPath = Core.const "/"

instance Core.ToQuery ListAvailableSolutionStacks where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("ListAvailableSolutionStacks" :: Core.ByteString),
            "Version" Core.=: ("2010-12-01" :: Core.ByteString)
          ]
      )

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'newListAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { -- | A list of available solution stacks.
    solutionStacks :: Core.Maybe [Core.Text],
    -- | A list of available solution stacks and their SolutionStackDescription.
    solutionStackDetails :: Core.Maybe [SolutionStackDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAvailableSolutionStacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionStacks', 'listAvailableSolutionStacksResponse_solutionStacks' - A list of available solution stacks.
--
-- 'solutionStackDetails', 'listAvailableSolutionStacksResponse_solutionStackDetails' - A list of available solution stacks and their SolutionStackDescription.
--
-- 'httpStatus', 'listAvailableSolutionStacksResponse_httpStatus' - The response's http status code.
newListAvailableSolutionStacksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAvailableSolutionStacksResponse
newListAvailableSolutionStacksResponse pHttpStatus_ =
  ListAvailableSolutionStacksResponse'
    { solutionStacks =
        Core.Nothing,
      solutionStackDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of available solution stacks.
listAvailableSolutionStacksResponse_solutionStacks :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [Core.Text])
listAvailableSolutionStacksResponse_solutionStacks = Lens.lens (\ListAvailableSolutionStacksResponse' {solutionStacks} -> solutionStacks) (\s@ListAvailableSolutionStacksResponse' {} a -> s {solutionStacks = a} :: ListAvailableSolutionStacksResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of available solution stacks and their SolutionStackDescription.
listAvailableSolutionStacksResponse_solutionStackDetails :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [SolutionStackDescription])
listAvailableSolutionStacksResponse_solutionStackDetails = Lens.lens (\ListAvailableSolutionStacksResponse' {solutionStackDetails} -> solutionStackDetails) (\s@ListAvailableSolutionStacksResponse' {} a -> s {solutionStackDetails = a} :: ListAvailableSolutionStacksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAvailableSolutionStacksResponse_httpStatus :: Lens.Lens' ListAvailableSolutionStacksResponse Core.Int
listAvailableSolutionStacksResponse_httpStatus = Lens.lens (\ListAvailableSolutionStacksResponse' {httpStatus} -> httpStatus) (\s@ListAvailableSolutionStacksResponse' {} a -> s {httpStatus = a} :: ListAvailableSolutionStacksResponse)

instance
  Core.NFData
    ListAvailableSolutionStacksResponse
