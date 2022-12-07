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
-- Module      : Amazonka.BackupGateway.ListVirtualMachines
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your virtual machines.
--
-- This operation returns paginated results.
module Amazonka.BackupGateway.ListVirtualMachines
  ( -- * Creating a Request
    ListVirtualMachines (..),
    newListVirtualMachines,

    -- * Request Lenses
    listVirtualMachines_nextToken,
    listVirtualMachines_maxResults,
    listVirtualMachines_hypervisorArn,

    -- * Destructuring the Response
    ListVirtualMachinesResponse (..),
    newListVirtualMachinesResponse,

    -- * Response Lenses
    listVirtualMachinesResponse_nextToken,
    listVirtualMachinesResponse_virtualMachines,
    listVirtualMachinesResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVirtualMachines' smart constructor.
data ListVirtualMachines = ListVirtualMachines'
  { -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of virtual machines to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the hypervisor connected to your
    -- virtual machine.
    hypervisorArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualMachines' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualMachines_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'maxResults', 'listVirtualMachines_maxResults' - The maximum number of virtual machines to list.
--
-- 'hypervisorArn', 'listVirtualMachines_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor connected to your
-- virtual machine.
newListVirtualMachines ::
  ListVirtualMachines
newListVirtualMachines =
  ListVirtualMachines'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      hypervisorArn = Prelude.Nothing
    }

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listVirtualMachines_nextToken :: Lens.Lens' ListVirtualMachines (Prelude.Maybe Prelude.Text)
listVirtualMachines_nextToken = Lens.lens (\ListVirtualMachines' {nextToken} -> nextToken) (\s@ListVirtualMachines' {} a -> s {nextToken = a} :: ListVirtualMachines)

-- | The maximum number of virtual machines to list.
listVirtualMachines_maxResults :: Lens.Lens' ListVirtualMachines (Prelude.Maybe Prelude.Natural)
listVirtualMachines_maxResults = Lens.lens (\ListVirtualMachines' {maxResults} -> maxResults) (\s@ListVirtualMachines' {} a -> s {maxResults = a} :: ListVirtualMachines)

-- | The Amazon Resource Name (ARN) of the hypervisor connected to your
-- virtual machine.
listVirtualMachines_hypervisorArn :: Lens.Lens' ListVirtualMachines (Prelude.Maybe Prelude.Text)
listVirtualMachines_hypervisorArn = Lens.lens (\ListVirtualMachines' {hypervisorArn} -> hypervisorArn) (\s@ListVirtualMachines' {} a -> s {hypervisorArn = a} :: ListVirtualMachines)

instance Core.AWSPager ListVirtualMachines where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualMachinesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVirtualMachinesResponse_virtualMachines
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVirtualMachines_nextToken
          Lens..~ rs
          Lens.^? listVirtualMachinesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualMachines where
  type
    AWSResponse ListVirtualMachines =
      ListVirtualMachinesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualMachinesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "VirtualMachines"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVirtualMachines where
  hashWithSalt _salt ListVirtualMachines' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` hypervisorArn

instance Prelude.NFData ListVirtualMachines where
  rnf ListVirtualMachines' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf hypervisorArn

instance Data.ToHeaders ListVirtualMachines where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.ListVirtualMachines" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListVirtualMachines where
  toJSON ListVirtualMachines' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("HypervisorArn" Data..=) Prelude.<$> hypervisorArn
          ]
      )

instance Data.ToPath ListVirtualMachines where
  toPath = Prelude.const "/"

instance Data.ToQuery ListVirtualMachines where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVirtualMachinesResponse' smart constructor.
data ListVirtualMachinesResponse = ListVirtualMachinesResponse'
  { -- | The next item following a partial list of returned resources. For
    -- example, if a request is made to return @maxResults@ number of
    -- resources, @NextToken@ allows you to return more items in your list
    -- starting at the location pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of your @VirtualMachine@ objects, ordered by their Amazon
    -- Resource Names (ARNs).
    virtualMachines :: Prelude.Maybe [VirtualMachine],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualMachinesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualMachinesResponse_nextToken' - The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
--
-- 'virtualMachines', 'listVirtualMachinesResponse_virtualMachines' - A list of your @VirtualMachine@ objects, ordered by their Amazon
-- Resource Names (ARNs).
--
-- 'httpStatus', 'listVirtualMachinesResponse_httpStatus' - The response's http status code.
newListVirtualMachinesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualMachinesResponse
newListVirtualMachinesResponse pHttpStatus_ =
  ListVirtualMachinesResponse'
    { nextToken =
        Prelude.Nothing,
      virtualMachines = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned resources. For
-- example, if a request is made to return @maxResults@ number of
-- resources, @NextToken@ allows you to return more items in your list
-- starting at the location pointed to by the next token.
listVirtualMachinesResponse_nextToken :: Lens.Lens' ListVirtualMachinesResponse (Prelude.Maybe Prelude.Text)
listVirtualMachinesResponse_nextToken = Lens.lens (\ListVirtualMachinesResponse' {nextToken} -> nextToken) (\s@ListVirtualMachinesResponse' {} a -> s {nextToken = a} :: ListVirtualMachinesResponse)

-- | A list of your @VirtualMachine@ objects, ordered by their Amazon
-- Resource Names (ARNs).
listVirtualMachinesResponse_virtualMachines :: Lens.Lens' ListVirtualMachinesResponse (Prelude.Maybe [VirtualMachine])
listVirtualMachinesResponse_virtualMachines = Lens.lens (\ListVirtualMachinesResponse' {virtualMachines} -> virtualMachines) (\s@ListVirtualMachinesResponse' {} a -> s {virtualMachines = a} :: ListVirtualMachinesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVirtualMachinesResponse_httpStatus :: Lens.Lens' ListVirtualMachinesResponse Prelude.Int
listVirtualMachinesResponse_httpStatus = Lens.lens (\ListVirtualMachinesResponse' {httpStatus} -> httpStatus) (\s@ListVirtualMachinesResponse' {} a -> s {httpStatus = a} :: ListVirtualMachinesResponse)

instance Prelude.NFData ListVirtualMachinesResponse where
  rnf ListVirtualMachinesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf virtualMachines
      `Prelude.seq` Prelude.rnf httpStatus
