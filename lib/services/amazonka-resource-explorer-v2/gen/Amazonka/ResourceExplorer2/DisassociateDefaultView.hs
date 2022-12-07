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
-- Module      : Amazonka.ResourceExplorer2.DisassociateDefaultView
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- After you call this operation, the affected Amazon Web Services Region
-- no longer has a default view. All Search operations in that Region must
-- explicitly specify a view or the operation fails. You can configure a
-- new default by calling the AssociateDefaultView operation.
--
-- If an Amazon Web Services Region doesn\'t have a default view
-- configured, then users must explicitly specify a view with every
-- @Search@ operation performed in that Region.
module Amazonka.ResourceExplorer2.DisassociateDefaultView
  ( -- * Creating a Request
    DisassociateDefaultView (..),
    newDisassociateDefaultView,

    -- * Destructuring the Response
    DisassociateDefaultViewResponse (..),
    newDisassociateDefaultViewResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateDefaultView' smart constructor.
data DisassociateDefaultView = DisassociateDefaultView'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDefaultView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateDefaultView ::
  DisassociateDefaultView
newDisassociateDefaultView = DisassociateDefaultView'

instance Core.AWSRequest DisassociateDefaultView where
  type
    AWSResponse DisassociateDefaultView =
      DisassociateDefaultViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateDefaultViewResponse'

instance Prelude.Hashable DisassociateDefaultView where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisassociateDefaultView where
  rnf _ = ()

instance Data.ToHeaders DisassociateDefaultView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDefaultView where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociateDefaultView where
  toPath = Prelude.const "/DisassociateDefaultView"

instance Data.ToQuery DisassociateDefaultView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDefaultViewResponse' smart constructor.
data DisassociateDefaultViewResponse = DisassociateDefaultViewResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDefaultViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateDefaultViewResponse ::
  DisassociateDefaultViewResponse
newDisassociateDefaultViewResponse =
  DisassociateDefaultViewResponse'

instance
  Prelude.NFData
    DisassociateDefaultViewResponse
  where
  rnf _ = ()
